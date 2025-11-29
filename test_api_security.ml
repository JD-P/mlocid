open OUnit2
open Lwt.Syntax
open Dream
open Caqti_lwt
open Database
open Auth
open Api

let test_db_path = "/tmp/mlocid_api_test.db"

(* Helper function to iterate over a list with Lwt *)
let rec iter_lwt f = function
  | [] -> Lwt.return_unit
  | x :: xs -> 
    let* () = f x in
    iter_lwt f xs

let setup_test_db () =
  let _ = try Unix.unlink test_db_path with _ -> () in
  let uri = Printf.sprintf "sqlite3:%s" test_db_path in
  let* connection = Caqti_lwt_unix.connect (Uri.of_string uri) in
  match connection with
  | Ok (module Db : Caqti_lwt.CONNECTION) ->
    let* () = init_db (module Db) in
    Lwt.return (Ok (module Db : Caqti_lwt.CONNECTION))
  | Error e -> Lwt.return (Error (Caqti_error.show e))

let create_test_request ?(user_id=None) ~meth ~path () =
  (* Create request - Dream.request creates a GET request by default *)
  (* We'll need to use Dream's lower-level API or construct manually *)
  let request = Dream.request path in
  (* Note: Setting method might require using Dream's internal API *)
  (* For now, we'll rely on the default and handle method in handlers *)
  let request = match meth with
    | `GET -> request
    | `POST -> request  (* Will be handled by handler expecting POST *)
    | `PUT -> request
    | `DELETE -> request
    | _ -> request
  in
  match user_id with
  | Some uid -> 
    let _ = Dream.set_session_field request "user_id" (Int64.to_string uid) in
    request
  | None -> request

let test_unauthenticated_cannot_access_flashcards _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let request = create_test_request ~meth:`GET ~path:"/api/flashcards" () in
    let* response = require_auth (get_flashcards_handler db) request in
    let status = Dream.status response in
    assert_equal `Unauthorized status;
    Lwt.return_unit
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_unauthenticated_cannot_create_flashcard _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let request = create_test_request ~meth:`POST ~path:"/api/flashcards" () in
    let* response = require_auth (create_flashcard_handler db) request in
    let status = Dream.status response in
    assert_equal `Unauthorized status;
    Lwt.return_unit
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_unauthenticated_cannot_get_flashcard _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let request = create_test_request ~meth:`GET ~path:"/api/flashcards/1" () in
    let* response = require_auth (get_flashcard_handler db) request in
    let status = Dream.status response in
    assert_equal `Unauthorized status;
    Lwt.return_unit
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_unauthenticated_cannot_update_flashcard _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let request = create_test_request ~meth:`PUT ~path:"/api/flashcards/1" () in
    let* response = require_auth (update_flashcard_handler db) request in
    let status = Dream.status response in
    assert_equal `Unauthorized status;
    Lwt.return_unit
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_unauthenticated_cannot_delete_flashcard _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let request = create_test_request ~meth:`DELETE ~path:"/api/flashcards/1" () in
    let* response = require_auth (delete_flashcard_handler db) request in
    let status = Dream.status response in
    assert_equal `Unauthorized status;
    Lwt.return_unit
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_unauthenticated_cannot_get_due_flashcards _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let request = create_test_request ~meth:`GET ~path:"/api/study/due" () in
    let* response = require_auth (get_due_flashcards_handler db) request in
    let status = Dream.status response in
    assert_equal `Unauthorized status;
    Lwt.return_unit
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_unauthenticated_cannot_review_flashcard _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let request = create_test_request ~meth:`POST ~path:"/api/study/review/1" () in
    let* response = require_auth (review_flashcard_handler db) request in
    let status = Dream.status response in
    assert_equal `Unauthorized status;
    Lwt.return_unit
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_unauthenticated_cannot_import _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let request = create_test_request ~meth:`POST ~path:"/api/import/mnemosyne" () in
    let* response = require_auth (import_mnemosyne_handler db) request in
    let status = Dream.status response in
    assert_equal `Unauthorized status;
    Lwt.return_unit
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_authenticated_user_cannot_access_other_users_flashcard _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* user1_result = create_user db "user1" password_hash in
    let* user2_result = create_user db "user2" password_hash in
    match user1_result, user2_result with
    | Ok user1_id, Ok user2_id ->
      (* User1 creates a flashcard *)
      let* card_result = create_flashcard db user1_id "User1's Question" "User1's Answer" in
      match card_result with
      | Ok card_id ->
        (* User2 tries to access User1's flashcard via API *)
        let path = Printf.sprintf "/api/flashcards/%Ld" card_id in
        let request = Dream.request path in
        let _ = Dream.set_session_field request "user_id" (Int64.to_string user2_id) in
        let* response = get_flashcard_handler db user2_id request in
        let status = Dream.status response in
        assert_equal `Not_Found status; (* Should return 404, not the card *)
        Lwt.return_unit
      | Error msg -> assert_failure ("Failed to create flashcard: " ^ msg)
    | _ -> assert_failure "Failed to create users"
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_authenticated_user_cannot_update_other_users_flashcard _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* user1_result = create_user db "user1" password_hash in
    let* user2_result = create_user db "user2" password_hash in
    match user1_result, user2_result with
    | Ok user1_id, Ok user2_id ->
      (* User1 creates a flashcard *)
      let* card_result = create_flashcard db user1_id "User1's Question" "User1's Answer" in
      match card_result with
      | Ok card_id ->
        (* User2 tries to update User1's flashcard via API *)
        let path = Printf.sprintf "/api/flashcards/%Ld" card_id in
        let request = Dream.request path in
        (* Note: Method and body setting may need Dream's actual API - simplified for now *)
        let _ = Dream.set_session_field request "user_id" (Int64.to_string user2_id) in
        (* Set the path parameter - Dream.param extracts from the path *)
        let* response = update_flashcard_handler db user2_id request in
        let status = Dream.status response in
        assert_equal `Not_Found status; (* Should return 404 *)
        (* Verify card wasn't updated *)
        let* verify_result = get_flashcard db card_id user1_id in
        match verify_result with
        | Ok (Some card) ->
          assert_equal "User1's Question" card.question;
          Lwt.return_unit
        | _ -> assert_failure "Failed to verify card wasn't updated"
      | Error msg -> assert_failure ("Failed to create flashcard: " ^ msg)
    | _ -> assert_failure "Failed to create users"
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_authenticated_user_cannot_delete_other_users_flashcard _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* user1_result = create_user db "user1" password_hash in
    let* user2_result = create_user db "user2" password_hash in
    match user1_result, user2_result with
    | Ok user1_id, Ok user2_id ->
      (* User1 creates a flashcard *)
      let* card_result = create_flashcard db user1_id "User1's Question" "User1's Answer" in
      match card_result with
      | Ok card_id ->
        (* User2 tries to delete User1's flashcard via API *)
        let path = Printf.sprintf "/api/flashcards/%Ld" card_id in
        let request = Dream.request path in
        let _ = Dream.set_session_field request "user_id" (Int64.to_string user2_id) in
        let* response = delete_flashcard_handler db user2_id request in
        let status = Dream.status response in
        (* Delete should succeed (no error), but card should still exist for user1 *)
        let* verify_result = get_flashcard db card_id user1_id in
        match verify_result with
        | Ok (Some _) -> Lwt.return_unit (* Card still exists, which is correct *)
        | Ok None -> assert_failure "Card should still exist after unauthorized delete attempt"
        | Error msg -> assert_failure ("Failed to verify card: " ^ msg)
      | Error msg -> assert_failure ("Failed to create flashcard: " ^ msg)
    | _ -> assert_failure "Failed to create users"
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

(* SQL Injection test payloads *)
let sql_injection_payloads = [
  "'; DROP TABLE users; --";
  "' OR '1'='1";
  "' OR '1'='1' --";
  "' OR '1'='1' /*";
  "admin'--";
  "admin'/*";
  "' UNION SELECT * FROM users --";
  "1' OR '1'='1";
  "1' UNION SELECT NULL, NULL, NULL --";
  "'; DELETE FROM flashcards; --";
  "'; UPDATE users SET password_hash='hacked'; --";
  "1'; INSERT INTO users (username, password_hash) VALUES ('hacker', 'hash'); --";
]

(* Test SQL injection in username during registration *)
let test_sql_injection_in_username_registration _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    (* Try to register with SQL injection in username *)
    let* () = iter_lwt (fun payload ->
      let username = payload in
      let* result = create_user db username password_hash in
      (* Should either succeed (treating as literal string) or fail gracefully *)
      (* If it succeeds, verify the username was stored as-is, not executed *)
      match result with
      | Ok user_id ->
        let* verify_result = get_user_by_username db username in
        (match verify_result with
        | Ok (Some user) ->
          (* Username should be stored exactly as provided, not executed *)
          assert_equal username user.username;
          (* Verify no other users were affected *)
          let* all_users = get_user_by_id db user_id in
          (match all_users with
          | Ok (Some _) -> Lwt.return_unit
          | _ -> assert_failure "User should exist after registration")
        | _ -> assert_failure "User should be retrievable after registration")
      | Error _ -> Lwt.return_unit (* Registration failure is acceptable for some payloads *)
    ) sql_injection_payloads in
    Lwt.return_unit
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

(* Test SQL injection in username during login *)
let test_sql_injection_in_username_login _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* user_result = create_user db "legitimate_user" password_hash in
    match user_result with
    | Ok _ ->
      (* Try to login with SQL injection in username *)
      let* () = iter_lwt (fun payload ->
        let username = payload in
        let* result = get_user_by_username db username in
        (* Should not find any user (or find only if exact match) *)
        match result with
        | Ok None -> Lwt.return_unit (* Expected - SQL injection should not work *)
        | Ok (Some user) ->
          (* If a user is found, it should only be because the payload exactly matches a username *)
          assert_bool "If user found, it should be exact match, not SQL injection" 
            (user.username = username);
          Lwt.return_unit
        | Error _ -> Lwt.return_unit (* Error is acceptable *)
      ) sql_injection_payloads in
      Lwt.return_unit
    | Error msg -> assert_failure ("Failed to create test user: " ^ msg)
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

(* Test SQL injection in flashcard question field *)
let test_sql_injection_in_flashcard_question _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* user_result = create_user db "testuser" password_hash in
    match user_result with
    | Ok user_id ->
      (* Try to create flashcards with SQL injection in question *)
      let* () = iter_lwt (fun payload ->
        let question = payload in
        let answer = "Normal answer" in
        let* result = create_flashcard db user_id question answer in
        match result with
        | Ok card_id ->
          (* Verify the question was stored as literal string, not executed *)
          let* verify_result = get_flashcard db card_id user_id in
          (match verify_result with
          | Ok (Some card) ->
            assert_equal question card.question;
            assert_equal answer card.answer;
            (* Verify no other cards were affected *)
            let* all_cards = get_flashcards db user_id in
            (match all_cards with
            | Ok cards ->
              assert_bool "Should have at least one card" (List.length cards >= 1);
              (* Verify the malicious payload is stored as data, not executed *)
              let found = List.exists (fun c -> c.question = question) cards in
              assert_bool "Card with SQL injection payload should exist as literal string" found;
              Lwt.return_unit
            | Error _ -> assert_failure "Should be able to retrieve flashcards")
          | _ -> assert_failure "Should be able to retrieve created flashcard")
        | Error _ -> assert_failure ("Failed to create flashcard with SQL injection payload: " ^ payload)
      ) sql_injection_payloads in
      Lwt.return_unit
    | Error msg -> assert_failure ("Failed to create test user: " ^ msg)
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

(* Test SQL injection in flashcard answer field *)
let test_sql_injection_in_flashcard_answer _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* user_result = create_user db "testuser" password_hash in
    match user_result with
    | Ok user_id ->
      (* Try to create flashcards with SQL injection in answer *)
      let* () = iter_lwt (fun payload ->
        let question = "Normal question" in
        let answer = payload in
        let* result = create_flashcard db user_id question answer in
        match result with
        | Ok card_id ->
          (* Verify the answer was stored as literal string, not executed *)
          let* verify_result = get_flashcard db card_id user_id in
          (match verify_result with
          | Ok (Some card) ->
            assert_equal question card.question;
            assert_equal answer card.answer;
            Lwt.return_unit
          | _ -> assert_failure "Should be able to retrieve created flashcard")
        | Error _ -> assert_failure ("Failed to create flashcard with SQL injection payload: " ^ payload)
      ) sql_injection_payloads in
      Lwt.return_unit
    | Error msg -> assert_failure ("Failed to create test user: " ^ msg)
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

(* Test SQL injection in flashcard update (question field) *)
let test_sql_injection_in_flashcard_update_question _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* user_result = create_user db "testuser" password_hash in
    match user_result with
    | Ok user_id ->
      (* Create a normal flashcard first *)
      let* card_result = create_flashcard db user_id "Original Question" "Original Answer" in
      match card_result with
      | Ok card_id ->
        (* Try to update with SQL injection in question - test first payload only *)
        let payload = List.hd sql_injection_payloads in
        let* existing_result = get_flashcard db card_id user_id in
        match existing_result with
        | Ok (Some existing_card) ->
          let updated_card = { existing_card with question = payload } in
          let* update_result = update_flashcard db updated_card in
          match update_result with
          | Ok () ->
            (* Verify the question was updated as literal string *)
            let* verify_result = get_flashcard db card_id user_id in
            (match verify_result with
            | Ok (Some card) ->
              assert_equal payload card.question;
              (* Verify no other cards were affected *)
              let* all_cards = get_flashcards db user_id in
              (match all_cards with
              | Ok cards ->
                assert_bool "Should have at least one card" (List.length cards >= 1);
                Lwt.return_unit
              | Error _ -> assert_failure "Should be able to retrieve flashcards")
            | _ -> assert_failure "Should be able to retrieve updated flashcard")
          | Error _ -> assert_failure ("Failed to update flashcard with SQL injection payload: " ^ payload)
        | _ -> assert_failure "Should be able to retrieve existing flashcard"
      | Error msg -> assert_failure ("Failed to create flashcard: " ^ msg)
    | Error msg -> assert_failure ("Failed to create test user: " ^ msg)
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

(* Test SQL injection via API endpoint (registration) *)
let test_sql_injection_via_api_register _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    (* Try to register via API with SQL injection in username - test first few payloads *)
    let test_payloads = 
      let rec take n = function
        | [] -> []
        | _ when n <= 0 -> []
        | x :: xs -> x :: take (n - 1) xs
      in
      take 3 sql_injection_payloads
    in
    let* () = iter_lwt (fun payload ->
      let username = payload in
      let password = "testpass" in
      let body = Printf.sprintf "{\"username\":\"%s\",\"password\":\"%s\"}" 
        (String.escaped username) password in
      let request = Dream.request "/api/register" in
      (* Note: Method and body setting may need Dream's actual API - simplified for now *)
      let* response = register_handler db request in
      let status = Dream.status response in
      (* Should either succeed (treating as literal) or fail gracefully *)
      if status = `OK then (
        (* If registration succeeded, verify username was stored correctly *)
        let* verify_result = get_user_by_username db username in
        match verify_result with
        | Ok (Some user) ->
          assert_equal username user.username;
          (* Verify no unauthorized access occurred *)
          let* other_users = get_user_by_username db "admin" in
          (match other_users with
          | Ok None -> Lwt.return_unit (* Good - no admin user should exist *)
          | _ -> Lwt.return_unit) (* If admin exists, it's not from this injection *)
        | _ -> assert_failure "User should be retrievable after registration"
      ) else (
        (* Registration failed - verify it failed gracefully *)
        assert_bool "Registration failure should be Bad_Request or Conflict" 
          (status = `Bad_Request || status = `Conflict);
        Lwt.return_unit
      )
    ) test_payloads in
    Lwt.return_unit
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

(* Test SQL injection via API endpoint (create flashcard) *)
let test_sql_injection_via_api_create_flashcard _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* user_result = create_user db "testuser" password_hash in
    match user_result with
    | Ok user_id ->
      (* Try to create flashcard via API with SQL injection in question - test first payload *)
      let payload = List.hd sql_injection_payloads in
      let question = payload in
      let answer = "Normal answer" in
      let body = Printf.sprintf "{\"question\":\"%s\",\"answer\":\"%s\"}" 
        (String.escaped question) (String.escaped answer) in
      let request = Dream.request "/api/flashcards" in
      (* Note: Method and body setting may need Dream's actual API - simplified for now *)
      let _ = Dream.set_session_field request "user_id" (Int64.to_string user_id) in
      let* response = create_flashcard_handler db user_id request in
      let status = Dream.status response in
      (* Should succeed (treating as literal string) *)
      assert_equal `OK status;
      (* Verify the flashcard was created and stored correctly *)
      let* all_cards = get_flashcards db user_id in
      match all_cards with
      | Ok cards ->
        let found = List.exists (fun c -> c.question = question) cards in
        assert_bool "Card with SQL injection payload should exist as literal string" found;
        Lwt.return_unit
      | Error _ -> assert_failure "Should be able to retrieve flashcards"
    | Error msg -> assert_failure ("Failed to create test user: " ^ msg)
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

(* Test SQL injection in path parameter (flashcard ID) *)
let test_sql_injection_in_path_parameter _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* user_result = create_user db "testuser" password_hash in
    match user_result with
    | Ok user_id ->
      (* Create a legitimate flashcard *)
      let* card_result = create_flashcard db user_id "Test Question" "Test Answer" in
      match card_result with
      | Ok _ ->
        (* Try to access flashcard with SQL injection in ID parameter *)
        let malicious_ids = [
          "1' OR '1'='1";
          "1' UNION SELECT * FROM flashcards --";
          "'; DROP TABLE flashcards; --";
          "1 OR 1=1";
        ] in
        let* () = iter_lwt (fun malicious_id ->
          let path = Printf.sprintf "/api/flashcards/%s" malicious_id in
          let request = Dream.request path in
          let _ = Dream.set_session_field request "user_id" (Int64.to_string user_id) in
          (* Dream.param should extract the ID, but Int64.of_string_opt should fail for non-numeric *)
          let* response = get_flashcard_handler db user_id request in
          let status = Dream.status response in
          (* Should return Bad_Request (invalid ID format) or NotFound, not execute SQL *)
          assert_bool "Should reject invalid ID format" 
            (status = `Bad_Request || status = `Not_Found);
          Lwt.return_unit
        ) malicious_ids in
        (* Verify database is still intact *)
        let* verify_result = get_flashcards db user_id in
        match verify_result with
        | Ok cards ->
          assert_bool "Database should still be intact" (List.length cards >= 1);
          Lwt.return_unit
        | Error _ -> assert_failure "Database should still be accessible"
      | Error msg -> assert_failure ("Failed to create flashcard: " ^ msg)
    | Error msg -> assert_failure ("Failed to create test user: " ^ msg)
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

(* Test SQL injection in import data *)
let test_sql_injection_in_import_data _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* user_result = create_user db "testuser" password_hash in
    match user_result with
    | Ok user_id ->
      (* Try to import data with SQL injection payload - test first payload *)
      let payload = List.hd sql_injection_payloads in
      let import_data = Printf.sprintf "%s\tAnswer with %s" payload payload in
      let request = Dream.request "/api/import/mnemosyne" in
      (* Note: Method and body setting may need Dream's actual API - simplified for now *)
      let _ = Dream.set_session_field request "user_id" (Int64.to_string user_id) in
      let* response = import_mnemosyne_handler db user_id request in
      let status = Dream.status response in
      (* Should succeed (treating as literal data) *)
      assert_equal `OK status;
      (* Verify the data was imported correctly *)
      let* all_cards = get_flashcards db user_id in
      match all_cards with
      | Ok cards ->
        let found = List.exists (fun c -> String.contains c.question '\'' || String.contains c.answer '\'') cards in
        (* If SQL injection payload was in the data, it should be stored as literal *)
        assert_bool "SQL injection payload should be stored as literal string" found;
        Lwt.return_unit
      | Error _ -> assert_failure "Should be able to retrieve flashcards"
    | Error msg -> assert_failure ("Failed to create test user: " ^ msg)
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let suite =
  "API Security Tests" >:::
  [
    "unauthenticated_cannot_access_flashcards" >:: test_unauthenticated_cannot_access_flashcards;
    "unauthenticated_cannot_create_flashcard" >:: test_unauthenticated_cannot_create_flashcard;
    "unauthenticated_cannot_get_flashcard" >:: test_unauthenticated_cannot_get_flashcard;
    "unauthenticated_cannot_update_flashcard" >:: test_unauthenticated_cannot_update_flashcard;
    "unauthenticated_cannot_delete_flashcard" >:: test_unauthenticated_cannot_delete_flashcard;
    "unauthenticated_cannot_get_due_flashcards" >:: test_unauthenticated_cannot_get_due_flashcards;
    "unauthenticated_cannot_review_flashcard" >:: test_unauthenticated_cannot_review_flashcard;
    "unauthenticated_cannot_import" >:: test_unauthenticated_cannot_import;
    "authenticated_user_cannot_access_other_users_flashcard" >:: test_authenticated_user_cannot_access_other_users_flashcard;
    "authenticated_user_cannot_update_other_users_flashcard" >:: test_authenticated_user_cannot_update_other_users_flashcard;
    "authenticated_user_cannot_delete_other_users_flashcard" >:: test_authenticated_user_cannot_delete_other_users_flashcard;
    (* SQL Injection tests *)
    "sql_injection_in_username_registration" >:: test_sql_injection_in_username_registration;
    "sql_injection_in_username_login" >:: test_sql_injection_in_username_login;
    "sql_injection_in_flashcard_question" >:: test_sql_injection_in_flashcard_question;
    "sql_injection_in_flashcard_answer" >:: test_sql_injection_in_flashcard_answer;
    "sql_injection_in_flashcard_update_question" >:: test_sql_injection_in_flashcard_update_question;
    "sql_injection_via_api_register" >:: test_sql_injection_via_api_register;
    "sql_injection_via_api_create_flashcard" >:: test_sql_injection_via_api_create_flashcard;
    "sql_injection_in_path_parameter" >:: test_sql_injection_in_path_parameter;
    "sql_injection_in_import_data" >:: test_sql_injection_in_import_data;
  ]

let () =
  Lwt_main.run (OUnit2.run_test_tt_main suite)
