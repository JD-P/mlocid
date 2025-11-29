open OUnit2
open Lwt.Syntax
open Dream
open Caqti_lwt
open Database
open Auth
open Api

let test_db_path = "/tmp/mlocid_api_test.db"

let setup_test_db () =
  let _ = try Unix.unlink test_db_path with _ -> () in
  let uri = Printf.sprintf "sqlite3:%s" test_db_path in
  let* connection = Caqti_lwt.connect (Uri.of_string uri) in
  match connection with
  | Ok (module Db : CONNECTION) ->
    let* () = init_db (module Db) in
    Lwt.return (Ok (module Db : CONNECTION))
  | Error e -> Lwt.return (Error (Caqti_error.show e))

let create_test_request ?(user_id=None) ~meth ~path () =
  let request = Dream.request ~method:meth path in
  match user_id with
  | Some uid -> 
    let _ = Dream.set_session request "user_id" (Int64.to_string uid) in
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
        let request = Dream.request ~method:`GET path in
        let _ = Dream.set_session request "user_id" (Int64.to_string user2_id) in
        let* response = get_flashcard_handler db user2_id request in
        let status = Dream.status response in
        assert_equal `NotFound status; (* Should return 404, not the card *)
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
        let request = Dream.request 
          ~method:`PUT 
          ~body:(`String "{\"question\":\"Hacked!\",\"answer\":\"Hacked!\"}")
          path in
        let _ = Dream.set_session request "user_id" (Int64.to_string user2_id) in
        (* Set the path parameter - Dream.param extracts from the path *)
        let* response = update_flashcard_handler db user2_id request in
        let status = Dream.status response in
        assert_equal `NotFound status; (* Should return 404 *)
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
        let request = Dream.request ~method:`DELETE path in
        let _ = Dream.set_session request "user_id" (Int64.to_string user2_id) in
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
  ]

let () =
  Lwt_main.run (OUnit2.run_test_tt_main suite)
