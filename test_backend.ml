open OUnit2
open Lwt.Syntax
open Caqti_lwt
open Database
open Sm2
open Auth

let test_db_path = "/tmp/mlocid_test.db"

let setup_test_db () =
  (* Remove old test database if it exists *)
  let _ = try Unix.unlink test_db_path with _ -> () in
  let uri = Printf.sprintf "sqlite3:%s" test_db_path in
  let* connection = Caqti_lwt.connect (Uri.of_string uri) in
  match connection with
  | Ok (module Db : CONNECTION) ->
    let* () = init_db (module Db) in
    Lwt.return (Ok (module Db : CONNECTION))
  | Error e -> Lwt.return (Error (Caqti_error.show e))

let test_create_user _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let* result = create_user db "testuser" (hash_password "testpass") in
    match result with
    | Ok user_id ->
      assert_bool "User ID should be positive" (Int64.compare user_id 0L > 0);
      Lwt.return_unit
    | Error msg -> assert_failure ("Failed to create user: " ^ msg)
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_get_user_by_username _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* _ = create_user db "testuser" password_hash in
    let* result = get_user_by_username db "testuser" in
    match result with
    | Ok (Some user) ->
      assert_equal "testuser" user.username;
      assert_equal password_hash user.password_hash;
      Lwt.return_unit
    | Ok None -> assert_failure "User should exist"
    | Error msg -> assert_failure ("Failed to get user: " ^ msg)
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_create_flashcard _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* user_result = create_user db "testuser" password_hash in
    match user_result with
    | Ok user_id ->
      let* result = create_flashcard db user_id "What is 2+2?" "4" in
      match result with
      | Ok card_id ->
        assert_bool "Card ID should be positive" (Int64.compare card_id 0L > 0);
        Lwt.return_unit
      | Error msg -> assert_failure ("Failed to create flashcard: " ^ msg)
    | Error msg -> assert_failure ("Failed to create user: " ^ msg)
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_get_flashcard _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* user_result = create_user db "testuser" password_hash in
    match user_result with
    | Ok user_id ->
      let* card_result = create_flashcard db user_id "What is 2+2?" "4" in
      match card_result with
      | Ok card_id ->
        let* result = get_flashcard db card_id user_id in
        match result with
        | Ok (Some card) ->
          assert_equal "What is 2+2?" card.question;
          assert_equal "4" card.answer;
          assert_equal 2.5 card.efactor;
          Lwt.return_unit
        | Ok None -> assert_failure "Flashcard should exist"
        | Error msg -> assert_failure ("Failed to get flashcard: " ^ msg)
      | Error msg -> assert_failure ("Failed to create flashcard: " ^ msg)
    | Error msg -> assert_failure ("Failed to create user: " ^ msg)
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_update_flashcard _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* user_result = create_user db "testuser" password_hash in
    match user_result with
    | Ok user_id ->
      let* card_result = create_flashcard db user_id "What is 2+2?" "4" in
      match card_result with
      | Ok card_id ->
        let* get_result = get_flashcard db card_id user_id in
        match get_result with
        | Ok (Some card) ->
          let updated_card = { card with question = "What is 3+3?"; answer = "6" } in
          let* update_result = update_flashcard db updated_card in
          match update_result with
          | Ok () ->
            let* verify_result = get_flashcard db card_id user_id in
            match verify_result with
            | Ok (Some updated) ->
              assert_equal "What is 3+3?" updated.question;
              assert_equal "6" updated.answer;
              Lwt.return_unit
            | _ -> assert_failure "Failed to verify update"
          | Error msg -> assert_failure ("Failed to update flashcard: " ^ msg)
        | _ -> assert_failure "Failed to get flashcard for update"
      | Error msg -> assert_failure ("Failed to create flashcard: " ^ msg)
    | Error msg -> assert_failure ("Failed to create user: " ^ msg)
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_delete_flashcard _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* user_result = create_user db "testuser" password_hash in
    match user_result with
    | Ok user_id ->
      let* card_result = create_flashcard db user_id "What is 2+2?" "4" in
      match card_result with
      | Ok card_id ->
        let* delete_result = delete_flashcard db card_id user_id in
        match delete_result with
        | Ok () ->
          let* verify_result = get_flashcard db card_id user_id in
          match verify_result with
          | Ok None -> Lwt.return_unit
          | Ok (Some _) -> assert_failure "Flashcard should be deleted"
          | Error msg -> assert_failure ("Failed to verify deletion: " ^ msg)
        | Error msg -> assert_failure ("Failed to delete flashcard: " ^ msg)
      | Error msg -> assert_failure ("Failed to create flashcard: " ^ msg)
    | Error msg -> assert_failure ("Failed to create user: " ^ msg)
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_get_flashcards _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* user_result = create_user db "testuser" password_hash in
    match user_result with
    | Ok user_id ->
      let* _ = create_flashcard db user_id "Q1" "A1" in
      let* _ = create_flashcard db user_id "Q2" "A2" in
      let* _ = create_flashcard db user_id "Q3" "A3" in
      let* result = get_flashcards db user_id in
      match result with
      | Ok cards ->
        assert_equal 3 (List.length cards);
        Lwt.return_unit
      | Error msg -> assert_failure ("Failed to get flashcards: " ^ msg)
    | Error msg -> assert_failure ("Failed to create user: " ^ msg)
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_sm2_algorithm _ =
  let state = initial_state in
  assert_equal 2.5 state.efactor;
  assert_equal 0 state.interval;
  assert_equal 0 state.repetitions;
  
  (* Test quality 5 (perfect) *)
  let state1 = update_card_state state Q5 in
  assert_bool "E-Factor should increase or stay same" (state1.efactor >= state.efactor);
  assert_equal 1 state1.repetitions;
  assert_equal 1 state1.interval;
  
  (* Test quality 3 (correct but difficult) *)
  let state2 = update_card_state state1 Q3 in
  assert_bool "E-Factor should decrease" (state2.efactor < state1.efactor);
  assert_equal 2 state2.repetitions;
  
  (* Test quality 2 (incorrect) - should restart *)
  let state3 = update_card_state state2 Q2 in
  assert_equal 0 state3.repetitions;
  assert_equal 0 state3.interval;
  assert_bool "E-Factor should not change" (abs_float (state3.efactor -. state2.efactor) < 0.01);
  
  Lwt.return_unit

let test_sm2_efactor_minimum _ =
  let state = { efactor = 1.3; interval = 5; repetitions = 10 } in
  let state1 = update_card_state state Q0 in
  assert_bool "E-Factor should not go below 1.3" (state1.efactor >= 1.3);
  Lwt.return_unit

let test_calculate_next_review _ =
  let state = { efactor = 2.5; interval = 5; repetitions = 3 } in
  let next_review = calculate_next_review state in
  let now = Int64.of_float (Unix.time ()) in
  let expected = Int64.add now (Int64.mul 5L 86400L) in
  assert_bool "Next review should be approximately 5 days from now"
    (Int64.abs (Int64.sub next_review expected) < 60L);
  Lwt.return_unit

let test_get_due_flashcards _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* user_result = create_user db "testuser" password_hash in
    match user_result with
    | Ok user_id ->
      let now = Int64.of_float (Unix.time ()) in
      let past = Int64.sub now 86400L in (* 1 day ago *)
      let future = Int64.add now 86400L in (* 1 day from now *)
      
      (* Create a card that's due (past) *)
      let* _ = create_flashcard db user_id "Due Q" "Due A" in
      let* card1_result = get_flashcard db 1L user_id in
      (match card1_result with
      | Ok (Some card1) ->
        let due_card = { card1 with next_review = past } in
        let* _ = update_flashcard db due_card in
        ()
      | _ -> ());
      
      (* Create a card that's not due (future) *)
      let* _ = create_flashcard db user_id "Future Q" "Future A" in
      let* card2_result = get_flashcard db 2L user_id in
      (match card2_result with
      | Ok (Some card2) ->
        let future_card = { card2 with next_review = future } in
        let* _ = update_flashcard db future_card in
        ()
      | _ -> ());
      
      let* result = get_due_flashcards db user_id in
      match result with
      | Ok cards ->
        assert_bool "Should have at least one due card" (List.length cards >= 1);
        assert_bool "All cards should be due" 
          (List.for_all (fun c -> Int64.compare c.next_review now <= 0) cards);
        Lwt.return_unit
      | Error msg -> assert_failure ("Failed to get due flashcards: " ^ msg)
    | Error msg -> assert_failure ("Failed to create user: " ^ msg)
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let suite =
  "Database and SM2 Tests" >:::
  [
    "create_user" >:: test_create_user;
    "get_user_by_username" >:: test_get_user_by_username;
    "create_flashcard" >:: test_create_flashcard;
    "get_flashcard" >:: test_get_flashcard;
    "update_flashcard" >:: test_update_flashcard;
    "delete_flashcard" >:: test_delete_flashcard;
    "get_flashcards" >:: test_get_flashcards;
    "get_due_flashcards" >:: test_get_due_flashcards;
    "sm2_algorithm" >:: test_sm2_algorithm;
    "sm2_efactor_minimum" >:: test_sm2_efactor_minimum;
    "calculate_next_review" >:: test_calculate_next_review;
  ]

let () =
  Lwt_main.run (OUnit2.run_test_tt_main suite)
