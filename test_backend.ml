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
  let* connection = Caqti_lwt_unix.connect (Uri.of_string uri) in
  match connection with
  | Ok (module Db : Caqti_lwt.CONNECTION) ->
    let* () = init_db (module Db) in
    Lwt.return (Ok (module Db : Caqti_lwt.CONNECTION))
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
        Lwt.return_unit
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

let test_user_cannot_access_other_users_flashcards _ =
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
        (* User2 tries to access User1's flashcard - should fail *)
        let* result = get_flashcard db card_id user2_id in
        match result with
        | Ok None -> Lwt.return_unit (* Correct: user2 cannot see user1's card *)
        | Ok (Some _) -> assert_failure "User2 should not be able to access User1's flashcard"
        | Error msg -> assert_failure ("Unexpected error: " ^ msg)
      | Error msg -> assert_failure ("Failed to create flashcard: " ^ msg)
    | _ -> assert_failure "Failed to create users"
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_user_cannot_update_other_users_flashcards _ =
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
        (* Get the card as user1 *)
        let* get_result = get_flashcard db card_id user1_id in
        match get_result with
        | Ok (Some card) ->
          (* User2 tries to update User1's flashcard - should fail *)
          let malicious_card = { card with user_id = user2_id; question = "Hacked!" } in
          let* update_result = update_flashcard db malicious_card in
          match update_result with
          | Ok () ->
            (* Verify the card wasn't actually updated *)
            let* verify_result = get_flashcard db card_id user1_id in
            match verify_result with
            | Ok (Some updated_card) ->
              assert_equal "User1's Question" updated_card.question;
              Lwt.return_unit
            | _ -> assert_failure "Failed to verify card wasn't updated"
          | Error _ -> Lwt.return_unit (* Update should fail, which is correct *)
        | _ -> assert_failure "Failed to get flashcard"
      | Error msg -> assert_failure ("Failed to create flashcard: " ^ msg)
    | _ -> assert_failure "Failed to create users"
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_user_cannot_delete_other_users_flashcards _ =
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
        (* User2 tries to delete User1's flashcard - should fail *)
        let* delete_result = delete_flashcard db card_id user2_id in
        match delete_result with
        | Ok () ->
          (* Verify the card still exists *)
          let* verify_result = get_flashcard db card_id user1_id in
          match verify_result with
          | Ok (Some _) -> Lwt.return_unit (* Card still exists, which is correct *)
          | Ok None -> assert_failure "Card should still exist after unauthorized delete attempt"
          | Error msg -> assert_failure ("Failed to verify card: " ^ msg)
        | Error _ -> Lwt.return_unit (* Delete should fail, which is correct *)
      | Error msg -> assert_failure ("Failed to create flashcard: " ^ msg)
    | _ -> assert_failure "Failed to create users"
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_users_only_see_their_own_flashcards _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* user1_result = create_user db "user1" password_hash in
    let* user2_result = create_user db "user2" password_hash in
    match user1_result, user2_result with
    | Ok user1_id, Ok user2_id ->
      (* User1 creates 2 flashcards *)
      let* _ = create_flashcard db user1_id "User1 Q1" "User1 A1" in
      let* _ = create_flashcard db user1_id "User1 Q2" "User1 A2" in
      (* User2 creates 1 flashcard *)
      let* _ = create_flashcard db user2_id "User2 Q1" "User2 A1" in
      (* User1 should only see their 2 cards *)
      let* user1_cards_result = get_flashcards db user1_id in
      match user1_cards_result with
      | Ok user1_cards ->
        assert_equal 2 (List.length user1_cards);
        assert_bool "All cards should belong to user1"
          (List.for_all (fun c -> Int64.equal c.user_id user1_id) user1_cards);
        (* User2 should only see their 1 card *)
        let* user2_cards_result = get_flashcards db user2_id in
        match user2_cards_result with
        | Ok user2_cards ->
          assert_equal 1 (List.length user2_cards);
          assert_bool "All cards should belong to user2"
            (List.for_all (fun c -> Int64.equal c.user_id user2_id) user2_cards);
          Lwt.return_unit
        | Error msg -> assert_failure ("Failed to get user2 cards: " ^ msg)
      | Error msg -> assert_failure ("Failed to get user1 cards: " ^ msg)
    | _ -> assert_failure "Failed to create users"
  | Error msg -> assert_failure ("Failed to setup database: " ^ msg)

let test_users_only_see_their_own_due_flashcards _ =
  let* db_result = setup_test_db () in
  match db_result with
  | Ok db ->
    let password_hash = hash_password "testpass" in
    let* user1_result = create_user db "user1" password_hash in
    let* user2_result = create_user db "user2" password_hash in
    match user1_result, user2_result with
    | Ok user1_id, Ok user2_id ->
      let now = Int64.of_float (Unix.time ()) in
      let past = Int64.sub now 86400L in
      
      (* User1 creates a due card *)
      let* _ = create_flashcard db user1_id "User1 Due Q" "User1 Due A" in
      let* card1_result = get_flashcard db 1L user1_id in
      (match card1_result with
      | Ok (Some card1) ->
        let due_card1 = { card1 with next_review = past } in
        let* _ = update_flashcard db due_card1 in
        ()
      | _ -> ());
      
      (* User2 creates a due card *)
      let* _ = create_flashcard db user2_id "User2 Due Q" "User2 Due A" in
      let* card2_result = get_flashcard db 2L user2_id in
      (match card2_result with
      | Ok (Some card2) ->
        let due_card2 = { card2 with next_review = past } in
        let* _ = update_flashcard db due_card2 in
        ()
      | _ -> ());
      
      (* User1 should only see their own due card *)
      let* user1_due_result = get_due_flashcards db user1_id in
      match user1_due_result with
      | Ok user1_due ->
        assert_equal 1 (List.length user1_due);
        assert_bool "All due cards should belong to user1"
          (List.for_all (fun c -> Int64.equal c.user_id user1_id) user1_due);
        (* User2 should only see their own due card *)
        let* user2_due_result = get_due_flashcards db user2_id in
        match user2_due_result with
        | Ok user2_due ->
          assert_equal 1 (List.length user2_due);
          assert_bool "All due cards should belong to user2"
            (List.for_all (fun c -> Int64.equal c.user_id user2_id) user2_due);
          Lwt.return_unit
        | Error msg -> assert_failure ("Failed to get user2 due cards: " ^ msg)
      | Error msg -> assert_failure ("Failed to get user1 due cards: " ^ msg)
    | _ -> assert_failure "Failed to create users"
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
    (* Security tests *)
    "user_cannot_access_other_users_flashcards" >:: test_user_cannot_access_other_users_flashcards;
    "user_cannot_update_other_users_flashcards" >:: test_user_cannot_update_other_users_flashcards;
    "user_cannot_delete_other_users_flashcards" >:: test_user_cannot_delete_other_users_flashcards;
    "users_only_see_their_own_flashcards" >:: test_users_only_see_their_own_flashcards;
    "users_only_see_their_own_due_flashcards" >:: test_users_only_see_their_own_due_flashcards;
  ]

let () =
  Lwt_main.run (OUnit2.run_test_tt_main suite)
