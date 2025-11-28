open Lwt.Syntax
open Dream
open Yojson.Safe

module DB = Database
module SM2 = Sm2
module Auth = Auth

let json_response json =
  Dream.response ~headers:[("Content-Type", "application/json; charset=utf-8")] (Yojson.Safe.to_string json)

let error_response status message =
  json_response (`Assoc [("error", `String message)]) |> status status

let success_response data =
  json_response (`Assoc [("success", `Bool true); ("data", data)])

let get_user_id request =
  match Dream.session request "user_id" with
  | Some user_id_str ->
    (try Some (Int64.of_string user_id_str) with _ -> None)
  | None -> None

let require_auth handler request =
  match get_user_id request with
  | Some user_id -> handler user_id request
  | None -> error_response `Unauthorized "Authentication required" |> Lwt.return

let register_handler db request =
  let* body = Dream.body request in
  match Yojson.Safe.from_string body with
  | exception _ -> error_response `Bad_Request "Invalid JSON" |> Lwt.return
  | json ->
    let username = json |> member "username" |> to_string_option in
    let password = json |> member "password" |> to_string_option in
    match username, password with
    | Some username, Some password ->
      let password_hash = Auth.hash_password password in
      let* result = DB.create_user db username password_hash in
      match result with
      | Ok user_id ->
        let* _ = Dream.set_session request "user_id" (Int64.to_string user_id) in
        success_response (`Assoc [("user_id", `String (Int64.to_string user_id)); ("username", `String username)])
        |> Lwt.return
      | Error msg -> error_response `Conflict msg |> Lwt.return
    | _ -> error_response `Bad_Request "Missing username or password" |> Lwt.return

let login_handler db request =
  let* body = Dream.body request in
  match Yojson.Safe.from_string body with
  | exception _ -> error_response `Bad_Request "Invalid JSON" |> Lwt.return
  | json ->
    let username = json |> member "username" |> to_string_option in
    let password = json |> member "password" |> to_string_option in
    match username, password with
    | Some username, Some password ->
      let* result = DB.get_user_by_username db username in
      match result with
      | Ok (Some user) ->
        if Auth.verify_password password user.DB.password_hash then
          let* _ = Dream.set_session request "user_id" (Int64.to_string user.DB.id) in
          success_response (`Assoc [("user_id", `String (Int64.to_string user.DB.id)); ("username", `String user.DB.username)])
          |> Lwt.return
        else
          error_response `Unauthorized "Invalid username or password" |> Lwt.return
      | Ok None -> error_response `Unauthorized "Invalid username or password" |> Lwt.return
      | Error msg -> error_response `Internal_Server_Error msg |> Lwt.return
    | _ -> error_response `Bad_Request "Missing username or password" |> Lwt.return

let logout_handler request =
  let* _ = Dream.invalidate_session request in
  success_response (`Null) |> Lwt.return

let get_current_user_handler db user_id _request =
  let* result = DB.get_user_by_id db user_id in
  match result with
  | Ok (Some user) ->
    success_response (`Assoc [("user_id", `String (Int64.to_string user.DB.id)); ("username", `String user.DB.username)])
    |> Lwt.return
  | Ok None -> error_response `NotFound "User not found" |> Lwt.return
  | Error msg -> error_response `Internal_Server_Error msg |> Lwt.return

let create_flashcard_handler db user_id request =
  let* body = Dream.body request in
  match Yojson.Safe.from_string body with
  | exception _ -> error_response `Bad_Request "Invalid JSON" |> Lwt.return
  | json ->
    let question = json |> member "question" |> to_string_option in
    let answer = json |> member "answer" |> to_string_option in
    match question, answer with
    | Some question, Some answer ->
      let* result = DB.create_flashcard db user_id question answer in
      match result with
      | Ok flashcard_id ->
        success_response (`Assoc [("flashcard_id", `String (Int64.to_string flashcard_id))])
        |> Lwt.return
      | Error msg -> error_response `Internal_Server_Error msg |> Lwt.return
    | _ -> error_response `Bad_Request "Missing question or answer" |> Lwt.return

let get_flashcards_handler db user_id _request =
  let* result = DB.get_flashcards db user_id in
  match result with
  | Ok flashcards ->
    let cards_json = `List (List.map (fun card ->
      `Assoc [
        ("id", `String (Int64.to_string card.DB.id));
        ("question", `String card.DB.question);
        ("answer", `String card.DB.answer);
        ("efactor", `Float card.DB.efactor);
        ("interval", `Int card.DB.interval);
        ("repetitions", `Int card.DB.repetitions);
        ("next_review", `String (Int64.to_string card.DB.next_review));
      ]
    ) flashcards) in
    success_response cards_json |> Lwt.return
  | Error msg -> error_response `Internal_Server_Error msg |> Lwt.return

let get_flashcard_handler db user_id request =
  let flashcard_id_str = Dream.param request "id" in
  match Int64.of_string_opt flashcard_id_str with
  | Some flashcard_id ->
    let* result = DB.get_flashcard db flashcard_id user_id in
    match result with
    | Ok (Some card) ->
      success_response (`Assoc [
        ("id", `String (Int64.to_string card.DB.id));
        ("question", `String card.DB.question);
        ("answer", `String card.DB.answer);
        ("efactor", `Float card.DB.efactor);
        ("interval", `Int card.DB.interval);
        ("repetitions", `Int card.DB.repetitions);
        ("next_review", `String (Int64.to_string card.DB.next_review));
      ]) |> Lwt.return
    | Ok None -> error_response `NotFound "Flashcard not found" |> Lwt.return
    | Error msg -> error_response `Internal_Server_Error msg |> Lwt.return
  | None -> error_response `Bad_Request "Invalid flashcard ID" |> Lwt.return

let update_flashcard_handler db user_id request =
  let flashcard_id_str = Dream.param request "id" in
  match Int64.of_string_opt flashcard_id_str with
  | Some flashcard_id ->
    let* body = Dream.body request in
    match Yojson.Safe.from_string body with
    | exception _ -> error_response `Bad_Request "Invalid JSON" |> Lwt.return
    | json ->
      let* existing_result = DB.get_flashcard db flashcard_id user_id in
      match existing_result with
      | Ok (Some existing_card) ->
        let question = json |> member "question" |> to_string_option |> Option.value ~default:existing_card.DB.question in
        let answer = json |> member "answer" |> to_string_option |> Option.value ~default:existing_card.DB.answer in
        let updated_card = { existing_card with DB.question; answer } in
        let* result = DB.update_flashcard db updated_card in
        match result with
        | Ok () -> success_response (`Null) |> Lwt.return
        | Error msg -> error_response `Internal_Server_Error msg |> Lwt.return
      | Ok None -> error_response `NotFound "Flashcard not found" |> Lwt.return
      | Error msg -> error_response `Internal_Server_Error msg |> Lwt.return
  | None -> error_response `Bad_Request "Invalid flashcard ID" |> Lwt.return

let delete_flashcard_handler db user_id request =
  let flashcard_id_str = Dream.param request "id" in
  match Int64.of_string_opt flashcard_id_str with
  | Some flashcard_id ->
    let* result = DB.delete_flashcard db flashcard_id user_id in
    match result with
    | Ok () -> success_response (`Null) |> Lwt.return
    | Error msg -> error_response `Internal_Server_Error msg |> Lwt.return
  | None -> error_response `Bad_Request "Invalid flashcard ID" |> Lwt.return

let get_due_flashcards_handler db user_id _request =
  let* result = DB.get_due_flashcards db user_id in
  match result with
  | Ok flashcards ->
    let cards_json = `List (List.map (fun card ->
      `Assoc [
        ("id", `String (Int64.to_string card.DB.id));
        ("question", `String card.DB.question);
        ("answer", `String card.DB.answer);
        ("efactor", `Float card.DB.efactor);
        ("interval", `Int card.DB.interval);
        ("repetitions", `Int card.DB.repetitions);
        ("next_review", `String (Int64.to_string card.DB.next_review));
      ]
    ) flashcards) in
    success_response cards_json |> Lwt.return
  | Error msg -> error_response `Internal_Server_Error msg |> Lwt.return

let review_flashcard_handler db user_id request =
  let flashcard_id_str = Dream.param request "id" in
  match Int64.of_string_opt flashcard_id_str with
  | Some flashcard_id ->
    let* body = Dream.body request in
    match Yojson.Safe.from_string body with
    | exception _ -> error_response `Bad_Request "Invalid JSON" |> Lwt.return
    | json ->
      let quality_int = json |> member "quality" |> to_int_option in
      match quality_int with
      | Some q when q >= 0 && q <= 5 ->
        let quality = SM2.int_to_quality q in
        let* existing_result = DB.get_flashcard db flashcard_id user_id in
        match existing_result with
        | Ok (Some existing_card) ->
          let state = {
            SM2.efactor = existing_card.DB.efactor;
            interval = existing_card.DB.interval;
            repetitions = existing_card.DB.repetitions;
          } in
          let new_state = SM2.update_card_state state quality in
          let next_review = SM2.calculate_next_review new_state in
          let updated_card = {
            existing_card with
            DB.efactor = new_state.SM2.efactor;
            interval = new_state.SM2.interval;
            repetitions = new_state.SM2.repetitions;
            next_review;
          } in
          let* result = DB.update_flashcard db updated_card in
          match result with
          | Ok () ->
            success_response (`Assoc [
              ("efactor", `Float updated_card.DB.efactor);
              ("interval", `Int updated_card.DB.interval);
              ("repetitions", `Int updated_card.DB.repetitions);
              ("next_review", `String (Int64.to_string updated_card.DB.next_review));
            ]) |> Lwt.return
          | Error msg -> error_response `Internal_Server_Error msg |> Lwt.return
        | Ok None -> error_response `NotFound "Flashcard not found" |> Lwt.return
        | Error msg -> error_response `Internal_Server_Error msg |> Lwt.return
      | _ -> error_response `Bad_Request "Quality must be between 0 and 5" |> Lwt.return
  | None -> error_response `Bad_Request "Invalid flashcard ID" |> Lwt.return

let import_mnemosyne_handler db user_id request =
  let* body = Dream.body request in
  let content = body in
  let lines = String.split_on_char '\n' content in
  let rec process_lines acc = function
    | [] -> Lwt.return acc
    | line :: rest ->
      let trimmed = String.trim line in
      if trimmed = "" then process_lines acc rest
      else
        match String.split_on_char '\t' trimmed with
        | question :: answer_parts ->
          let answer = String.concat "\t" answer_parts in
          let* result = DB.create_flashcard db user_id question answer in
          (match result with
          | Ok _ -> process_lines (acc + 1) rest
          | Error _ -> process_lines acc rest)
        | _ -> process_lines acc rest
  in
  let* count = process_lines 0 lines in
  success_response (`Assoc [("imported", `Int count)]) |> Lwt.return
