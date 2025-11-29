open Lwt.Syntax
open Dream
open Caqti_lwt

module DB = Database
module Config = Config
module API = Api

let logger = Logs.Src.create "mlocid" ~doc:"Mlocid application"
let () = Logs.Src.set_level logger (Some Logs.Info)

let init_database (config : Config.config) =
  let uri = Printf.sprintf "sqlite3:%s" config.database_path in
  let* connection = Caqti_lwt.connect (Uri.of_string uri) in
  match connection with
  | Ok (module Db : CONNECTION) ->
    let* () = DB.init_db (module Db) in
    Lwt.return (Ok (module Db : CONNECTION))
  | Error e -> Lwt.return (Error (Caqti_error.show e))

let router db =
  Dream.router [
    (* Static files *)
    Dream.get "/static/**" (Dream.static ~local_path:"static" ~uri_prefix:"/static");
    
    (* Frontend pages *)
    Dream.get "/" (fun _ -> Dream.from_filesystem "static/index.html" "text/html");
    Dream.get "/login" (fun _ -> Dream.from_filesystem "static/login.html" "text/html");
    Dream.get "/register" (fun _ -> Dream.from_filesystem "static/register.html" "text/html");
    Dream.get "/cards" (fun _ -> Dream.from_filesystem "static/cards.html" "text/html");
    Dream.get "/study" (fun _ -> Dream.from_filesystem "static/study.html" "text/html");
    
    (* API endpoints *)
    Dream.post "/api/register" (API.register_handler db);
    Dream.post "/api/login" (API.login_handler db);
    Dream.post "/api/logout" API.logout_handler;
    Dream.get "/api/user" (API.require_auth (API.get_current_user_handler db));
    
    Dream.post "/api/flashcards" (API.require_auth (API.create_flashcard_handler db));
    Dream.get "/api/flashcards" (API.require_auth (API.get_flashcards_handler db));
    Dream.get "/api/flashcards/:id" (API.require_auth (API.get_flashcard_handler db));
    Dream.put "/api/flashcards/:id" (API.require_auth (API.update_flashcard_handler db));
    Dream.delete "/api/flashcards/:id" (API.require_auth (API.delete_flashcard_handler db));
    
    Dream.get "/api/study/due" (API.require_auth (API.get_due_flashcards_handler db));
    Dream.post "/api/study/review/:id" (API.require_auth (API.review_flashcard_handler db));
    Dream.post "/api/import/mnemosyne" (API.require_auth (API.import_mnemosyne_handler db));
  ]

let () =
  let config_path = 
    let args = Array.to_list Sys.argv in
    let rec find_config = function
      | [] -> "config.yaml"
      | "--config" :: path :: _ -> path
      | _ :: rest -> find_config rest
    in
    find_config args
  in
  let config = Config.load_config_yaml config_path in
  
  let* db_result = init_database config in
  match db_result with
  | Ok db ->
    let app = Dream.logger @@
              Dream.memory_sessions @@
              router db in
    let port = config.port in
    let host = config.host in
    Logs.info (fun m -> m "Starting mlocid server on %s:%d" host port);
    Dream.run ~interface:host ~port:port app
  | Error msg ->
    Logs.err (fun m -> m "Failed to initialize database: %s" msg);
    exit 1
