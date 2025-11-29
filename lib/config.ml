open Yojson.Safe
open Yojson.Safe.Util

(* Yaml module might need to be opened or accessed differently *)

type config = {
  database_path: string;
  port: int;
  host: string;
  static_dir: string;
  session_secret: string;
}

let default_config = {
  database_path = "./mlocid.db";
  port = 8080;
  host = "127.0.0.1";
  static_dir = "./static";
  session_secret = "change-this-secret-in-production";
}

let load_config path =
  try
    let json = from_file path in
    let database_path = json |> member "database_path" |> to_string_option |> Option.value ~default:default_config.database_path in
    let port = json |> member "port" |> to_int_option |> Option.value ~default:default_config.port in
    let host = json |> member "host" |> to_string_option |> Option.value ~default:default_config.host in
    let static_dir = json |> member "static_dir" |> to_string_option |> Option.value ~default:default_config.static_dir in
    let session_secret = json |> member "session_secret" |> to_string_option |> Option.value ~default:default_config.session_secret in
    { database_path; port; host; static_dir; session_secret }
  with
  | _ -> default_config

let load_config_yaml path =
  try
    (* Read YAML file content *)
    let ic = open_in path in
    let rec read_lines acc =
      try
        let line = input_line ic in
        read_lines (line :: acc)
      with
      | End_of_file -> List.rev acc
    in
    let lines = read_lines [] in
    close_in ic;
    let content = String.concat "\n" lines in
    let yaml = match Yaml.of_string content with
      | Ok y -> y
      | Error _ -> raise (Invalid_argument "Failed to parse YAML file")
    in
    (* Extract values from YAML - try to access as a mapping and get values *)
    let get_string key default = 
      try
        match yaml with
        | `O assoc ->
          (try
             match List.assoc key assoc with
             | `String s -> s
             | _ -> default
           with Not_found -> default)
        | _ -> default
      with _ -> default
    in
    let get_int key default =
      try
        match yaml with
        | `O assoc ->
          (try
             match List.assoc key assoc with
             | `Float f -> int_of_float f
             | `Int i -> i
             | _ -> default
           with Not_found -> default)
        | _ -> default
      with _ -> default
    in
    let database_path = get_string "database_path" default_config.database_path in
    let port = get_int "port" default_config.port in
    let host = get_string "host" default_config.host in
    let static_dir = get_string "static_dir" default_config.static_dir in
    let session_secret = get_string "session_secret" default_config.session_secret in
    { database_path; port; host; static_dir; session_secret }
  with
  | _ -> default_config
