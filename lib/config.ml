open Yojson.Safe
open Yojson.Safe.Util

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
    let yaml = match Yaml.of_file path with
      | Ok y -> y
      | Error _ -> raise (Invalid_argument "Failed to parse YAML file")
    in
    let database_path = Yaml.Util.to_string_default (Yaml.Util.find_exn yaml ["database_path"]) default_config.database_path in
    let port = Yaml.Util.to_int_default (Yaml.Util.find_exn yaml ["port"]) default_config.port in
    let host = Yaml.Util.to_string_default (Yaml.Util.find_exn yaml ["host"]) default_config.host in
    let static_dir = Yaml.Util.to_string_default (Yaml.Util.find_exn yaml ["static_dir"]) default_config.static_dir in
    let session_secret = Yaml.Util.to_string_default (Yaml.Util.find_exn yaml ["session_secret"]) default_config.session_secret in
    { database_path; port; host; static_dir; session_secret }
  with
  | _ -> default_config
