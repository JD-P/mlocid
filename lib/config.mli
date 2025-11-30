type config = {
  database_path : string;
  port : int;
  host : string;
  static_dir : string;
  session_secret : string;
}

val default_config : config

val load_config : string -> config
val load_config_yaml : string -> config

val database_path : config -> string
val port          : config -> int
val host          : config -> string
val static_dir    : config -> string
val session_secret : config -> string
