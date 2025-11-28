open Lwt.Syntax

(* Simple password hashing using SHA256 *)
let hash_password password =
  let open Cryptokit in
  let hash = Hash.sha256 () in
  hash#add_string password;
  hash#result |> transform_string (Base64.encode ()) |> String.trim

let verify_password password password_hash =
  let hashed = hash_password password in
  String.equal hashed password_hash

let create_session_token user_id =
  let open Cryptokit in
  let random = Random.string (Hash.sha256 ()) 32 in
  let timestamp = string_of_int (int_of_float (Unix.time ())) in
  let token = random ^ timestamp in
  let hash = Hash.sha256 () in
  hash#add_string token;
  hash#result |> transform_string (Base64.encode ()) |> String.trim
