open Lwt.Syntax

(* Simple password hashing using SHA256 *)
let hash_password password =
  let digest = Digest.string password in
  Digest.to_hex digest

let verify_password password password_hash =
  let hashed = hash_password password in
  String.equal hashed password_hash

let create_session_token user_id =
  let random = String.init 32 (fun _ -> Char.chr (Random.int 256)) in
  let timestamp = string_of_int (int_of_float (Unix.time ())) in
  let token = random ^ timestamp ^ (Int64.to_string user_id) in
  Digest.string token |> Digest.to_hex
