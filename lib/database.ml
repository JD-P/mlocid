open Lwt.Syntax
open Caqti_type
open Caqti_request

(* Connection module type - matches Caqti_lwt.CONNECTION interface *)
(* We define it here to avoid needing Caqti_lwt module in library compilation *)
module type CONNECTION = sig
  type 'a future = 'a Lwt.t
  val exec : ('a, 'b, 'c) Caqti_request.t -> 'a -> (unit, Caqti_error.t) result future
  val find : ('a, 'b, 'c) Caqti_request.t -> 'a -> ('b option, Caqti_error.t) result future
  val find_opt : ('a, 'b, 'c) Caqti_request.t -> 'a -> ('b option, Caqti_error.t) result future
  val collect_list : ('a, 'b, 'c) Caqti_request.t -> 'a -> ('b list, Caqti_error.t) result future
end

type user = {
  id: int64;
  username: string;
  password_hash: string;
}

type flashcard = {
  id: int64;
  user_id: int64;
  question: string;
  answer: string;
  efactor: float;
  interval: int;
  repetitions: int;
  next_review: int64; (* Unix timestamp *)
  created_at: int64;
  updated_at: int64;
}

let init_db (module Db : CONNECTION) =
  (*
   * Database schema in Boyce-Codd Normal Form (BCNF):
   * 
   * users table:
   *   - Candidate keys: {id}, {username}
   *   - Functional dependencies:
   *     * id → (username, password_hash, created_at) [id is a candidate key]
   *     * username → (id, password_hash, created_at) [username is a candidate key]
   *   - All determinants are candidate keys ✓ BCNF compliant
   * 
   * flashcards table:
   *   - Candidate keys: {id}
   *   - Functional dependencies:
   *     * id → (user_id, question, answer, efactor, interval, repetitions, next_review, created_at, updated_at) [id is a candidate key]
   *   - All determinants are candidate keys ✓ BCNF compliant
   *   - No transitive dependencies: all attributes depend directly on the primary key
   *)
  let* () = Db.exec
    (Caqti_request.exec
       "CREATE TABLE IF NOT EXISTS users (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          username TEXT UNIQUE NOT NULL,
          password_hash TEXT NOT NULL,
          created_at INTEGER NOT NULL DEFAULT (strftime('%s', 'now')),
          CONSTRAINT users_username_unique UNIQUE (username)
        )"
       ~oneshot:true)
    unit in
  let* () = Db.exec
    (Caqti_request.exec
       "CREATE TABLE IF NOT EXISTS flashcards (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          user_id INTEGER NOT NULL,
          question TEXT NOT NULL,
          answer TEXT NOT NULL,
          efactor REAL NOT NULL DEFAULT 2.5 CHECK (efactor >= 1.3),
          interval INTEGER NOT NULL DEFAULT 0 CHECK (interval >= 0),
          repetitions INTEGER NOT NULL DEFAULT 0 CHECK (repetitions >= 0),
          next_review INTEGER NOT NULL CHECK (next_review > 0),
          created_at INTEGER NOT NULL DEFAULT (strftime('%s', 'now')) CHECK (created_at > 0),
          updated_at INTEGER NOT NULL DEFAULT (strftime('%s', 'now')) CHECK (updated_at > 0),
          FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
          CONSTRAINT flashcards_id_pk PRIMARY KEY (id)
        )"
       ~oneshot:true)
    unit in
  let* () = Db.exec
    (Caqti_request.exec
       "CREATE INDEX IF NOT EXISTS idx_flashcards_user_id ON flashcards(user_id)"
       ~oneshot:true)
    unit in
  let* () = Db.exec
    (Caqti_request.exec
       "CREATE INDEX IF NOT EXISTS idx_flashcards_next_review ON flashcards(next_review)"
       ~oneshot:true)
    unit in
  Lwt.return_unit

let create_user (module Db : CONNECTION) username password_hash =
  let* () = Db.exec
    (Caqti_request.exec
       ~oneshot:true
       "INSERT INTO users (username, password_hash) VALUES (?, ?)"
       (tup2 string string))
    (username, password_hash) in
  let+ id = Db.find
    (Caqti_request.find_opt
       ~oneshot:true
       int64
       "SELECT last_insert_rowid()")
    unit in
  match id with
  | Ok (Some id) -> Ok id
  | Ok None -> Error "Failed to create user"
  | Error e -> Error (Caqti_error.show e)

let get_user_by_username (module Db : CONNECTION) username =
  let+ result = Db.find_opt
    (Caqti_request.find_opt
       ~oneshot:true
       (tup3 int64 string string)
       "SELECT id, username, password_hash FROM users WHERE username = ?")
    username in
  match result with
  | Ok (Some (id, username, password_hash)) -> Ok (Some { id; username; password_hash })
  | Ok None -> Ok None
  | Error e -> Error (Caqti_error.show e)

let get_user_by_id (module Db : CONNECTION) user_id =
  let+ result = Db.find_opt
    (Caqti_request.find_opt
       ~oneshot:true
       (tup3 int64 string string)
       "SELECT id, username, password_hash FROM users WHERE id = ?")
    user_id in
  match result with
  | Ok (Some (id, username, password_hash)) -> Ok (Some { id; username; password_hash })
  | Ok None -> Ok None
  | Error e -> Error (Caqti_error.show e)

let create_flashcard (module Db : CONNECTION) user_id question answer =
  let now = Int64.of_float (Unix.time ()) in
  let next_review = now in
  let* () = Db.exec
    (Caqti_request.exec
       ~oneshot:true
       "INSERT INTO flashcards (user_id, question, answer, next_review) VALUES (?, ?, ?, ?)"
       (tup4 int64 string string int64))
    (user_id, question, answer, next_review) in
  let+ id = Db.find
    (Caqti_request.find_opt
       ~oneshot:true
       int64
       "SELECT last_insert_rowid()")
    unit in
  match id with
  | Ok (Some id) -> Ok id
  | Ok None -> Error "Failed to create flashcard"
  | Error e -> Error (Caqti_error.show e)

let get_flashcard (module Db : CONNECTION) flashcard_id user_id =
  let+ result = Db.find_opt
    (Caqti_request.find_opt
       ~oneshot:true
       (tup8 int64 int64 string string float int int int64)
       "SELECT id, user_id, question, answer, efactor, interval, repetitions, next_review FROM flashcards WHERE id = ? AND user_id = ?")
    (tup2 int64 int64)
    (flashcard_id, user_id) in
  match result with
  | Ok (Some (id, user_id, question, answer, efactor, interval, repetitions, next_review)) ->
    Ok (Some { id; user_id; question; answer; efactor; interval; repetitions; next_review; created_at = 0L; updated_at = 0L })
  | Ok None -> Ok None
  | Error e -> Error (Caqti_error.show e)

let get_flashcards (module Db : CONNECTION) user_id =
  let+ result = Db.collect_list
    (Caqti_request.collect
       ~oneshot:true
       (tup8 int64 int64 string string float int int int64)
       "SELECT id, user_id, question, answer, efactor, interval, repetitions, next_review FROM flashcards WHERE user_id = ? ORDER BY created_at DESC")
    int64
    user_id in
  match result with
  | Ok rows ->
    Ok (List.map (fun (id, user_id, question, answer, efactor, interval, repetitions, next_review) ->
      { id; user_id; question; answer; efactor; interval; repetitions; next_review; created_at = 0L; updated_at = 0L })
      rows)
  | Error e -> Error (Caqti_error.show e)

let get_due_flashcards (module Db : CONNECTION) user_id =
  let now = Int64.of_float (Unix.time ()) in
  let+ result = Db.collect_list
    (Caqti_request.collect
       ~oneshot:true
       (tup8 int64 int64 string string float int int int64)
       "SELECT id, user_id, question, answer, efactor, interval, repetitions, next_review FROM flashcards WHERE user_id = ? AND next_review <= ? ORDER BY next_review ASC")
    (tup2 int64 int64)
    (user_id, now) in
  match result with
  | Ok rows ->
    Ok (List.map (fun (id, user_id, question, answer, efactor, interval, repetitions, next_review) ->
      { id; user_id; question; answer; efactor; interval; repetitions; next_review; created_at = 0L; updated_at = 0L })
      rows)
  | Error e -> Error (Caqti_error.show e)

let update_flashcard (module Db : CONNECTION) flashcard =
  let now = Int64.of_float (Unix.time ()) in
  let+ result = Caqti_lwt.exec (module Db)
    (Caqti_request.exec
       ~oneshot:true
       "UPDATE flashcards SET question = ?, answer = ?, efactor = ?, interval = ?, repetitions = ?, next_review = ?, updated_at = ? WHERE id = ? AND user_id = ?"
       (tup9 string string float int int int64 int64 int64 int64))
    (flashcard.question, flashcard.answer, flashcard.efactor, flashcard.interval, flashcard.repetitions, flashcard.next_review, now, flashcard.id, flashcard.user_id) in
  match result with
  | Ok () -> Ok ()
  | Error e -> Error (Caqti_error.show e)

let delete_flashcard (module Db : CONNECTION) flashcard_id user_id =
  let+ result = Caqti_lwt.exec (module Db)
    (Caqti_request.exec
       ~oneshot:true
       "DELETE FROM flashcards WHERE id = ? AND user_id = ?"
       (tup2 int64 int64))
    (flashcard_id, user_id) in
  match result with
  | Ok () -> Ok ()
  | Error e -> Error (Caqti_error.show e)
