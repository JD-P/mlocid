(* SM-2 Spaced Repetition Algorithm *)

type quality = Q0 | Q1 | Q2 | Q3 | Q4 | Q5

let quality_to_int = function
  | Q0 -> 0
  | Q1 -> 1
  | Q2 -> 2
  | Q3 -> 3
  | Q4 -> 4
  | Q5 -> 5

let int_to_quality = function
  | 0 -> Q0
  | 1 -> Q1
  | 2 -> Q2
  | 3 -> Q3
  | 4 -> Q4
  | 5 -> Q5
  | _ -> failwith "Invalid quality value"

type card_state = {
  efactor: float;
  interval: int;
  repetitions: int;
}

let initial_state = {
  efactor = 2.5;
  interval = 0;
  repetitions = 0;
}

(* Calculate new E-Factor based on quality *)
let update_efactor efactor quality =
  let q = float_of_int (quality_to_int quality) in
  let new_efactor = efactor +. (0.1 -. (5. -. q) *. (0.08 +. (5. -. q) *. 0.02)) in
  (* E-Factor should not fall below 1.3 *)
  max 1.3 new_efactor

(* Calculate next interval based on repetitions and E-Factor *)
let rec calculate_interval repetitions efactor =
  match repetitions with
  | 0 -> 1
  | 1 -> 6
  | n when n > 1 -> 
    let interval = float_of_int (calculate_interval (n - 1) efactor) *. efactor in
    int_of_float (ceil interval)
  | _ -> 1

(* Update card state based on quality response *)
let update_card_state state quality =
  let q = quality_to_int quality in
  let new_efactor = update_efactor state.efactor quality in
  
  (* If quality < 3, restart from beginning without changing E-Factor *)
  if q < 3 then
    { state with repetitions = 0; interval = 0 }
  else
    let new_repetitions = state.repetitions + 1 in
    let new_interval = calculate_interval new_repetitions new_efactor in
    {
      efactor = new_efactor;
      interval = new_interval;
      repetitions = new_repetitions;
    }

(* Calculate next review date (Unix timestamp) *)
let calculate_next_review state =
  let now = Int64.of_float (Unix.time ()) in
  let days = Int64.of_int state.interval in
  let seconds_per_day = 86400L in
  Int64.add now (Int64.mul days seconds_per_day)
