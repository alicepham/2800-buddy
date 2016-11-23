
(* Direction determines movement of head reader along tape relative to the
 * the current position. Left is leftward movement and right is rightward*)
type direction = Left | Right

(* Gamma represents the complete set of tape symbols. Sigma is a subset
 * of gamma*)
type gamma = Blank of string| Sigma of int list

(* The transition function describes movement along the tape.
 * The function moves from (state, sigma) -> (state, sigma, direction)
 * producing the new state, the overwritten symbol and the direction moved
 * The results of this function are stored in a matrix *)
type transition_function = string * gamma * direction list list

(* Type machine maintains all data necessary for a working Turing
 * machine*)
type machine = {all_states : string list ; prev_state : string ;
               curr_state: string; next_state : string ; sigma : int list ;
               tape_symbol_read : int}

(* [step machine num] Takes a [machine] and steps the configuration the desired
 * number [num] of times, returning the transformed machine.   *)
val step : machine -> int -> machine

(* [init x] Takes in the necessary components [x] of a turing machine and
 * produces a machine for use in GUI visualization*)
val init : x -> machine

