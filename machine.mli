
(* Direction determines movement of head reader along tape relative to the
 * the current position. Left is leftward movement and right is rightward*)
type direction = Left | Right

(*State is the state of the Turing Machine*)
type state = Q of int

(* The transition function describes movement along the tape.
 * The function moves from (state, sigma) -> (state, sigma, direction)
 * producing the new state, the overwritten symbol and the direction moved
 * The results of this function are stored in a matrix *)
type transition_function = state * string * state *string* direction list

(* Type machine maintains all data necessary for a working Turing
 * machine*)
type machine = {all_states : state list ; prev_state : state ;
               curr_state: state; next_state : state ; alphabet : string list ;
               tape_symbol_read : string}

(* [step machine num] Takes a [machine] and steps the configuration the desired
 * number [num] of times, returning the transformed machine.   *)
val step : machine -> machine

(* [init x] Takes in the necessary components [x] of a turing machine and
 * produces a machine for use in GUI visualization*)
(* val init : string -> machine *)

