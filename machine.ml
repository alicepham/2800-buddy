
type direction = Left | Right

type state =  Q of int | None

type transition_function = state * string * state *string * direction list

type machine = {all_states : state list ; prev_state : state ;
               curr_state: state; next_state : state ; alphabet : string list ;
               tape_char : string}

(* [remove_dups lst] Removes duplicate elements from a lst
 * Requires:
 *  - lst is a valid list*)
let rec remove_dups lst =
  match lst with
  | [] -> []
  | h::t -> if List.mem h t then remove_dups t
            else h :: remove_dups t

(* [get_states lst] Returns the current state of the Turing machine as defined
 * by the transition function. This is then used Dictionary look-up
 * Requires:
 *  - lst is a valid list describing the transition function *)
let rec get_states lst =
  match lst with
  | [] -> []
  | ((x,y), (a,b,c))::t -> x::(get_states t)

(* [get_alphabet lst] Returns the current tape character of the Turing machine
 * as defined by the transition function. This is then used Dictionary look-up
 * Requires:
 *  - lst is a valid list describing the transition function *)
let rec get_alphabet lst =
  match lst with
  | [] -> []
  | ((x,y),(a,b,c))::t -> y::(get_alphabet t)

(* [position machine lst] Indexes the states of the transition function
 * Requires:
 *  - machine a valid machine type
 *  - lst a valid transition function type *)
let rec position machine lst =
  match lst with
  | [] -> 0
  | ((x,y),(a,b,c))::t -> if machine.curr_state = x then 0
                          else
                            1 + position machine t

(* [find_next_state k] Returns the next state in the transition function
 * list
 * Requires:
 *  - k is a valid integer *)
let rec find_next_state k = function
  | [] -> raise (Failure "Machine is in final state")
  | ((x,y),(a,b,c))::t -> if k = 0 then x else find_next_state (k-1) t

(* [transform machine matrix] Finds the state, tape character, and direction
 * the current state and tape character correspond to. This effectively
 * transforms the Turing machine values as defined by the transition function
 * Requires:
 *  - machine is a valid machine type
 *  - matrix is a valid transition function type *)
let rec transform machine matrix =
  match matrix with
  | [] -> raise (Failure "Not Found")
  | ((x,y),(a,b,c))::t -> if machine.curr_state = x &&
                             machine.tape_char = y then (a,b,c)
                          else transform machine t

(* [step mach] Updates the Turing machine to reflect a transition function
 * determined transformation.
 * Requires:
 *  - mach is a valid machine type*)
let step mach =
  let update = transform mach matrix in
  match update with
  | (x,y,z) -> {all_states = remove_dups(get_states matrix );
                prev_state = mach.curr_state;
                curr_state = x; next_state =
                find_next_state ((position mach matrix)+1) matrix;
                alphabet = remove_dups(get_alphabet matrix) ;
                tape_char = y}
