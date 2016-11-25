


type direction = Left | Right

type state = Q of int

type symbol = Blank | Sigma of int

type transition_function = state * symbol * direction list list

type machine = {all_states : state list ; prev_state : state ;
               curr_state: state; next_state : state ; alphabet : int list ;
               tape_symbol_read : symbol}


let matrix = [[(Q 0, Sigma 1, Right); (Q 1, Sigma 0, Right)];
               [(Q 1, Sigma 1, Left); (Q 0, Blank, Right)]]

let machine = {all_states = [Q 0 ; Q 1] ; prev_state = Q 0;
               curr_state = Q 1; next_state = Q 1 ; alphabet = [0;1];
               tape_symbol_read = Sigma 0}

(*[state_to_num state] unwraps a state to its int value for use in transition
 * function matrix lookup*)
let state_to_num state =
  match state with
  | Q n -> n

(* [symbol_to_num symbol] unwraps a symbol to its int value for transition
 * function matrix lookup. Here 2 corresponds to the Symbol column for the
 * Blank symbol*)
let symbol_to_num symbol =
  match symbol with
  | Sigma x -> x
  | Blank -> 2

(*[step mach] Steps a turing machine from its current state to the next
 * as determined by the trasition function matrix.
 * Requires
 *  - mach is a valid machine type*)
let step mach =
let x_comp = state_to_num (mach.curr_state) in
let y_comp = symbol_to_num(mach.tape_symbol_read) in
let move = List.nth (List.nth matrix x_comp) y_comp in
match move with
| (x,y,z) -> {all_states = [Q 0 ; Q 1] ; prev_state = mach.curr_state;
               curr_state = x; next_state = Q 1 ; alphabet = [0;1];
               tape_symbol_read = y}



