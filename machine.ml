
(**)
type direction = Left | Right

(**)
type state =  Q of int | None

type transition_function = state * string * state *string * direction list

type machine = {all_states : state list ; prev_state : state ;
               curr_state: state; next_state : state ; alphabet : string list ;
               tape_char : string}

let rec remove_dups lst =
  match lst with
  | [] -> []
  | h::t -> if List.mem h t then remove_dups t
            else h :: remove_dups t

let rec get_states lst =
  match lst with
  | [] -> []
  | ((x,y), (a,b,c))::t -> x::(get_states t)

let rec get_alphabet lst =
  match lst with
  | [] -> []
  | ((x,y),(a,b,c))::t -> y::(get_alphabet t)

  let rec position machine lst =
  match lst with
  | [] -> 0
  | ((x,y),(a,b,c))::t -> if machine.curr_state = x then 0
                          else
                            1 + position machine t

let rec find_next_state k = function
  | [] -> raise (Failure "Machine is in final state")
  | ((x,y),(a,b,c))::t -> if k = 0 then x else find_next_state (k-1) t

let rec transform machine matrix =
  match matrix with
  | [] -> raise (Failure "Not Found")
  | ((x,y),(a,b,c))::t -> if machine.curr_state = x &&
                                       machine.tape_char = y then (a,b,c)
                          else
                            transform machine t

let step mach =
  let update = transform mach matrix in
  match update with
  | (x,y,z) -> {all_states = remove_dups(get_states matrix );
                prev_state = mach.curr_state;
                curr_state = x; next_state =
                find_next_state ((position mach matrix)+1) matrix;
                alphabet = remove_dups(get_alphabet matrix) ;
                tape_char = y}
