

(* 
let current_state = ref "q_0"
let current_input = ref "T"
let current_index = ref 0 *)
type state = Q of int
type direction = R | L
type input_write = char

type step_output = {new_state: state; input_write: input_write; direction: direction}

let make_state = 

let parse (s:string) : value =
  let char_list = string_to_list s in 
  let n_char_list = List.map (fun x -> String.make 1 x) char_list in
  match n_char_list with
  | [p_1; q; u; o; c_1; i; c_2; dir1; p2] -> 
      let state1 = q^u^o in
        {state=state1; input=i; dir = dir1}
      (* (state, i, dir) *)
  | _ -> raise (Failure "invalid entry in transition matrix")

(*machine.ml should return a tuple so I know what to do next*)
let step current_state current_input transition_matrix = 
  let three_tuple = List.assoc (current_state, current_input) transition_matrix in
    {new_state: }



let step transition_matrix packing () =
  (* raise (Failure "Unimplemnted") *)
  let trans_dictionary = 
    List.map (fun ((q,i),a) -> ((q,i),(parse a#text))) !transition_matrix in
(*   let _ = 
  parsed_transition_matrix :=
    List.map (fun ((q,i),a) -> ((q,i),(parse a#text))) !transition_matrix in *)
(*   let _ = prerr_endline (list_to_string !transition_matrix) in
  let _ = prerr_endline (string_of_int (List.length !transition_matrix)) in 
  let _ = prerr_endline (fst(fst (List.nth !transition_matrix 1))) in  *)
  let get_value = 
    List.assoc (!current_state, !current_input) trans_dictionary in
    (* List.assoc ("q_0","input_1") !transition_matrix in  *)
    (* List.nth !transition_matrix 1 in (*well that works..*) *)

  let new_state = get_value.state in
  let new_char = get_value.input in
  let dir = get_value.dir in
  let curr = !current_index in
  let curr_entry = List.nth !turing_machine curr in
  let _ = (!curr_entry)#set_has_frame false in
  let _ = (!curr_entry)#set_text new_char in
  let next_index = 
    (match dir with
    | "R" -> curr + 1 
    | "L" -> curr - 1 
    | _ -> raise (Failure "step - not a valid direction")) in
  (* let next = curr + 1 in *)
  let next_entry = List.nth !turing_machine next_index in
  let _ = (!next_entry)#set_has_frame true in
  let _ = current_index := next_index in
  let _ = current_state := new_state in
  let _ = current_input := (!next_entry)#text in
  let _ = prerr_endline ("current_state: "^(!current_state)) in
  let _ = prerr_endline ("current_input: "^(!current_input)) in
    () 
