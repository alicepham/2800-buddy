open GMain
open GdkKeysyms

let locale = GtkMain.Main.init ()

type state = Q of int 

type key = state * char

(* type value = state * char * char *)
type value = {state: string; input: string; dir: string}

(*initializing states*)
(* let current_state = ref (Q 0, 'T') *)
let current_state = ref "q_0"
let current_input = ref "T"

(* let next_state = ref (Q 0, 'T') *)
(* let input_char = ref 'T' *)

(* [current] is a global variable that keeps track of the 
 * index of where you are in the tape*)
let current_index = ref 0

(* let (turing_machine: (key * value) list ref) = ref [] *)

(* [turing_machine] is a global variable to hold all GUI the tape cells in the
 * tape of a turing machine*)
let (turing_machine: (GEdit.entry ref) list ref) = ref []

(*[transition matrix] is a global variable  to hold an association dictionary*)
let (transition_matrix: ((string * string) * GEdit.entry) list ref) = ref []
let (parsed_transition_matrix: ((string * string) * value) list ref) = ref []

let setup packing (make_entry : (GObj.widget -> unit) -> GEdit.entry) =
  let box = GPack.hbox ~packing () in
  let entry = make_entry box#pack in
  let button = GButton.button ~stock:`JUMP_TO ~packing:box#pack () in
  button#connect#clicked (fun () -> prerr_endline entry#text) ;
    ()

(* [string_to_list s] is a helper function that seperates a string into a string
 * list of the individual characters in that string.
 *)
let string_to_list (s: string) =
  let rec helper c len = 
    if c = len then []
    else (String.get s c)::(helper (c+1) len)
  in 
  helper 0 (String.length s)
  
(* [list_to_string l] is a helper function for debugging purposes, called
 * to print the contents of the transition matrix/dictionary
 *)
let list_to_string l =
 (*  List.fold_left 
  let first = List.nth l 0 in
    first#text *)
  let a = List.fold_left 
    (fun acc ((n,q),s) -> acc^"(("^n^", "^q^"), "^(s#text)^"); ") "" l in
    "["^a^"]"
  (* String.concat "" (List.map (String.make 1) l) *)

(* [parse s] is a simple function that takes in a value of form
 * "(q_0, a, R)" and returns a triple of form ("q_0","a","R").
 * It is used to parse through the text entry inputs
 *)
let parse (s:string) : value =
  let char_list = string_to_list s in 
  let n_char_list = List.map (fun x -> String.make 1 x) char_list in
  match n_char_list with
  | [p_1; q; u; o; c_1; i; c_2; dir1; p2] -> 
      let state1 = q^u^o in
        {state=state1; input=i; dir = dir1}
      (* (state, i, dir) *)
  | _ -> raise (Failure "invalid entry in transition matrix")

(* [step packing ()] is a function that steps the turing machine. 
 * It accesses global variable current, transition function and 
 * turing machine. 
 * It then does the following:
 * 1) lookup in transition dictionary for triple given current state
 *    and input: (next_state, new_char, direction)
 * 2) updates the char in the current_tape_cell to new_char
 * 3) update the current_state to the next_state
 * 4) updates the current_tape_cell with the next_tape_cell using direction
 *)
let step packing () =
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

(* [make_turing_machine packing s] does three things:
 * 1) creates a table for the turing machine 
 * 2) creates each of the individual tape cells and adds to table 
 * 3) adds each cell to a global [turing_machine] variable so each 
 *    cell can be accessed/manipulated later 
 *)
let make_turing_machine packing (s:string) =
  let turing_table = GPack.table ~rows: 1 ~columns: ((String.length s)+3) 
  ~row_spacings:10 ~col_spacings:10 ~packing () in 
  let turnstile = 
    ref (GEdit.entry ~width: 50 ~height: 50 ~has_frame: true ~text:"T" 
      ~packing:(turing_table #attach ~left:1 ~top:0 ~expand:`NONE) ()) in
  let _ = turing_machine := turnstile::!turing_machine in
  for i = 0 to ((String.length s)-1) do
    turing_machine := ref(GEdit.entry ~width: 50 ~height: 50 ~has_frame: false
      ~text:(Char.escaped (String.get s i)) ~packing:(turing_table #attach 
        ~left:(i+2) ~top:0 ~expand:`NONE) ())::!turing_machine
  done;
  let empty = ref (GEdit.entry ~width: 50 ~height: 50 ~has_frame: false
    ~text:"u" ~packing:(turing_table #attach ~left:((String.length s)+2) 
      ~top:0 ~expand:`NONE) ()) in
  let _ = turing_machine := empty::!turing_machine in
  let _ = turing_machine := List.rev !turing_machine in
(*   let _ = empty := ((GEdit.entry ~width: 50 ~height: 50 ~has_frame: false
    ~text:"emp" ~packing:(turing_table #attach ~left:((String.length s)+2) 
      ~top:0 ~expand:`NONE) ())) in (*this works!!!!! :DDDD*) *)
  ()

(*[make_matrix packing rows cols] creates a transition matrix in the GUI*)
let make_matrix packing (r: string) (c: string) (input_alphabet: string list) =
  (*TODO make into a pop-up window*)
  let rows = int_of_string r in
  let cols = int_of_string c in
  let frame = GBin.frame ~label:"Transition Function Matrix" ~packing () in
  let button_submit = GButton.button ~label: "submit matrix" ~packing () in
    button_submit#connect#clicked 
      (fun () -> prerr_endline (list_to_string !transition_matrix));
  let scrolled_window = GBin.scrolled_window ~border_width:10 
    ~hpolicy: `AUTOMATIC ~vpolicy:`AUTOMATIC ~height: 250 
      ~packing: frame#add () in
  let table = GPack.table ~rows:rows ~columns:(cols+2) ~row_spacings:5 
    ~col_spacings:5 ~packing:scrolled_window#add_with_viewport () in
  table #focus#set_hadjustment (Some scrolled_window # hadjustment);
  table #focus#set_vadjustment (Some scrolled_window # vadjustment);
  let input_turnstile = GButton.toggle_button ~label:("T")
    ~packing:(table #attach ~left: 1 ~top: 0 ~expand: `BOTH) () in
  for m = 2 to (cols+1) do
    GButton.toggle_button ~label:(String.make 1 (Char.chr(m+95)))
    ~packing:(table #attach ~left: m ~top: 0 ~expand: `BOTH) ()
  done;
  for n = 1 to rows do
    GButton.toggle_button ~label:("q_"^(string_of_int (n-1)))
    ~packing:(table #attach ~left: 0 ~top: n ~expand: `BOTH) ()
  done;(* Char.chr(i + 97) *)
  let input_turnstile = GButton.toggle_button ~label:("u")
    ~packing:(table #attach ~left: (cols+2) ~top: 0 ~expand: `BOTH) () in
  for i = 1 to (cols+2) do
    (* let y = [] *)
    for j=1 to (rows) do
      if i=1 then 
        transition_matrix := 
        (("q_"^(string_of_int (j-1)), "T"),
        (GEdit.entry ~packing:(table #attach ~left:i ~top:j ~expand:`BOTH) ()))
          ::(!transition_matrix)
      else
      if i=(cols+2) then
        transition_matrix := 
        (("q_"^(string_of_int (j-1)), "u"),
        (GEdit.entry ~packing:(table #attach ~left:i ~top:j ~expand:`BOTH) ()))
          ::(!transition_matrix)        
      else
      transition_matrix := 
        (("q_"^(string_of_int (j-1)), (String.make 1 (Char.chr(i+95)))),
        (GEdit.entry ~packing:(table #attach ~left:i ~top:j ~expand:`BOTH) ()))
          ::(!transition_matrix)
    done
  done;
  ()

(* let make_simple_entry packing =
  let entry = GEdit.entry ~text:"text" ~packing () in
  let (model, col) = model_of_list Gobject.Data.string string_completion_list in
  let c = GEdit.entry_completion ~model ~entry () in
  c#set_text_column col ;
  c#insert_action_markup 0 "<span foreground='blue'>action 0</span>" ;
  c#insert_action_markup 1 "<span foreground='red' >action 1</span>" ;
  entry *)
 
let main () =
  let window = GWindow.window ~height: 600 ~width: 500 
    ~title:"CS 2800 Buddy" () in
  let vbox = GPack.vbox ~spacing: 5 ~packing:window#add () in

  (*frames*)
  let input_str_frame = GBin.frame ~label:"Inputs"
    ~packing:vbox#pack () in
  let input_trans_frame = GBin.frame ~label:"Transition Function"
    ~packing:vbox#pack () in

  (*hboxes to hold entries*)
  let box_entry_str = GPack.hbox ~height: 20 ~width: 100 ~spacing: 10 
    ~homogeneous: true ~packing: input_str_frame#add  () in
  let box_entry_trans = GPack.hbox ~height: 20 ~width: 100 ~spacing: 10 
    ~homogeneous: true ~packing: input_trans_frame#add  () in
  let box_entry_num_input = GPack.hbox ~height: 20 ~width: 100 ~spacing: 10 
    ~homogeneous: true ~packing: vbox#add  () in
  let box_entry_num_states = GPack.hbox ~height: 20 ~width: 100 ~spacing: 10 
    ~homogeneous: true ~packing: vbox#add  () in
  let box_make_matrix = GPack.hbox ~height: 20 ~width: 100 ~spacing: 10 
    ~homogeneous: true ~packing: vbox#add  () in
  let box_trans_matrix = GPack.vbox ~border_width: 5 ~packing: vbox#add () in
    window#connect#destroy ~callback:Main.quit;
  (*keep some mutable record that gets updated every time the button is pressed. 
    i can use eval's mli or something*)

  (*entries*)
  let entry_string = GEdit.entry ~packing:box_entry_str#add () in
  let entry_transition = GEdit.entry ~packing:box_entry_trans#add () in
  let entry_num_input = GEdit.entry ~packing:box_entry_num_input#add () in
  let entry_num_states = GEdit.entry ~packing:box_entry_num_states#add () in

  (*buttons*)
(*   let button_entry_str = GButton.button ~label:"input string" 
    ~packing:box_entry_str#add () in button_entry_str#connect#clicked     
    (fun () -> prerr_endline entry_string#text); *)
  let button_entry_trans = GButton.button ~label:"input transition function" 
    ~packing:box_entry_trans#add () in button_entry_trans#connect#clicked     
    (fun () -> prerr_endline entry_transition#text) ;
  let button_entry_num_input = GButton.button ~label:"input number of input alphabet" 
    ~packing:box_entry_num_input#add () in button_entry_num_input#connect#clicked     
    (fun () -> prerr_endline entry_transition#text) ;
  let button_entry_num_states = GButton.button ~label:"input number of states" 
    ~packing:box_entry_num_states#add () in button_entry_num_states#connect#clicked     
    (fun () -> prerr_endline entry_transition#text) ;
  let button_make_matrix = GButton.button ~label:"make matrix" 
    ~packing:box_make_matrix#add () in button_make_matrix#connect#clicked     
    (fun () -> make_matrix box_trans_matrix#pack entry_num_states#text 
      entry_num_input#text ["hello"]) ;

  (*for me to get the entries of each, i can save all of them 
  to a matrix then iterate through the entire matrix to get the values.*)
  let box_turing = GPack.vbox ~packing: vbox#pack () in
  let turing_frame = GBin.frame ~label:"Turing Machine" 
    ~packing:box_turing#pack () in

  let button_entry_str = GButton.button ~label:"input string" 
    ~packing:box_entry_str#add () in button_entry_str#connect#clicked     
    (fun () -> make_turing_machine turing_frame#add entry_string#text);
  let box_step = GPack.hbox ~height: 20 ~width: 100 ~spacing: 10  
    ~packing: box_turing#pack () in


  (*turing step button*)
  let button_step = GButton.button ~label: "step forward" 
    ~packing: box_step#pack () in button_step#connect#clicked (step ()); 
  (* Menu bar *)
  (*let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "Quit" ~key:_Q ~callback: Main.quit;
 *)

  (* Display the windows and enter Gtk+ main loop *)
  (* window#add_accel_group accel_group; *)
  window#show ();
  Main.main ()

let () = main ()