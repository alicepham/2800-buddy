open GMain
open GdkKeysyms

let locale = GtkMain.Main.init ()

type state = Q of int 

type key = state * char

type value = state * char * char

(*initializing states*)
let current_state = ref (Q 0, 'T')
let next_state = ref (Q 0, 'T')
let input_char = ref 'T'
let current = ref 0

(* let (turing_machine: (key * value) list ref) = ref [] *)

(* [turing_machine] is a global variable to hold all GUI the tape cells in the
 * tape of a turing machine*)
let (turing_machine: (GEdit.entry ref) list ref) = ref []

(*[transition matrix] is a global variable  to hold an association dictionary*)
let (transition_matrix: ((string * string) * GEdit.entry) list ref) = ref []

let setup packing (make_entry : (GObj.widget -> unit) -> GEdit.entry) =
  let box = GPack.hbox ~packing () in
  let entry = make_entry box#pack in
  let button = GButton.button ~stock:`JUMP_TO ~packing:box#pack () in
  button#connect#clicked (fun () -> prerr_endline entry#text) ;
    ()

(* [string_to_list s] is a helper function that seperates a string into a string
 * list of the individual characters in that string
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
  let curr = !current in
  let curr_entry = List.nth !turing_machine curr in
  let _ = (!curr_entry)#set_has_frame false in
  let next = curr + 1 in
  let next_entry = List.nth !turing_machine next in
  let _ = (!next_entry)#set_has_frame true in
  let _ = current := (!current + 1) in
    () 
let make_turing_machine packing (s:string) =
  let turing_table = GPack.table ~rows: 1 ~columns: ((String.length s)+3) 
  ~row_spacings:10 ~col_spacings:10 ~packing () in 
  let turnstile = ref (GEdit.entry ~width: 50 ~height: 50 ~has_frame: true
    ~text:"T" ~packing:(turing_table #attach ~left:1 ~top:0 ~expand:`NONE) ()) in
  let _ = turing_machine := turnstile::!turing_machine in
  for i = 0 to ((String.length s)-1) do
    turing_machine := ref(GEdit.entry ~width: 50 ~height: 50 ~has_frame: false
      ~text:(Char.escaped (String.get s i)) ~packing:(turing_table #attach 
        ~left:(i+2) ~top:0 ~expand:`NONE) ())::!turing_machine
  done;
  let empty = ref (GEdit.entry ~width: 50 ~height: 50 ~has_frame: false
    ~text:"empty" ~packing:(turing_table #attach ~left:((String.length s)+2) 
      ~top:0 ~expand:`NONE) ()) in
  (* let _ = (!empty)#set_has_frame true in *)

(*   (GEdit.entry ~width: 50 ~height: 50 ~has_frame: true ~text:"changed"
    ~packing: (!empty)?packing ())  *)
  let _ = turing_machine := empty::!turing_machine in
  let _ = turing_machine := List.rev !turing_machine in
(*   let _ = empty := ((GEdit.entry ~width: 50 ~height: 50 ~has_frame: false
    ~text:"emp" ~packing:(turing_table #attach ~left:((String.length s)+2) 
      ~top:0 ~expand:`NONE) ())) in (*this works!!!!! :DDDD*) *)
  ()
  
  (* let text_holder = GPack.hbox ~height: 50 ~width: 50 ~packing: 
    (turing_table #attach ~left:1 ~top: 0 ~expand: `BOTH) () in *)
  (* let a = !input_t1 in *)

  (* let _ = input_t1 := (GEdit.entry ~width: 50 ~height: 50) in *)
    (* ~text:"M" ~packing:(turing_table #attach ~left:1 ~top:0 ~expand:`NONE) ()) in *)
(*   let input_t1 = ref (GEdit.entry ~width: 50 ~height: 50 ~text:"T" 
    ~editable: false 
      ~packing:(turing_table #attach ~left:1 ~top:0 ~expand:`NONE) ()) in
  let input_t2 = GEdit.entry ~width: 50 ~height: 50
    ~text:"a" ~packing:(turing_table #attach ~left:2 ~top:0 ~expand:`NONE) () in
  let input_t3 = GEdit.entry ~width: 50 ~height: 50
    ~text:"b" ~packing:(turing_table #attach ~left:3 ~top:0 ~expand:`NONE) () in
  let input_t4 = GEdit.entry ~width: 50 ~height: 50
    ~text:"a" ~packing:(turing_table #attach ~left:4 ~top:0 ~expand:`NONE) () in
  let input_t5 = GEdit.entry ~width: 50 ~height: 50
    ~text:"a" ~packing:(turing_table #attach ~left:5 ~top:0 ~expand:`NONE) () in
  let input_t6 = GEdit.entry ~width: 50 ~height: 50
    ~text:"a" ~packing:(turing_table #attach ~left:6 ~top:0 ~expand:`NONE) () in
  let input_t7 = GEdit.entry ~width: 50 ~height: 50
    ~text:"U" ~packing:(turing_table #attach ~left:7 ~top:0 ~expand:`NONE) () in
  let input_t8 = GEdit.entry ~width: 50 ~height: 50
    ~text:"U" ~packing:(turing_table #attach ~left:8 ~top:0 ~expand:`NONE) () in
(*   let box_entry_str = GPack.hbox ~height: 20 ~width: 100 ~spacing: 10 
    ~homogeneous: true ~packing: input_str_frame#add  () in *)
  let box_step = GPack.hbox ~height: 20 ~width: 100 ~spacing: 10  
    ~packing: box_turing#pack () in *)

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
  let table = GPack.table ~rows:rows ~columns:cols ~row_spacings:5 
    ~col_spacings:5 ~packing:scrolled_window#add_with_viewport () in
  table #focus#set_hadjustment (Some scrolled_window # hadjustment);
  table #focus#set_vadjustment (Some scrolled_window # vadjustment);
  let input_turnstile = GButton.toggle_button ~label:("turnstile")
    ~packing:(table #attach ~left: 1 ~top: 0 ~expand: `BOTH) () in
  for m = 2 to cols do
    GButton.toggle_button ~label:("input "^(string_of_int (m-1)))
    ~packing:(table #attach ~left: m ~top: 0 ~expand: `BOTH) ()
  done;
(*   let input_1 = GButton.toggle_button ~label:("input 1")
    ~packing:(table #attach ~left:1 ~top:0 ~expand:`BOTH) () in
  let input_2 = GButton.toggle_button ~label:("input 2")
    ~packing:(table #attach ~left:2 ~top:0 ~expand:`BOTH) () in
  let input_1 = GButton.toggle_button ~label:("input 3")
    ~packing:(table #attach ~left:3 ~top:0 ~expand:`BOTH) () in *)
  for n = 1 to rows do
    GButton.toggle_button ~label:("q_"^(string_of_int (n-1)))
    ~packing:(table #attach ~left: 0 ~top: n ~expand: `BOTH) ()
  done;
(*   let state_1 = GButton.toggle_button ~label:("state 1")
    ~packing:(table #attach ~left:0 ~top:1 ~expand:`BOTH) () in
  let state_2 = GButton.toggle_button ~label:("state 2")
    ~packing:(table #attach ~left:0 ~top:2 ~expand:`BOTH) () in  
  let state_3 = GButton.toggle_button ~label:("state 3")
    ~packing:(table #attach ~left:0 ~top:3 ~expand:`BOTH) () in 
  let state_4 = GButton.toggle_button ~label:("state 4")
    ~packing:(table #attach ~left:0 ~top:4 ~expand:`BOTH) () in
  let state_5 = GButton.toggle_button ~label:("state 5")
    ~packing:(table #attach ~left:0 ~top:5 ~expand:`BOTH) () in
  let state_6 = GButton.toggle_button ~label:("state 6")
    ~packing:(table #attach ~left:0 ~top:6 ~expand:`BOTH) () in 
  let state_7 = GButton.toggle_button ~label:("state 7")
    ~packing:(table #attach ~left:0 ~top:7 ~expand:`BOTH) () in  *)
  for i = 1 to (cols) do
    (* let y = [] *)
    for j=1 to (rows) do
      transition_matrix := 
        (("q_"^(string_of_int (j-1)), "input "^(string_of_int i)),
        (GEdit.entry ~packing:(table #attach ~left:i ~top:j ~expand:`BOTH) ()))
          ::(!transition_matrix)
      (* GButton.toggle_button *)
        (* ~label:("button ("^ string_of_int i ^","^ string_of_int j ^")\n") *)
    done
    (* y@transition_matrix *)
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
    (fun () -> make_matrix box_trans_matrix#pack entry_num_states#text entry_num_input#text ["hello"]) ;

  (* let _ = make_matrix vbox#pack 2 3 ["hello"] in   *)
(*   for i = 1 to 19 do
    for j=1 to 19 do
      GEdit.entry *)
      (* GButton.toggle_button *)
        (* ~label:("button ("^ string_of_int i ^","^ string_of_int j ^")\n") *)
        (* ~packing:(table #attach ~left:i ~top:j ~expand:`BOTH) () *)
(*     done
  done;
 *)
  (*for me to get the entries of each, i can save all of them 
  to a matrix then iterate through the entire matrix to get the values.*)
  let box_turing = GPack.vbox ~packing: vbox#pack () in
  let turing_frame = GBin.frame ~label:"Turing Machine" 
    ~packing:box_turing#pack () in
(*   let table = GPack.table ~rows:20 ~columns:20 ~row_spacings:10
      ~col_spacings:10 ~packing:scrolled_window#add_with_viewport () in *)
  let button_entry_str = GButton.button ~label:"input string" 
    ~packing:box_entry_str#add () in button_entry_str#connect#clicked     
    (fun () -> make_turing_machine turing_frame#add entry_string#text);
(*   let turing_table = GPack.table ~rows: 1 ~columns: 5 ~row_spacings:10
    ~col_spacings:10 ~packing:turing_frame#add () in *)
  (* let text_holder = GPack.hbox ~height: 50 ~width: 50 ~packing: 
    (turing_table #attach ~left:1 ~top: 0 ~expand: `BOTH) () in *)
  (* let a = !input_t1 in *)

  (* let _ = input_t1 := (GEdit.entry ~width: 50 ~height: 50) in *)
    (* ~text:"M" ~packing:(turing_table #attach ~left:1 ~top:0 ~expand:`NONE) ()) in *)
(*   let input_t1 = ref (GEdit.entry ~width: 50 ~height: 50 ~text:"T" 
    ~editable: false 
      ~packing:(turing_table #attach ~left:1 ~top:0 ~expand:`NONE) ()) in
  let input_t2 = GEdit.entry ~width: 50 ~height: 50
    ~text:"a" ~packing:(turing_table #attach ~left:2 ~top:0 ~expand:`NONE) () in
  let input_t3 = GEdit.entry ~width: 50 ~height: 50
    ~text:"b" ~packing:(turing_table #attach ~left:3 ~top:0 ~expand:`NONE) () in
  let input_t4 = GEdit.entry ~width: 50 ~height: 50
    ~text:"a" ~packing:(turing_table #attach ~left:4 ~top:0 ~expand:`NONE) () in
  let input_t5 = GEdit.entry ~width: 50 ~height: 50
    ~text:"a" ~packing:(turing_table #attach ~left:5 ~top:0 ~expand:`NONE) () in
  let input_t6 = GEdit.entry ~width: 50 ~height: 50
    ~text:"a" ~packing:(turing_table #attach ~left:6 ~top:0 ~expand:`NONE) () in
  let input_t7 = GEdit.entry ~width: 50 ~height: 50
    ~text:"U" ~packing:(turing_table #attach ~left:7 ~top:0 ~expand:`NONE) () in
  let input_t8 = GEdit.entry ~width: 50 ~height: 50
    ~text:"U" ~packing:(turing_table #attach ~left:8 ~top:0 ~expand:`NONE) () in *)
(*   let box_entry_str = GPack.hbox ~height: 20 ~width: 100 ~spacing: 10 
    ~homogeneous: true ~packing: input_str_frame#add  () in *)
  let box_step = GPack.hbox ~height: 20 ~width: 100 ~spacing: 10  
    ~packing: box_turing#pack () in

(*     let button_entry_num_states = GButton.button ~label:"input number of states" 
    ~packing:box_entry_num_states#add () in button_entry_num_states#connect#clicked     
    (fun () -> prerr_endline entry_transition#text) ;
 *)
  (*this will be the step button*)
  let button_step = GButton.button ~label: "step forward" ~packing: box_step#pack () in
    button_step#connect#clicked (step ()); (* (fun () -> step ()  prerr_endline "next button pressed" );
 *)(*   (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "Quit" ~key:_Q ~callback: Main.quit;
 *)
  (* Button *)
(*   let button = GButton.button ~label:"Next"
                              ~packing:vbox#add () in
  button#connect#clicked ~callback: (fun () -> prerr_endline "Ouch!");
  let button1 = GButton.button ~label:"Previous"
                              ~packing:vbox#add () in
  button1#connect#clicked ~callback: (fun () -> prerr_endline "Ouch!");
 *)  (* let entry =  *)
  (*call back function to print line*)

  (* Display the windows and enter Gtk+ main loop *)
  (* window#add_accel_group accel_group; *)
  window#show ();
  Main.main ()

let () = main ()