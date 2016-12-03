open GMain
open GdkKeysyms
(* open Machine *)

let locale = GtkMain.Main.init ()

(* How to Run
 * ocamlfind ocamlc -g -package lablgtk2 -linkpkg gui.ml -o gui
 * ./gui*)

(************************Turing Machine Helpers********************************)

type value = {state: string; input: string; dir: string}

(*initializing states*)
let current_state = ref "q_0"
let current_input = ref "["

(* [curr_turing_machine_index] is a global variable that keeps track of the 
 * index of where you are in the tape*)
let curr_turing_machine_index = ref 0

(* [turing_machine] is a global variable to hold all GUI 
 * tape cells in the tape of a turing machine
 *)
let (turing_machine: (GEdit.entry ref) list ref) = ref []

(* [transition matrix] is a global variable to hold an association 
 * dictionary of inputs into the transition matrix from the GUI*)
let (transition_matrix: ((string * string) * GEdit.entry) list ref) = ref []

let setup packing (make_entry : (GObj.widget -> unit) -> GEdit.entry) =
  let box = GPack.hbox ~packing () in
  let entry = make_entry box#pack in
  let button = GButton.button ~stock:`JUMP_TO ~packing:box#pack () in
    button#connect#clicked (fun () -> prerr_endline entry#text) ; ()
    
let make_str_input packing (make_entry : (GObj.widget -> unit) -> GEdit.entry) =
  let box = GPack.hbox ~packing () in
  let entry = make_entry box#pack in
  let button = GButton.button ~stock:`JUMP_TO ~packing:box#pack () in
    button#connect#clicked (fun () -> prerr_endline entry#text) ; ()

(* [string_to_list s] is a helper function that seperates a string 
 * into a string list of the individual characters in that string.
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
 * It accesses global variable curr_turing_machine_index, transition_matrix, 
 * turing_machine, current_state, current_input. 
 * It then does the following:
 * 1) parses the inputs of the GEdit.entry object in the transition matrix and 
 *    creates a dictionary of keys and records
 * 2) lookup in transition dictionary for corresponding record given 
 *    current state and input: {next_state, new_char, direction}
 * 3) get the current tape cell in the GUI (from variable turing_machine)
 *    a) 'deselect' the cell
 *    b) updates the char in the current_tape_cell to new_char
 * 4) given direction, get the next tape cell in the GUI (from turing_machine)
 *    a) 'selects' the cell
 * 5) updates the 3 current values to the next values:
 *    a) curr_turing_machine_index is changed given the direction
 *    b) current_state is changed give the next_state
 *    c) current_input is changed to the character in the next tape cell
 *)
let step packing () =
  let trans_dictionary = 
    List.map (fun ((q,i),a) -> ((q,i),(parse a#text))) !transition_matrix in

  let get_value = 
    List.assoc (!current_state, !current_input) trans_dictionary in

  let new_state = get_value.state in
  let new_char = get_value.input in
  let dir = get_value.dir in

  let curr_entry = List.nth !turing_machine !curr_turing_machine_index in
  let _ = (!curr_entry)#set_has_frame false in
  let _ = (!curr_entry)#set_text new_char in

  let next_turing_machine_index = 
    (match dir with
    | "R" -> !curr_turing_machine_index + 1 
    | "L" -> !curr_turing_machine_index - 1 
    | _ -> raise (Failure "step - not a valid direction")) in

  let next_entry = List.nth !turing_machine next_turing_machine_index in
  let _ = (!next_entry)#set_has_frame true in

  let _ = curr_turing_machine_index := next_turing_machine_index in
  let _ = current_state := new_state in
  let _ = current_input := (!next_entry)#text in

  (*debugging purposes*)
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
    ref (GEdit.entry 
      ~width: 50 ~height: 50 ~has_frame: true ~text:"[" ~editable: false
      ~packing:(turing_table #attach ~left:1 ~top:0 ~expand:`NONE) ()) in
  let _ = turing_machine := turnstile::!turing_machine in

  (*creates entries based on text input*)
  for i = 0 to ((String.length s)-1) do
    turing_machine := ref(GEdit.entry ~width: 50 ~height: 50 ~has_frame: false
      ~editable: false ~text:(Char.escaped (String.get s i)) 
      ~packing:(turing_table #attach 
        ~left:(i+2) ~top:0 ~expand:`NONE) ())::!turing_machine
  done;
  let empty = ref (GEdit.entry ~width: 50 ~height: 50 ~has_frame: false
    ~editable: false ~text:"_" 
    ~packing:(turing_table #attach ~left:((String.length s)+2) 
      ~top:0 ~expand:`NONE) ()) in
  let _ = turing_machine := empty::!turing_machine in
  let _ = turing_machine := List.rev !turing_machine in
  ()

(* [make_matrix packing rows cols] creates a transition matrix in the GUI
 * after a button is pressed -- labeled "make matrix"*)
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

  (*creating state labels in transition matrix*)
  for n = 1 to rows do
    GButton.toggle_button ~label:("q_"^(string_of_int (n-1)))
    ~packing:(table #attach ~left: 0 ~top: n ~expand: `BOTH) ()
  done;(* Char.chr(i + 97) *)
  let input_turnstile = GButton.toggle_button ~label:("u")
    ~packing:(table #attach ~left: (cols+2) ~top: 0 ~expand: `BOTH) () in

  (*adding GEdit.entries objects to the global transition_matrix to be 
   *editted later*)
  for i = 1 to (cols+2) do
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

(******************************************************************************)

(*******************************RSA Helpers************************************)
let rec make_step_one_encrypt packing = 
  let frame_step_one = GBin.frame ~label: "Step 1" ~height: 60
    ~packing () in
  let vbox_step_one = GPack.vbox ~packing: (frame_step_one#add) () in
  let a = GEdit.entry ~packing: vbox_step_one#pack () in
  let next_button = GButton.button ~label: "next step" 
    ~packing: vbox_step_one#pack () in
    next_button#connect#clicked
    ~callback: (fun () -> make_step_one_encrypt packing); ()

(******************************************************************************)

let main () =
  let window = GWindow.window ~height: 600 ~width: 500 
    ~title:"CS 2800 Buddy" () in
  let notebook = GPack.notebook ~show_tabs: true ~packing:window#add () in

  (*page 1 - welcome page*)
  let vbox_welcome = GPack.vbox ~spacing: 5
    ~packing:(fun w -> ignore (notebook#append_page w)) () in
  let welcome = GEdit.entry ~height: 100 ~width: 100 ~editable: false
    ~text: 
    "Welcome to CS 2800 buddy! Your help for all things Discrete Structures. :)"
      ~packing:vbox_welcome#pack () in
  let hbox_turing_button = GPack.hbox ~height: 30 ~width: 300 
    ~packing: (vbox_welcome#pack ~padding: 5 ~expand: false) () in

  (*page 2 - turing machine page*)
  let vbox_turing   = GPack.vbox ~spacing: 5 
    ~packing:(fun w -> ignore (notebook#append_page w)) () in
  let turing_button_page = GButton.button ~label: "go to turing machine!" 
    ~packing:(hbox_turing_button#add) () in
      turing_button_page#connect#clicked 
      ~callback: (fun () -> (notebook#goto_page 1));
  let rsa_button_page = GButton.button ~label: "go to RSA simulation!" 
    ~packing:(hbox_turing_button#add) () in
      rsa_button_page#connect#clicked 
      ~callback: (fun () -> (notebook#goto_page 2));

  (*turing machine page - frames*)
  let input_str_frame = GBin.frame ~label:"Inputs"
    ~packing:vbox_turing#pack () in
  let vbox_inputs = GPack.vbox ~height: 100 ~width: 100 ~spacing: 10 
    ~homogeneous: true ~packing: input_str_frame#add () in

  (*turing machine page - hboxes to hold entries*)
  let box_entry_str        = GPack.hbox ~height: 20 ~width: 100 ~spacing: 10 
    ~homogeneous: true ~packing: vbox_inputs#add    () in
(*   let box_entry_num_input  = GPack.hbox ~height: 20 ~width: 100 ~spacing: 10 
    ~homogeneous: true ~packing: vbox_inputs#add        () in *)
  let box_entry_num_states = GPack.hbox ~height: 20 ~width: 100 ~spacing: 10 
    ~homogeneous: true ~packing: vbox_inputs#add        () in
  let box_make_matrix      = GPack.hbox ~height: 20 ~width: 100 ~spacing: 10 
    ~homogeneous: true ~packing: vbox_inputs#add        () in
  let box_trans_matrix = GPack.vbox ~border_width: 5 
    ~packing: vbox_turing#add () in window#connect#destroy ~callback:Main.quit;

  (*turing machine page - entries*)
  let entry_string = GEdit.entry ~packing:box_entry_str#add () in
  (* let entry_transition = GEdit.entry ~packing:box_entry_trans#add () in *)
  (* let entry_num_input = GEdit.entry ~packing:box_entry_num_input#add () in *)
  let entry_num_states = GEdit.entry ~packing:box_entry_num_states#add () in

  (*turing machine page - buttons*)
  (*TODO make the formatting of this better looking*)
(*   let button_entry_num_input = GButton.button 
    ~label:"input number of input alphabet" ~packing:box_entry_num_input#add () 
    in button_entry_num_input#connect#clicked     
    (fun () -> prerr_endline entry_num_input#text); *)
  let button_entry_num_states = GButton.button 
    ~label:"input number of states"         ~packing:box_entry_num_states#add () 
    in button_entry_num_states#connect#clicked     
    (fun () -> prerr_endline entry_num_states#text);
  let button_make_matrix = GButton.button 
    ~label:"make matrix"                    ~packing:box_make_matrix#add      () 
    in button_make_matrix#connect#clicked     
    (fun () -> make_matrix box_trans_matrix#pack entry_num_states#text 
      "2" ["hello"]) ;

  (*turing machine page - turing machine box*)
  let box_turing = GPack.vbox ~packing: vbox_turing#pack () in
  let turing_frame = GBin.frame ~label:"Turing Machine" 
    ~packing:box_turing#pack () in
  let turing_scroll_window = GBin.scrolled_window ~border_width:10 
    ~hpolicy: `AUTOMATIC ~vpolicy:`AUTOMATIC ~height: 180
      ~packing: turing_frame#add () in

  let button_entry_str = GButton.button ~label:"input string" 
    ~packing:box_entry_str#add () in button_entry_str#connect#clicked     
    (fun () -> make_turing_machine 
     turing_scroll_window#add_with_viewport entry_string#text);
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

  (*page 3 - RSA page*)
  let vbox_rsa_page = GPack.vbox ~spacing: 5
    ~packing:(fun w -> ignore (notebook#append_page w)) () in

  (*RSA page - input message*)
  let frame_rsa_message = GBin.frame ~label: "Input RSA message" ~height: 60
    ~packing: (vbox_rsa_page#pack ~expand: false) () in

  (*RSA page - input p and q*)
  let hbox_p_q = GPack.hbox ~height: 40
    ~packing: (vbox_rsa_page#pack ~expand: false) () in
  let entry_rsa_message = GEdit.entry ~height: 30 
    ~packing: frame_rsa_message#add () in
  let frame_p = GBin.frame ~label: "p" ~height: 40
    ~packing: (hbox_p_q#pack ~expand: false) () in
  let entry_p = GEdit.entry ~width: 30 ~packing: frame_p#add () in
  let frame_q = GBin.frame ~label: "q" ~height: 40
    ~packing: (hbox_p_q#pack ~expand: false) () in
  let entry_q = GEdit.entry ~width: 30 ~packing: frame_q#add () in

  let encrypt_button = GButton.button ~label: "encrypt message" 
    ~packing: (hbox_p_q#pack ~expand: false ~fill: true) () in
    encrypt_button#connect#clicked
    ~callback: (fun () -> make_step_one_encrypt 
      (vbox_rsa_page#pack ~expand: false));

  (*GUI notes*)
  (* -- You can access the text in an entry by doing say, 
   *     entry_rsa_message#text
   * -- Here is an example of having a button do something on the callback:
   *      let button_entry_str = GButton.button ~label:"input string" 
   *         ~packing:box_entry_str#add () in button_entry_str#connect#clicked     
   *         (fun () -> make_turing_machine 
   *         turing_scroll_window#add_with_viewport entry_string#text);
   *)

  window#show ();
  Main.main ()

let () = main ()