open GMain
open GdkKeysyms

let locale = GtkMain.Main.init ()

let setup packing (make_entry : (GObj.widget -> unit) -> GEdit.entry) =
  let box = GPack.hbox ~packing () in
  let entry = make_entry box#pack in
  let button = GButton.button ~stock:`JUMP_TO ~packing:box#pack () in
  button#connect#clicked
    (fun () -> prerr_endline entry#text) ;
  ()

(*[make_matrix packing rows cols] creates a transition matrix in the GUI*)
let make_matrix packing (r: string) (c: string) (input_alphabet: string list) =
  (*TODO make into a pop-up window*)
  let rows = int_of_string r in
  let cols = int_of_string c in
  let frame = GBin.frame ~label:"Transition Function Matrix" ~packing () in
  let scrolled_window = GBin.scrolled_window ~border_width:10 
    ~hpolicy: `AUTOMATIC ~vpolicy:`AUTOMATIC ~height: 250 
    ~packing: frame#add () in
  let table = GPack.table ~rows:20 ~columns:20 ~row_spacings:10 ~col_spacings:10 
    ~packing:scrolled_window#add_with_viewport () in
  table #focus#set_hadjustment (Some scrolled_window # hadjustment);
  table #focus#set_vadjustment (Some scrolled_window # vadjustment);
  let input_1 = GButton.toggle_button ~label:("input 1")
    ~packing:(table #attach ~left:1 ~top:0 ~expand:`BOTH) () in
  let input_2 = GButton.toggle_button ~label:("input 2")
    ~packing:(table #attach ~left:2 ~top:0 ~expand:`BOTH) () in
  let input_1 = GButton.toggle_button ~label:("input 3")
    ~packing:(table #attach ~left:3 ~top:0 ~expand:`BOTH) () in
  let state_1 = GButton.toggle_button ~label:("state 1")
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
    ~packing:(table #attach ~left:0 ~top:7 ~expand:`BOTH) () in 
  for i = 1 to rows do
    for j=1 to cols do
      GEdit.entry
      (* GButton.toggle_button *)
        (* ~label:("button ("^ string_of_int i ^","^ string_of_int j ^")\n") *)
        ~packing:(table #attach ~left:i ~top:j ~expand:`BOTH) ()
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
  let window = GWindow.window ~height: 520 ~width: 500 
    ~title:"CS 2800 Buddy" () in
  let vbox = GPack.vbox ~spacing: 5 ~packing:window#add () in

  (*frames*)
  let input_str_frame = GBin.frame ~label:"Inputs"
    ~packing:vbox#pack () in
  let input_trans_frame = GBin.frame ~label:"Transition Function"
    ~packing:vbox#pack () in

  (*hboxes to hold entries*)
  let entry_str_box = GPack.hbox ~height: 20 ~width: 100 ~spacing: 10 
    ~homogeneous: true ~packing: input_str_frame#add  () in
  let entry_trans_box = GPack.hbox ~height: 20 ~width: 100 ~spacing: 10 
    ~homogeneous: true ~packing: input_trans_frame#add  () in
  let entry_num_input_box = GPack.hbox ~height: 20 ~width: 100 ~spacing: 10 
    ~homogeneous: true ~packing: vbox#add  () in
  let entry_num_states_box = GPack.hbox ~height: 20 ~width: 100 ~spacing: 10 
    ~homogeneous: true ~packing: vbox#add  () in
  window#connect#destroy ~callback:Main.quit;
  (*keep some mutable record that gets updated every time the button is pressed. 
    i can use eval's mli or something*)

  (*entries*)
  let entry_string = GEdit.entry ~packing:entry_str_box#add () in
  let entry_transition = GEdit.entry ~packing:entry_trans_box#add () in
  let entry_num_input = GEdit.entry ~packing:entry_num_input_box#add () in
  let entry_num_states = GEdit.entry ~packing:entry_num_states_box#add () in

  (*buttons*)
  let entry_str_button = GButton.button ~label:"input string" 
      ~packing:entry_str_box#add () in entry_str_button#connect#clicked     
      (fun () -> prerr_endline entry_string#text);
  let entry_trans_button = GButton.button ~label:"input transition function" 
      ~packing:entry_trans_box#add () in entry_trans_button#connect#clicked     
      (fun () -> prerr_endline entry_transition#text) ;
  let entry_num_input_button = GButton.button ~label:"input number of input alphabet" 
      ~packing:entry_num_input_box#add () in entry_num_input_button#connect#clicked     
      (fun () -> prerr_endline entry_transition#text) ;
  let entry_num_states_button = GButton.button ~label:"input number of states" 
      ~packing:entry_num_states_box#add () in entry_num_states_button#connect#clicked     
      (fun () -> prerr_endline entry_transition#text) ;
  let make_matrix_button = GButton.button ~label:"make matrix" 
      ~packing:vbox#add () in make_matrix_button#connect#clicked     
      (fun () -> make_matrix vbox#pack entry_num_states#text entry_num_input#text ["hello"]) ;


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
  let turing_frame = GBin.frame ~label:"Turing Machine"
      ~packing:vbox#pack () in
(*   let table = GPack.table ~rows:20 ~columns:20 ~row_spacings:10
      ~col_spacings:10 ~packing:scrolled_window#add_with_viewport () in *)
  let turing_table = GPack.table ~rows: 1 ~columns: 5 ~row_spacings:10
      ~col_spacings:10 ~packing:turing_frame#add () in
  (* let text_holder = GPack.hbox ~height: 50 ~width: 50 ~packing: (turing_table #attach ~left:1 ~top: 0 ~expand: `BOTH) () in *)
  let input_t1 = GEdit.entry ~width: 50 ~height: 50
        ~text:"T"
        ~packing:(turing_table #attach ~left:1 ~top:0 ~expand:`NONE) () in
  let input_t2 = GEdit.entry ~width: 50 ~height: 50
        ~text:"a"
        ~packing:(turing_table #attach ~left:2 ~top:0 ~expand:`NONE) () in
  let input_t3 = GEdit.entry ~width: 50 ~height: 50
        ~text:"b"
        ~packing:(turing_table #attach ~left:3 ~top:0 ~expand:`NONE) () in
  let input_t4 = GEdit.entry ~width: 50 ~height: 50
        ~text:"a"
        ~packing:(turing_table #attach ~left:4 ~top:0 ~expand:`NONE) () in
  let input_t5 = GEdit.entry ~width: 50 ~height: 50
        ~text:"a"
        ~packing:(turing_table #attach ~left:5 ~top:0 ~expand:`NONE) () in
  let input_t6 = GEdit.entry ~width: 50 ~height: 50
        ~text:"a"
        ~packing:(turing_table #attach ~left:6 ~top:0 ~expand:`NONE) () in
  let input_t7 = GEdit.entry ~width: 50 ~height: 50
        ~text:"U"
        ~packing:(turing_table #attach ~left:7 ~top:0 ~expand:`NONE) () in
  let input_t8 = GEdit.entry ~width: 50 ~height: 50
        ~text:"U"
        ~packing:(turing_table #attach ~left:8 ~top:0 ~expand:`NONE) () in

(*   (* Menu bar *)
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