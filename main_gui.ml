(* Main GTK Loop: *)



open GMain
open GdkKeysyms
open GObj
open Fsm_class
(* open Machine *)

(*Types and inputs from Machine : Begin *)

type state = Q of int

type symbol = Blank | Sigma of int

type machine = {all_states : state list ; prev_state : state ;
               curr_state: state; next_state : state ; alphabet : int list ;
               tape_symbol_read : symbol}
(* End *)



let locale = GtkMain.Main.init ()


(* Main Loop *)
let main () =

  (* Create GUI window *)
  let window = GWindow.window ~width:640 ~height:480
                              ~title:"Turing Machine Visualization" () in
  let vbox = GPack.vbox ~packing:window#add () in
  window#connect#destroy ~callback:Main.quit;

  (* Initial Machine Info
  let init = Machine.init info in *)

  let init = {all_states = [Q 0 ; Q 1] ; prev_state = Q 0;
               curr_state = Q 1; next_state = Q 1 ; alphabet = [0;1];
               tape_symbol_read = Sigma 0} in

  (* Create Initial FSM using init *)
  let fsm = new fsm init ~packing:vbox#add in


  (* Create initial tape using inputted info Alice's widget *)


  (* Step FSM using inputted info - Call to step function + fsm_class *)

  (* Step FSM using inputted info - Call to step function + fsm_class *)

  (* Display the windows and enter Gtk+ main loop *)
  window#show ();
  Main.main ()



let () = main ()
