(* Main GTK Loop: *)



open GMain
open GdkKeysyms
open Graph


let locale = GtkMain.Main.init ()


(* Main Loop *)
let main () =

  let window = GWindow.window ~width:640 ~height:480
                              ~title:"LablGtk graph widget demo" () in
  let vbox = GPack.vbox ~packing:window#add () in
  window#connect#destroy ~callback:Main.quit;

  (* Create Initial FSM using inputted info - Call to create function + fsm_class *)
  let fsm = new fsm ~packing:vbox#add  in


  (* Create initial tape using inputted info - Call to create function + Alice's widget *)


  (* Step FSM using inputted info - Call to step function + fsm_class *)

  (* Step FSM using inputted info - Call to step function + fsm_class *)

  (* Display the windows and enter Gtk+ main loop *)
  window#show ();
  Main.main ()

in

let () = main ()
