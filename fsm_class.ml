(*Types and inputs from Machine : Begin *)

type state = Q of int

type symbol = Blank | Sigma of int

type machine = {all_states : state list ; prev_state : state ;
               curr_state: state; next_state : state ; alphabet : int list ;
               tape_symbol_read : symbol}

let init = {all_states = [Q 0 ; Q 1] ; prev_state = Q 0;
               curr_state = Q 1; next_state = Q 1 ; alphabet = [0;1];
               tape_symbol_read = Sigma 0}

(* End *)

open GMain
open GdkKeysyms
open GObj


(* Create FSM in initial state *)

(* [fsm status] class that inputs a status record and implements the methods
needed to the create a visualization of the FSM. *)
class fsm_class status ?packing ?show ?width ?height ?packing ?show =

(* Create the containing vbox. *)
  let vbox = GPack.vbox ?width ?height ?packing ?show () in

(* Create the drawing area. *)
  let da = GMisc.drawing_area ~packing:vbox#add () in
  let drawable = lazy (new GDraw.drawable da#misc#window) in

(* Constants *)

 object (self)
    inherit widget vbox#as_widget

    (*initializer*)
    (* Events: Next Button Event *)


    (* Methods *)


    (* - Draw Rectangle
       - Label Rectangles
       - Position Rectangles on the screen
       - Changing colors
     *)



 end
