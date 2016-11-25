(* Create FSM in initial state *)

(* [fsm status] class that inputs a status record and implements the methods
needed to the create a visualization of the FSM. *)
class fsm status ?packing ?show =

(* Create the containing vbox. *)
  let vbox = GPack.vbox ?width ?height ?packing ?show () in

(* Create the drawing area. *)
  let da = GMisc.drawing_area ~packing:vbox#add () in
  let drawable = lazy (new GDraw.drawable da#misc#window) in

(* Constants *)

 object (self)
    inherit widget vbox#as_widget

    initializer

    (* Events: Next Button Event *)

    (* Methods *)

    (* - Draw Rectangle
       - Label Rectangles
       - Position Rectangles on the screen
       -
     *)
 end
