(*Types and inputs from Machine : Begin

type state = Q of int

type symbol = Blank | Sigma of int

type machine = {all_states : state list ; prev_state : state ;
               curr_state: state; next_state : state ; alphabet : int list ;
               tape_symbol_read : symbol}

let init = {all_states = [Q 0 ; Q 1] ; prev_state = Q 0;
               curr_state = Q 1; next_state = Q 1 ; alphabet = [0;1];
               tape_symbol_read = Sigma 0}

 End *)

open GMain
open GdkKeysyms
open GObj
open Main_gui


(* Create FSM in initial state *)

(* Filled, black-outlined rectangle. *)
let draw_rectangle (drawable : GDraw.drawable)
    fill_col (x, y) squ_length =

  drawable#set_foreground (`NAME fill_col);
  drawable#rectangle ~x:x ~y:y ~width: squ_length
  ~height: squ_length ~filled:true ();

  drawable#set_foreground `BLACK;
  drawable#rectangle ~x:x ~y:y ~width: squ_length
  ~height: squ_length ~filled:false ()


(* [fsm status] class that inputs a status record and implements the methods
needed to the create a visualization of the FSM. *)
class fsm (status : Main_gui.machine ) ?packing ?show ?width ?height ?packing ?show =

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
    method init_paint () =

      (* Create Overall Ractangle to draw on *)
      let drawable_i = Lazy.force drawable in
      let (width_i, height_i) = drawable_i#size in
      let (float_width_i, float_height_i) =
      (float_of_int(width_i), float_of_int(height_i)) in

      (* Calculate Size of Rectangle *)
      let state_num = List.length status.all_states in
      let tot_state_area = width_i*height_i/2 in
      let state_area = tot_state_area/state_num in
      (* Problem: Need operator - Square-root of state_area *)
      let state_length = state_area |> float_of_int |> sqrt |> int_of_float in

      (* Draw Rectangles on screen - x/y-coordinates + color *)
      let a = int_of_float(0.1*.float_of_int(width_i)) in
      let b =  int_of_float(0.1*.float_of_int(height_i)) in


      for n = 0 to (state_num - 1) do

         (* Still to do - color, (a, b) coordinates *)

        draw_rectangle drawable_i "aqua" (a, b) state_length ;

        (* Draw Text *)
        let float_a = float_of_int(a) in
        let float_b = float_of_int(b) in

        (* New X Y Coordinates *)
        if float_b >= 0.9*.float_width_i then
          let a = int_of_float(0.1*.float_width_i) in
          let b = b + state_length + int_of_float(0.03*.float_height_i) in
          (a, b)

        else
          let a = a + state_length + int_of_float(0.03*.float_width_i) in
          (a, b)

      done






    (* - Draw Rectangle
       - Label Rectangles
       - Position Rectangles on the screen
       - Changing colors
     *)



 end
