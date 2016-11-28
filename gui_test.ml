open GMain
open GdkKeysyms
open GObj



type state = Q of int

type symbol = Blank | Sigma of int

type machine = {all_states : state list ; prev_state : state ;
               curr_state: state; next_state : state ; alphabet : int list ;
               tape_symbol_read : symbol}
(* End *)



let locale = GtkMain.Main.init ()


(* Create FSM in initial state
let font_name = "-*-helvetica-*--120-*"

let font =
  try
    Gdk.Font.load font_name
  with
    Gpointer.Null -> failwith ("font " ^ font_name ^ ": not found")
*)

(* Draw Text on drawable object *)
let draw_text drawable (x, y) font text =
  let string_width = Gdk.Font.string_width font text in
  let string_height = Gdk.Font.string_height font text in
  drawable#string text ~font ~x:(x - string_width/2) ~y:(y+string_height)



(* Filled, black-outlined rectangle. *)
let draw_rectangle (drawable : GDraw.drawable)
    fill_col (x, y) squ_length =

  drawable#set_foreground (`NAME fill_col);
  drawable#rectangle ~x:x ~y:y ~width: squ_length
  ~height: squ_length ~filled:true ();

  drawable#set_foreground `BLACK;
  drawable#rectangle ~x:x ~y:y ~width: squ_length
  ~height: squ_length ~filled:false ()



(* Main Loop *)
let main () =

  (* Create GUI window *)
  let window =  GWindow.window ~width:640 ~height:480
                              ~title:"Turing Machine Visualization" () in


  let vbox = GPack.vbox ~packing:window#add () in
  window#connect#destroy ~callback:Main.quit;

  (* Initial Machine Info
  let init = Machine.init info in *)

  let init = {all_states = [Q 0 ; Q 1] ; prev_state = Q 0;
               curr_state = Q 1; next_state = Q 1 ; alphabet = [0;1];
               tape_symbol_read = Sigma 0} in
  (* Create the containing vbox. *)


(* Create the drawing area. *)
  let da = GMisc.drawing_area ~packing:vbox#add () in
  let w = da#misc#realize () ; da#misc#window in
  let drawable_i = new GDraw.drawable w in

  let paint machine_state =
      let (width_i, height_i) = drawable_i#size in
      let (float_width_i, float_height_i) =
      (float_of_int(width_i), float_of_int(height_i)) in

      (* Calculate Size of Rectangle *)
      let state_num = List.length init.all_states in
      let tot_state_area = width_i*height_i/2 in
      let state_area = tot_state_area/state_num in
      (* Problem: Need operator - Square-root of state_area *)
      let state_length = state_area |> float_of_int |> sqrt |> int_of_float in

      (* Draw Rectangles on screen - x/y-coordinates + color *)
      let a = int_of_float(0.1*.float_width_i) in
      let b =  int_of_float(0.1*.float_height_i) in


      for n = 0 to (state_num - 1) do


        let () = Pervasives.print_string "Line 98" in
        (* Current State in Loop *)
        let l_state = List.nth init.all_states n in
        let l_string_state = match l_state with
                             | Q x -> "Q" ^ string_of_int(x) in

        (* Draw Rectangle *)
        let () = if l_state = init.curr_state then
        draw_rectangle drawable_i "Spring Green" (a, b) state_length
        else
        draw_rectangle drawable_i "Medium Blue" (a, b) state_length in

        (* Draw Text
        draw_text drawable_i (a, b) l_string_state ;
        *)

        let float_a = float_of_int(a) in
        let float_b = float_of_int(b) in

        (* New X Y Coordinates *)
        let (a,b) =

        if float_b >= 0.9*.float_width_i then
          (int_of_float(0.1*.float_width_i),
            b + state_length + int_of_float(0.03*.float_height_i) )

        else
          (a + state_length + int_of_float(0.03*.float_width_i), b)
        in
        ()

      done ;
   in

  let gui = paint init in

   (* Button - Stepping Part *)
  let button = GButton.button ~label:"Next Step"
                              ~packing:vbox#add () in
  (* Step
  button#connect#clicked ~callback: (fun () -> paint (step) ); *)


(* Display the windows and enter Gtk+ main loop *)
  window#show ();
  Main.main ()

let () = main ()