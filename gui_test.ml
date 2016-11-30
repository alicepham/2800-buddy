open GMain
open GdkKeysyms
open GObj



type state = Q of int

type symbol = Blank | Sigma of int

type direction = Left | Right

type transition_function = (state * string) * (state *string * direction) list

type machine = {all_states : state list ; prev_state : state ;
               curr_state: state; next_state : state ; alphabet : int list ;
               tape_symbol_read : symbol}
(* End *)



let locale = GtkMain.Main.init ()

type state_location = { state : state ; x_cent : int ; y_cent : int }

(* Create FSM in initial state *)
let font_name = "-arabic-newspaper-medium-*-*-*-*-120-*-*-*-*-*-*"

let font =
  try
    Gdk.Font.load font_name
  with
    Gpointer.Null -> failwith ("font " ^ font_name ^ ": not found")


(* Draw Text on drawable object *)
let draw_text drawable (x, y) font text =
  let string_width = Gdk.Font.string_width font text in
  let string_height = Gdk.Font.string_height font text in
  let () = Pervasives.print_string "draw text" in
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

  let init = {all_states = [Q 0 ; Q 1 ; Q 3 ; Q 4 ; Q 5; Q 6; Q 7 ; Q 8 ; Q 9 ; Q 10 ; Q 11] ; prev_state = Q 0;
               curr_state = Q 4; next_state = Q 1 ; alphabet = [0;1];
               tape_symbol_read = Sigma 0} in
  let matrix = [((Q 1, "a"), (Q 0, "b", Left)) ;
                ((Q 1, "b"), (Q 1, "b", Left)) ;
                ((Q 3, "c"), (Q 5, "z", Right)) ;
                ((Q 0, "h"), (Q 6, "h", Right)) ;
                ((Q 0, "a"), (Q 11, "h", Left)) ;
                ((Q 0, "z"), (Q 7, "h", Left)) ;
                ((Q 8, "z"), (Q 0, "h", Left))] in
  (* Create the containing vbox. *)


(* Create the drawing area. *)
  let da = GMisc.drawing_area ~packing:vbox#add () in
  let w = da#misc#realize () ; da#misc#window in
  let drawable_i = new GDraw.drawable w in

  let paint machine_state matrix =

      let (width_i, height_i) = drawable_i#size in
      let (float_width_i, float_height_i) =
      (float_of_int(width_i), float_of_int(height_i)) in
      let st_loc_lst = ref [] in

      (* Calculate Size of Rectangle *)
      let state_num = List.length init.all_states in
      let tot_state_area = width_i*height_i/4 in
      let state_area = tot_state_area/state_num in
      (* Square-root of state_area *)
      let state_length = state_area |> float_of_int |> sqrt |> int_of_float in

      (* Draw Rectangles on screen - x/y-coordinates + color *)
      let a = ref (int_of_float(0.1*.float_width_i)) in
      let b = ref (int_of_float(0.1*.float_height_i)) in


      for n = 0 to (state_num - 1) do

        (* Current State in Loop *)
        let l_state = List.nth init.all_states n in

        (* State Location List *)
        st_loc_lst := { state = l_state ;
                        x_cent = !a + state_length/2 ;
                        y_cent = !b + state_length/2 } :: !st_loc_lst ;
        let l_string_state = match l_state with
                             | Q x -> "Q" ^ string_of_int(x) in

        (* Draw Rectangle *)
        let () = if l_state = init.curr_state then

        draw_rectangle drawable_i "Spring Green" (!a, !b) state_length

        else if l_state = init.prev_state then

        draw_rectangle drawable_i "Violet" (!a, !b) state_length

        else

        draw_rectangle drawable_i "Medium Blue" (!a, !b) state_length in

        (* Draw Text
        draw_text drawable_i (a, b) l_string_state ;
        *)



        let float_a = float_of_int(!a) in
        let float_b = float_of_int(!b) in

        let str_sta_x = string_of_float float_a in
       let str_sta_y = string_of_float float_b in
       let () = Pervasives.print_string ("x : "^str_sta_x) in
       let () = Pervasives.print_string ("y : "^str_sta_y) in


        (* New X Y Coordinates *)
        let (imut_a,imut_b) =

        if float_a >= 0.7*.float_width_i then

          (int_of_float(0.1*.float_width_i),
            !b + state_length + int_of_float(0.03*.float_height_i) )

        else
          let () = Pervasives.print_string "new line" in
          (!a + state_length + int_of_float(0.03*.float_width_i), !b)
        in
        a := imut_a ;
        b := imut_b ;


        ()

      done ;

      let matrix_num = List.length matrix in

      let string_test = draw_text drawable_i (70, 70) font "test" in

      for y = 0 to (matrix_num - 1) do


       let ((st_state, in_lett),
            (end_state, out_lett, dir)) = List.nth matrix y in

       let st_state_loc = !st_loc_lst |> List.filter
         (fun x -> x.state = st_state) |> List.hd in

       let end_state_loc = !st_loc_lst |> List.filter
         (fun x -> x.state = end_state) |> List.hd in



       let () = if st_state = end_state
        then
        drawable_i#arc ~x: st_state_loc.x_cent ~y: st_state_loc.y_cent
                       ~width: 10 ~height: 10 ()
        else
        drawable_i#line ~x: st_state_loc.x_cent ~y: st_state_loc.y_cent
                       ~x: end_state_loc.x_cent ~y: end_state_loc.y_cent
        in

       drawable_i#polygon ~filled: true
       [(end_state_loc.x_cent - 8, end_state_loc.y_cent + 5) ;
        (end_state_loc.x_cent + 8, end_state_loc.y_cent + 5) ;
        (end_state_loc.x_cent, end_state_loc.y_cent - 5)] ;

       ()

      done ;




   in

  let gui _ = let () = paint init matrix in true in

  (* Button - Stepping Part
  let button = GButton.button ~label:"Next Step"
                              ~packing: vbox#add () in *)
  (* Step
  button#connect#clicked ~callback: (fun () -> paint (step) ); *)


(* Display the windows and enter Gtk+ main loop *)
  da#event#connect#expose ~callback: gui ;
  window#show ();
  Main.main ()

let () = main ()