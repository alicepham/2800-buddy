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

  let init = {all_states = [Q 1 ; Q 3 ; Q 4 ; Q 5; Q 7 ; Q 8 ; Q 9 ; Q 10 ;
  Q 11 ; Q 12 ; Q 22] ; prev_state = Q 1;
               curr_state = Q 4; next_state = Q 1 ; alphabet = [0;1];
               tape_symbol_read = Sigma 0} in
  let matrix = [ ((Q 11, "z"), (Q 12, "h", Left));
                 ((Q 11, "z"), (Q 10, "h", Left));
                  ] in
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
      let fl_state_length = float_of_int(state_length) in

      (* Draw Rectangles on screen - x/y-coordinates + color *)
      let a = ref (int_of_float(0.1*.float_width_i)) in
      let b = ref (int_of_float(0.1*.float_height_i)) in

      (* *)
      for n = 0 to (state_num - 1) do

        (* Current State in Loop *)
        let l_state = List.nth init.all_states n in

        (* State Location List *)
        st_loc_lst := { state = l_state ;
                        x_cent = !a + state_length/2 ;
                        y_cent = !b + state_length/2 } :: !st_loc_lst ;

        (* Draw Rectangle *)
        let () = if l_state = init.curr_state then

        draw_rectangle drawable_i "Spring Green" (!a, !b) state_length

        else if l_state = init.prev_state then

        draw_rectangle drawable_i "Violet" (!a, !b) state_length

        else

        draw_rectangle drawable_i "Medium Blue" (!a, !b) state_length in


        let float_a = float_of_int(!a) in
        let float_b = float_of_int(!b) in

      (* Tally Marks *)
        let st_num = match l_state with
                   | Q x -> x in
        let tally_x = ref (float_a +. 0.1*.fl_state_length) in
        let tally_y = ref (float_b +. 0.1*.fl_state_length) in

        for q = 1 to st_num do

           let top_x = !tally_x +. 0.05*.fl_state_length in
           let top_y = !tally_y in
           let bo_y = !tally_y +. 0.18*.fl_state_length in
           let bo_x = if q mod 5 = 0 then
             !tally_x -. 0.17*.fl_state_length

           else
             !tally_x +. 0.05*.fl_state_length

           in

           let int_top_x = int_of_float top_x in
           let int_top_y = int_of_float top_y in
           let int_bo_x = int_of_float bo_x in
           let int_bo_y = int_of_float bo_y in

           drawable_i#line ~x: int_top_x ~y: int_top_y
                           ~x: int_bo_x ~y: int_bo_y ;

       let () = if (q mod 5 = 0) &&
          ((top_x +. 0.2*.fl_state_length) > (float_a +. fl_state_length)) then
          (tally_x := (float_a +. 0.1*.fl_state_length) ;
          tally_y := (!tally_y +. 0.22*.fl_state_length) ; ())
       else
          (tally_x := top_x ; ())
       in
       ()

      done ;

      let str_sta_x = string_of_float float_a in
      let str_sta_y = string_of_float float_b in

      (* New X Y Coordinates *)
      let (imut_a,imut_b) =

        if float_a >= 0.7*.float_width_i || (!a + state_length) >= 640 then

          (int_of_float(0.1*.float_width_i),
            !b + state_length + int_of_float(0.03*.float_height_i) )

        else

          (!a + state_length + int_of_float(0.03*.float_width_i), !b)
        in
        a := imut_a ;
        b := imut_b ;


        ()

      done ;

      let matrix_num = List.length matrix in


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

       let dir_x = if end_state_loc.x_cent > st_state_loc.x_cent then 1
                   else if end_state_loc.x_cent = st_state_loc.x_cent then 0
                   else -1
       in

       let dir_y = if end_state_loc.y_cent > st_state_loc.y_cent then 1
                   else if end_state_loc.y_cent = st_state_loc.y_cent then 0
                   else -1
       in

       let arrow = match (dir_x, dir_y) with
                   | (1, 1) ->
                    [(end_state_loc.x_cent + 8, end_state_loc.y_cent + 8);
                     (end_state_loc.x_cent - 5, end_state_loc.y_cent + 5);
                     (end_state_loc.x_cent + 5, end_state_loc.y_cent - 5)]
                   | (0, 1) ->
                    [(end_state_loc.x_cent, end_state_loc.y_cent + 8);
                     (end_state_loc.x_cent - 5, end_state_loc.y_cent);
                     (end_state_loc.x_cent + 5, end_state_loc.y_cent)]
                   | (-1, 1) ->
                    [(end_state_loc.x_cent - 8, end_state_loc.y_cent + 8);
                     (end_state_loc.x_cent - 5, end_state_loc.y_cent - 5);
                     (end_state_loc.x_cent + 5, end_state_loc.y_cent + 5)]
                   | (-1, 0) ->
                    [(end_state_loc.x_cent - 8, end_state_loc.y_cent);
                     (end_state_loc.x_cent, end_state_loc.y_cent - 5);
                     (end_state_loc.x_cent, end_state_loc.y_cent + 5)]
                   | (-1, -1) ->
                    [(end_state_loc.x_cent - 8, end_state_loc.y_cent - 8);
                     (end_state_loc.x_cent - 5, end_state_loc.y_cent + 5);
                     (end_state_loc.x_cent + 5, end_state_loc.y_cent - 5)]
                   | (0, -1) ->
                    [(end_state_loc.x_cent, end_state_loc.y_cent - 8);
                     (end_state_loc.x_cent - 5, end_state_loc.y_cent);
                     (end_state_loc.x_cent + 5, end_state_loc.y_cent)]
                   | (1, -1) ->
                    [(end_state_loc.x_cent + 8, end_state_loc.y_cent - 8);
                     (end_state_loc.x_cent - 5, end_state_loc.y_cent - 5);
                     (end_state_loc.x_cent + 5, end_state_loc.y_cent + 5)]
                   | (1, 0) ->
                    [(end_state_loc.x_cent + 8, end_state_loc.y_cent);
                     (end_state_loc.x_cent, end_state_loc.y_cent + 5);
                     (end_state_loc.x_cent, end_state_loc.y_cent - 5)]
                   | (0, 0) ->
                    [(end_state_loc.x_cent + 8, end_state_loc.y_cent);
                     (end_state_loc.x_cent, end_state_loc.y_cent + 5);
                     (end_state_loc.x_cent, end_state_loc.y_cent - 5)]

       in

       drawable_i#polygon ~filled:true arrow ;

       ()

      done ;




   in

  let gui _ = let () = paint init matrix in true in

(* Display the windows and enter Gtk+ main loop *)
  da#event#connect#expose ~callback: gui ;
  window#show ();
  Main.main ()

let () = main ()