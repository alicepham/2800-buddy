
(* Imported libraries *)
open GMain
open GdkKeysyms
open GObj


(* Begin : types from turing machine - should be deleted after integration *)
type state = Q of int

type symbol = Blank | Sigma of int

type direction = Left | Right

type transition_function = (state * string) * (state *string * direction) list

type machine = {all_states : state list ; prev_state : state ;
               curr_state: state; next_state : state ; alphabet : int list ;
               tape_symbol_read : symbol}
(* End *)



let locale = GtkMain.Main.init ()

(* state location type:
 'state' field identifies the state,
 'x_cent' field identifies the location of the x-coordinate of the center of the
square on the drawable object.
 'y_cent' field identifies the location of the y-coordinates of the center of the
square on the drawable object. *)
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

  (* Create and pack a vbox  into the window *)
  let vbox = GPack.vbox ~packing:window#add () in
  window#connect#destroy ~callback:Main.quit;

  (* Machine/Matrix Information - variables currently being fed into paint
  function + can be deleted post-integration. *)

  let init = {all_states = [Q 1 ; Q 3 ; Q 4 ; Q 5; Q 7 ; Q 8 ; Q 9 ; Q 10 ;
  Q 11 ; Q 12 ; Q 22] ;
              prev_state = Q 1;
              curr_state = Q 4;
              next_state = Q 1 ;
              alphabet = [0;1];
              tape_symbol_read = Sigma 0} in

  let matrix = [ ((Q 11, "z"), (Q 12, "h", Left));
                 ((Q 11, "z"), (Q 10, "h", Left)) ] in

  (* Create the drawing area + drawable object *)
  let da = GMisc.drawing_area ~packing:vbox#add () in
  let w = da#misc#realize () ; da#misc#window in
  let drawable_i = new GDraw.drawable w in


  (* [paint machine_state matrix] takes a machine record [machine_state] and a
  list of transition functions [matrix] as inputs. The output is a gui that
  draws each of the states as blue squares. The current state in the state
  machine is a green square, and the previous state is a light purple square.
  The states are numbered in the range 1 - 60. To differentiate between states,
  the states have a specific number of tally marks that correspond to the
  state's number.

  The gui also outputs each of the transition functions in the matrix list. Each
  transition function is shown by a line beginning in the center of the start
  state and ending in the center of the end state. An arrow on the line points
  to the end state of the transition function.
  *)
  let paint machine_state matrix =

      (* Calculate size of drawable area *)
      let (width_i, height_i) = drawable_i#size in
      let (float_width_i, float_height_i) =
      (float_of_int(width_i), float_of_int(height_i)) in

      (* Instantiate list of the state locations *)
      let st_loc_lst = ref [] in


      (* Calculate Size of Rectangle *)
      let state_num = List.length init.all_states in
      let tot_state_area = width_i*height_i/4 in
      let state_area = tot_state_area/state_num in

      (* Square-root of state_area *)
      let state_length = state_area |> float_of_int |> sqrt |> int_of_float in
      let fl_state_length = float_of_int(state_length) in

      (* Instantiate x/y-coordinates to draw state squares at *)
      let a = ref (int_of_float(0.1*.float_width_i)) in
      let b = ref (int_of_float(0.1*.float_height_i)) in

      (* Draw each state as a square. *)
      for n = 0 to (state_num - 1) do

        (* Current State in Loop *)
        let l_state = List.nth init.all_states n in

        (* Add state to State Location List *)
        st_loc_lst := { state = l_state ;
                        x_cent = !a + state_length/2 ;
                        y_cent = !b + state_length/2 } :: !st_loc_lst ;

        (* Draw Rectangle *)
        let () = if l_state = init.curr_state then

        (* Current State *)
        draw_rectangle drawable_i "Spring Green" (!a, !b) state_length

        else if l_state = init.prev_state then

        (* Previous State *)
        draw_rectangle drawable_i "Violet" (!a, !b) state_length

        else

        (* Not Current or Previous State *)
        draw_rectangle drawable_i "Medium Blue" (!a, !b) state_length in


        let float_a = float_of_int(!a) in
        let float_b = float_of_int(!b) in

        (* Labeling states through Tally Marks *)

        (* Number of Tally Marks *)
        let st_num = match l_state with
                   | Q x -> x in

        (* Coordinates of tally marks *)
        let tally_x = ref (float_a +. 0.1*.fl_state_length) in
        let tally_y = ref (float_b +. 0.1*.fl_state_length) in

        (* Draw 'state_num' number of tally marks *)
        for q = 1 to st_num do

           (* Coordinates to draw tally marks *)
           let top_x = !tally_x +. 0.05*.fl_state_length in
           let top_y = !tally_y in
           let bo_y = !tally_y +. 0.18*.fl_state_length in

           (* Diagonal tally marks *)
           let bo_x = if q mod 5 = 0 then
             !tally_x -. 0.17*.fl_state_length

           else
             !tally_x +. 0.05*.fl_state_length

           in


           let int_top_x = int_of_float top_x in
           let int_top_y = int_of_float top_y in
           let int_bo_x = int_of_float bo_x in
           let int_bo_y = int_of_float bo_y in

           (* Draw lines *)
           drawable_i#line ~x: int_top_x ~y: int_top_y
                           ~x: int_bo_x ~y: int_bo_y ;

            (* New line if tally marks overflow. *)
            let () = if (q mod 5 = 0) &&
            ((top_x +. 0.2*.fl_state_length) > (float_a +. fl_state_length))
            then (tally_x := (float_a +. 0.1*.fl_state_length) ;
                  tally_y := (!tally_y +. 0.22*.fl_state_length) ; ())
            else
            (tally_x := top_x ; ())
            in
            ()

      done ;

      let str_sta_x = string_of_float float_a in
      let str_sta_y = string_of_float float_b in

      (* Updating X Y Coordinates for next square *)
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


      (* Transition Functions *)
      for y = 0 to (matrix_num - 1) do


       (* Transition Function to draw *)
       let ((st_state, in_lett),
            (end_state, out_lett, dir)) = List.nth matrix y in

       (* Start State Record *)
       let st_state_loc = !st_loc_lst |> List.filter
         (fun x -> x.state = st_state) |> List.hd in

       (* End State Record *)
       let end_state_loc = !st_loc_lst |> List.filter
         (fun x -> x.state = end_state) |> List.hd in

       (* Start State -> End State Line*)
       let () = if st_state = end_state
        then
        (* Circular Transition Function Line *)
        drawable_i#arc ~x: st_state_loc.x_cent ~y: st_state_loc.y_cent
                       ~width: 10 ~height: 10 ()
        else
        (* Transition Function Line *)
        drawable_i#line ~x: st_state_loc.x_cent ~y: st_state_loc.y_cent
                       ~x: end_state_loc.x_cent ~y: end_state_loc.y_cent
        in


       (* Direction arrow should point based on x-y coordinates. *)
       let dir_x = if end_state_loc.x_cent > st_state_loc.x_cent then 1
                   else if end_state_loc.x_cent = st_state_loc.x_cent then 0
                   else -1
       in

       let dir_y = if end_state_loc.y_cent > st_state_loc.y_cent then 1
                   else if end_state_loc.y_cent = st_state_loc.y_cent then 0
                   else -1
       in

       (* Matching Direction to Polygon matrix - points of triangle based on
       center of the square. *)
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

       (* Draw arrow on top of square *)
       drawable_i#polygon ~filled:true arrow ;

       (* Draw 0/1 above transition function line
       let mid_x = (end_state_loc.x_cent - st_state_loc.x_cent) / 2 +
       st_state_loc.x_cent in
       let mid_y = (end_state_loc.y_cent - st_state_loc.y_cent) / 2 +
       st_state_loc.y_cent in
       let fl_mid_x = float_of_int mid_x in
       let fl_mid_y = float_of_int mid_y in
       let fl_state_length_25 = 0.025*.fl_state_length in
       let int_state_length_25 = int_of_float fl_state_length_25 in
       let fl_state_length_20 = 0.02*.fl_state_length in
       let int_state_length_20 = int_of_float fl_state_length_20 in
       let fl_state_length_200 = 0.2*.fl_state_length in
       let int_state_length_200 = int_of_float fl_state_length_200 in

       let () = match in_lett with
                      | 0 ->  drawable_i#polygon ~filled: false
                                 [(mid_x + int_state_length_200,
                                   mid_y - int_state_length_20) ;
                                  (mid_x - int_state_length_25,
                                   mid_y - int_state_length_20) ;
                                  (mid_x + int_state_length_25,
                                   mid_y - int_state_length_200) ;
                                  (mid_x + int_state_length_25,
                                   mid_y - int_state_length_200)] ; ()
                      | 1 -> (drawable_i#line ~x: mid_x+int_state_length_25
                                             ~y: mid_y - int_state_length_20
                                             ~x: mid_x - int_state_length_25
                                             ~y: mid_y - int_state_length_20;
                             drawable_i#line ~x: mid_x + int_state_length_25
                                             ~y: mid_y - int_state_length_200
                                             ~x: mid_x + int_state_length_25
                                             ~y: mid_y - int_state_length_200 ;
                             drawable_i#line ~x: mid_x
                                             ~y: mid_y -int_state_length_20
                                             ~x: mid_x
                                             ~y: mid_y - int_state_length_20; ())


       in *)

       ()

      done ;

   in

  (* Input information - where integration should take place. *)
  let gui _ = let () = paint init matrix in true in

  (* Display the windows and enter Gtk+ main loop *)
  da#event#connect#expose ~callback: gui ;
  window#show ();
  Main.main ()

(* Run main loop *)
let () = main ()