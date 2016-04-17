open Graphics
open Convexhull

let ps = ref []

let render_state () =
  clear_graph ();
  let ch = convex_hull !ps in
  List.iter (fun (x, y) -> draw_circle x y 10) !ps;
  draw_poly @@ Array.of_list ch

let prev_button_state = ref false

let is_mouse_pos_ok (pos_x, pos_y) =
  0 <= pos_x && pos_x < size_x () && 0 <= pos_y && pos_y < size_y ()

let rec handle_mouse () =
  let current_button_state = button_down () in
  (match (current_button_state, !prev_button_state) with
   | (true, false) -> let mp = mouse_pos () in
                      if is_mouse_pos_ok mp
                      then ps := mp :: !ps; render_state ()
   | (false, true) -> ()
   | _ -> ());
  prev_button_state := current_button_state

let rec handle_keyboard () =
  if key_pressed () then (if read_key () == ' ' then ps := []; render_state ())

let rec event_loop () =
  handle_mouse ();
  handle_keyboard ();
  event_loop ()

let _ =
  open_graph "";
  resize_window 800 400;;
  event_loop ();
  close_graph ()
