open Graphics

let test_ps = [(1, 1);
               (2, 5);
               (3, 3);
               (5, 3);
               (3, 2);
               (2, 2)]

let ps = ref []

let render_state () =
  clear_graph ();
  List.iter (fun (x, y) -> draw_circle x y 20) !ps

let prev_button_state = ref false

let rec event_loop () =
  let current_button_state = button_down () in
  (match (current_button_state, !prev_button_state) with
   | (true, false) -> ps := mouse_pos () :: !ps;
                      render_state ()
   | (false, true) -> ()
   | _ -> ());
  prev_button_state := current_button_state;
  event_loop ()
