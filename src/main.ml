open Graphics
open ConvexHull
open BatOption.Infix

type point = int * int

let circle_radious = 10

let ps: (point ref) list ref = ref []
let dragging_point: point ref option ref = ref None
let exit_program = ref false

let render_state () =
  let pure_ps = List.map (!) !ps in
  let ch = convex_hull pure_ps in
  clear_graph ();
  List.iter (fun (x, y) -> draw_circle x y circle_radious;
                           moveto (x + circle_radious) (y + circle_radious);
                           draw_string @@ String.concat "," [(string_of_int x); (string_of_int y)])
            pure_ps;
  draw_poly @@ Array.of_list ch

let prev_button_state = ref false

let is_mouse_pos_ok (pos_x, pos_y : int * int) : bool =
  0 <= pos_x && pos_x < size_x () && 0 <= pos_y && pos_y < size_y ()

let point_on_circle (px, py: point) (r: int) (c: point ref): bool =
  let (cx, cy)  = !c in
  let dx = float_of_int (px - cx) in
  let dy = float_of_int (py - cy) in
  let d = sqrt (dx *. dx +. dy *. dy) in
  d < float_of_int r

let clicked_on_point (mp: point): point ref option =
  try
    Some (List.find (point_on_circle mp circle_radious) !ps)
  with
  | Not_found -> None

let rec handle_mouse (): unit =
  let mp = mouse_pos () in
  let current_button_state = button_down () in
  dragging_point := !dragging_point >>= (fun rp -> rp := mp; Some rp);
  (match current_button_state, !prev_button_state with
   | true, false -> if is_mouse_pos_ok mp
                    then let clicked_point = clicked_on_point mp
                         in (match clicked_point with
                             | Some rp -> dragging_point := clicked_point
                             | None -> let new_point = ref mp in
                                       (ps := new_point :: !ps); dragging_point := Some new_point)
   | false, true -> dragging_point := None
   | _ -> ());
  prev_button_state := current_button_state

let rec handle_keyboard () =
  if key_pressed () then
    (match read_key () with
     | ' ' -> ps := []
     | 'q' -> exit_program := true
     | _ -> ())

let rec event_loop () =
  handle_mouse ();
  handle_keyboard ();
  render_state ();
  synchronize ();
  if not !exit_program then
    event_loop ()

let _ =
  open_graph "";
  auto_synchronize false;
  resize_window 800 600;;
  event_loop ();
  close_graph ()
