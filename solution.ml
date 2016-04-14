
let test_ps = [(1, 1);
               (2, 5);
               (3, 3);
               (5, 3);
               (3, 2);
               (2, 2)]

(* let (|>) x f = f x *)

let find_start_point ps = 
  List.fold_left min (max_int, max_int) ps
  
let angle_sort (sx, sy) ps = 
  let vs = List.map (fun (px, py) -> (px - sx, py - sy)) ps in
  let angles = List.map (fun (vx, vy) -> atan2 (float_of_int vy) (float_of_int vx)) vs 
  in angles 
     |> List.combine ps 
     |> List.sort (fun (_, a1) (_, a2) -> compare a1 a2)
     |> List.map fst

let check_clockwise p1 p2 p3 = false

let rec graham_scan sorted_ps =
  match sorted_ps with
  | p1 :: p2 :: p3 :: rest_sorted_ps -> 
     if check_clockwise p1 p2 p3
     then p1 :: graham_scan (p2 :: p3 :: rest_sorted_ps)
     else graham_scan (p1 :: p3 :: rest_sorted_ps)
  | rest_sorted_ps -> rest_sorted_ps

let convex_hull ps = 
  if List.length ps > 3
  then let start_point = find_start_point ps in
       let sorted_ps = angle_sort start_point ps in
       graham_scan sorted_ps
  else ps
