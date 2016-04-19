open BatTuple.Tuple2
open BatPervasives

let find_start_point ps =
  let accumulate p1 p2 = swap @@ min (swap p1) (swap p2) in
  List.fold_left accumulate (max_int, max_int) ps

let angle_sort (sx, sy) ps =
  let vs = List.map (fun (px, py) -> (px - sx, py - sy)) ps in
  let angles = List.map (mapn float_of_int %> swap %> uncurry atan2) vs
  in angles
     |> List.combine ps
     |> List.sort (fun (_, a1) (_, a2) -> Pervasives.compare a1 a2)
     |> List.map fst

let ccw (x1, y1) (x2, y2) (x3, y3) =
  let (v1x, v1y) = (x2 - x1, y2 - y1) in
  let (v2x, v2y) = (x3 - x2, y3 - y2) in
  v1x * v2y - v1y * v2x

let rec drop_until_ccw p1 convex_hull =
  match convex_hull with
  | p2 :: p3 :: rest_convex_hull when ccw p1 p2 p3 <= 0 ->
     drop_until_ccw p1 (p3 :: rest_convex_hull)
  | rest_convex_hull -> rest_convex_hull

let rec graham_scan sorted_ps =
  match sorted_ps with
  | p1 :: rest_sorted_ps -> let rest_convex_hull = drop_until_ccw p1 @@ graham_scan rest_sorted_ps in
                            p1 :: rest_convex_hull
  | [] -> []

let convex_hull ps =
  if List.length ps > 3
  then let start_point = find_start_point ps in
       let sorted_ps = start_point :: (angle_sort start_point @@ List.filter (fun p -> p != start_point) ps) in
       graham_scan sorted_ps
  else ps
