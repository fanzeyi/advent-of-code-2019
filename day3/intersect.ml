type point = {x: int; y: int}

let make_point x y = {x; y}

let ( +. ) (lhs : point) (rhs : point) : point =
  make_point (lhs.x + rhs.x) (lhs.y + rhs.y)

let distance_of_point {x; y} = abs x + abs y

let point_to_string p = Printf.sprintf "(%d, %d)" p.x p.y

let opt_to_string format_ opt =
  match opt with
  | Some opt -> Printf.sprintf "Some(%s)" @@ format_ opt
  | None -> "None"

let optpoint_to_string = opt_to_string point_to_string

type line = {left: point; right: point}

let make_line_ left right = {left; right}

(* ensures the left point is always the one on the left bottom side *)
let make_line lhs rhs =
  if lhs.x = rhs.x then
    if lhs.y < rhs.y then make_line_ lhs rhs else make_line_ rhs lhs
  else if lhs.x < rhs.x then make_line_ lhs rhs
  else make_line_ rhs lhs

let make_line2 x1 y1 x2 y2 = make_line (make_point x1 y1) (make_point x2 y2)

let line_to_string line =
  let left = point_to_string line.left in
  let right = point_to_string line.right in
  Printf.sprintf "%s -> %s" left right

let optline_to_string = opt_to_string line_to_string

(* ensure a <= b <= c <= d *)
let ensure_ordering a b c d = a <= b && b <= c && c <= d

(* ensure a <= b <= c *)
let ensure_ordering3 a b c = a <= b && b <= c

(* ensure a == b == c == d *)
let ensure_equal a b c d = a == b && b == c && c == d

let absmin a b = if abs a < abs b then a else b

(* line -> line -> point option *)
let intersect_ p1 p2 =
  if
    ensure_ordering p1.left.x p2.left.x p2.right.x p1.right.x
    && ensure_ordering p2.left.y p1.left.y p1.right.y p2.right.y
  then
    if p1.left.x == p1.right.x then Some (make_point p1.left.x p2.left.y)
    else Some (make_point p2.left.x p1.left.y)
  else if ensure_equal p1.left.y p1.right.y p2.left.y p2.right.y then
    if ensure_ordering3 p1.left.x p2.left.x p1.right.x then
      Some (make_point (absmin p2.left.x p1.right.x) p1.left.y)
    else if ensure_ordering3 p2.left.x p1.left.x p2.right.x then
      Some (make_point (absmin p1.left.x p2.right.x) p1.left.y)
    else None
  else if ensure_equal p1.left.x p1.right.x p2.left.x p2.right.x then
    if ensure_ordering3 p1.left.y p2.left.y p1.right.y then
      Some (make_point p1.left.x (absmin p2.left.y p1.right.y))
    else if ensure_ordering3 p2.left.y p1.left.y p2.right.y then
      Some (make_point p1.left.x (absmin p1.left.y p2.right.y))
    else None
  else None

let intersect (p1 : line) (p2 : line) =
  let result = intersect_ p1 p2 in
  match result with Some x -> Some x | None -> intersect_ p2 p1

let ( %+ ) p1 p2 = intersect p1 p2

let from_path (str : string) : point =
  let chars = BatString.to_list str in
  match chars with
  | [] -> make_point 0 0
  | x :: xs -> (
      let offset = BatString.of_list xs |> int_of_string in
      match x with
      | 'U' -> make_point 0 offset
      | 'D' -> make_point 0 (-offset)
      | 'L' -> make_point (-offset) 0
      | 'R' -> make_point offset 0
      | _ -> make_point 0 0 )

let calculate_next (path : string) (previous : point list) : point =
  let offset = from_path path in
  match previous with [] -> offset | x :: _ -> x +. offset

let rec make_points_ (inputs : string list) (result : point list) =
  match inputs with
  | [] -> result
  | x :: xs -> make_points_ xs (calculate_next x result :: result)

let make_points inputs = make_points_ inputs []

let make_path (points : point list) : line list =
  let head = BatList.rev points |> BatList.tl |> BatList.rev in
  let tail = BatList.tl points in
  BatList.map2 (fun a b -> make_line a b) head tail

let read_path () : line list =
  read_line () |> String.split_on_char ',' |> make_points |> make_path

let find_intersection (paths : line list) (line : line) : point list =
  BatList.filter_map (fun x -> intersect line x) paths

let find_intersections (l1 : line list) (l2 : line list) : point list =
  List.map (fun ll -> find_intersection l1 ll) l2 |> List.flatten

let main () =
  let path1 = read_path () in
  let path2 = read_path () in
  find_intersections path1 path2
  |> List.sort (fun p1 p2 ->
         compare (distance_of_point p1) (distance_of_point p2) )
  |> List.hd |> distance_of_point |> string_of_int |> print_endline

let expect foo result =
  let answer = foo () in
  Printf.printf "expect: %s answer: %s\n" (optpoint_to_string result)
  @@ optpoint_to_string answer

let test () =
  expect
    (fun _ -> make_line2 0 0 0 5 %+ make_line2 (-1) 1 5 1)
    (Some (make_point 0 1)) ;
  expect (fun _ -> make_line2 0 0 0 5 %+ make_line2 (-1) 10 5 10) None ;
  expect (fun _ -> make_line2 0 0 0 5 %+ make_line2 (-1) (-2) 5 (-2)) None ;
  expect (fun _ -> make_line2 0 0 0 5 %+ make_line2 (-4) 1 (-2) 1) None ;
  expect (fun _ -> make_line2 0 0 0 5 %+ make_line2 1 1 2 1) None ;
  expect (fun _ -> make_line2 0 0 5 0 %+ make_line2 10 (-1) 10 5) None ;
  expect (fun _ -> make_line2 0 0 5 0 %+ make_line2 (-2) (-1) (-2) 5) None ;
  expect (fun _ -> make_line2 0 0 5 0 %+ make_line2 1 (-4) 1 (-2)) None ;
  expect (fun _ -> make_line2 0 0 5 0 %+ make_line2 1 1 1 2) None ;
  expect
    (fun _ -> make_line2 0 0 0 5 %+ make_line2 (-1) 5 5 5)
    (Some (make_point 0 5)) ;
  expect
    (fun _ -> make_line2 0 0 0 5 %+ make_line2 0 1 0 6)
    (Some (make_point 0 1)) ;
  expect
    (fun _ -> make_line2 0 1 0 5 %+ make_line2 0 (-5) 0 4)
    (Some (make_point 0 1)) ;
  expect
    (fun _ -> make_line2 0 1 0 5 %+ make_line2 0 (-5) 0 4)
    (Some (make_point 0 1))

;;
main ()
