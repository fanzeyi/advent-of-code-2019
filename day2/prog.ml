let ptr state idx = state.(state.(idx))

let instruction func state index =
  let lhs = ptr state (index + 1) in
  let rhs = ptr state (index + 2) in
  state.(state.(index + 3)) <- func lhs rhs ;
  Some (state, index + 4)

let addition = instruction ( + )

let multiplication = instruction ( * )

(* execute the program *)
let rec execute_ state index =
  match
    match state.(index) with
    | 1 -> addition state index
    | 2 -> multiplication state index
    | _ -> None
  with
  | Some (state, index) -> execute_ state index
  | None -> Some (state, index)

let execute state = execute_ state 0

let read_input () =
  read_line () |> String.split_on_char ',' |> List.map int_of_string
  |> Array.of_list

let main =
  match execute (read_input ()) with
  | Some (c, _) -> Array.iter (fun num -> string_of_int num |> print_endline) c
  | None -> ()

;;
main
