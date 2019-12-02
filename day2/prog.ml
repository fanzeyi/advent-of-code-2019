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

let rec find_ state noun verb =
  let new_state = Array.copy state in
  new_state.(1) <- noun ;
  new_state.(2) <- verb ;
  match execute new_state with
  | Some (c, _) when c.(0) = 19690720 -> Some (noun, verb)
  | _ ->
      if verb >= 100 then find_ state (noun + 1) 0
      else find_ state noun (verb + 1)

let find state = find_ state 0 0

(* Part 1 *)
let main_execute () =
  let initial_state = read_input () in
  match execute initial_state with
  | Some (c, _) -> Array.iter (fun num -> string_of_int num |> print_endline) c
  | None -> ()

(* Part 2 *)
let main () =
  match find (read_input ()) with
  | Some (noun, verb) -> Printf.printf "result: noun: %d verb: %d\n" noun verb
  | None -> Printf.printf "not found\n"

;;
main ()
