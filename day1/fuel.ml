(* Part 1 *)
let fuel_ mass = (mass / 3) - 2

(* Part 2 *)
let rec fuel mass =
  let addition = fuel_ mass in
  if addition <= 0 then 0 else addition + fuel addition

let line_stream_of_channel channel =
  Stream.from (fun _ ->
      try Some (input_line channel) with End_of_file -> None )

let main =
  line_stream_of_channel stdin
  |> BatStream.map (fun line -> int_of_string line |> fuel)
  |> BatStream.fold (fun a b -> (a + b, None))
  |> string_of_int |> print_endline

;;
main
