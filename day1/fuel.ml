let fuel mass = (float_of_int mass /. 3.0 |> floor |> int_of_float) - 2

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

(*line_stream_of_channel stdin |> List.iter print_endline*)
