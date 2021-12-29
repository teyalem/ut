(* read lines from input channel. *)
let rec input_lines (ic: in_channel) =
  match input_line ic with
  | line -> line::(input_lines ic)
  | exception End_of_file -> []

(* NOTE: input_all function will be introduced in Ocaml 4.14. *)
let input_all (file: in_channel) =
  really_input_string file (in_channel_length file)

let rec read_lines () =
  match read_line () with
  | exception _ -> []
  | x -> x :: read_lines ()

let read_all () =
  Seq.unfold (fun () -> try Some (read_line (), ()) with _ -> None) ()
  |> List.of_seq
  |> String.concat "\n"
