let rec read_lines (file: in_channel) : string list =
  match input_line file with
  | line -> line::(read_lines file)
  | exception End_of_file -> []

(* NOTE: input_all function introduced in Ocaml 4.14. *)
let input_all (file: in_channel) : string =
  really_input_string file (in_channel_length file)

let read_data () =
  Seq.unfold (fun () -> try Some (read_line (), ()) with _ -> None) ()
  |> List.of_seq
  |> String.concat "\n"
