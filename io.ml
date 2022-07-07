(* read lines from input channel. *)
let rec input_lines (ic: in_channel) =
  match In_channel.input_line ic with
  | None -> []
  | Some line -> line :: input_lines ic

let input_all =
  In_channel.input_all

let rec read_lines () =
  input_lines stdin

let read_all () =
  In_channel.input_all stdin
