let parse_entry str =
  let remove_nl str =
    String.to_seq str
    |> Seq.filter (fun c -> c <> '\n')
    |> String.of_seq
  in
  Scanf.sscanf str "%s@\n%s@!" (fun tag str -> tag, remove_nl str)

let parse str =
  String.split_on_char '>' str
  |> List.tl (* discard empty *)
  |> List.map parse_entry
