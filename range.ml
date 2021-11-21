(* inclusive range *)
type t = int * int

let make s e = s, e
let contains (s, e) n = s <= n && n <= e

let start_num (s, _) = s
let end_num (_, e) = e

(* iterate range with function f. *)
let iter f (s, e) =
  for i = s to e do f i done

let to_seq (s, e) =
  Seq.unfold (fun i -> if i > e then None else Some (i, i+1)) s

let to_list range = to_seq range |> List.of_seq
