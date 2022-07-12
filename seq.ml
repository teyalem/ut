include Stdlib.Seq

let rec nth n seq =
  match seq () with
  | Nil -> None
  | Cons (x, f) -> if n = 0 then Some x else nth (n-1) f

let nth_opt = nth

let nth_exn n seq =
  match nth n seq with
  | None -> raise Not_found
  | Some x -> x

let rec times n seq () =
  if n = 0
  then Nil
  else append seq (times (n-1) seq) ()

let rec repeat_each n seq () =
  let rec lo i x () =
    if i = 0 then Nil else Cons (x, lo (i-1) x)
  in
  match seq () with
  | Nil -> Nil
  | Cons (x, f) -> append (lo n x) (repeat_each n f) ()

let rec accumulate n seq () =
  match seq () with
  | Nil -> Cons (n, empty)
  | Cons (x, f) -> Cons (n, accumulate (n+x) f)

let rec windows size seq =
  if size = 0 then invalid_arg "Seq.windows"
  else
    fun () ->
      if length seq < size then Nil
      else Cons (take size seq, windows size @@ drop 1 seq)

let rec trail seq () =
  match seq () with
  | Nil -> Nil
  | Cons (_, f) -> Cons (seq, trail f)

let find_opt = find

let find_exn pred seq =
  match find pred seq with
  | None -> raise Not_found
  | Some x -> x

let integer = ints
