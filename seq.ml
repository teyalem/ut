(* Sequence Utilities for Stdlib.Seq *)
include Stdlib.Seq

(* Properties *)

(* returns nth elements of a sequence, starting from 0.
 * if the length of a sequence is shorter than n, returns None. *)
let rec nth_opt n seq =
  match seq () with
  | Nil -> None
  | Cons (x, f) -> if n = 0 then Some x else nth_opt (n-1) f

let nth n seq =
  match nth_opt n seq with
  | None -> raise Not_found
  | Some x -> x

(* get length of a sequence. *)
let length seq =
  fold_left (fun p _ -> p+1) 0 seq

(* Generators *)

(* init n f returns [ f 0; f 1; ...; f (n-1) ], as a sequence. *)
let init n f =
  let rec gen i () =
    if i = n
    then Nil
    else Cons (f i, gen (i+1))
  in
  gen 0

(* Taking/Dropping *)

(* take first n elements from a sequence. *)
let rec take n seq () =
  if n = 0
  then Nil
  else
    match seq () with
    | Nil -> Nil
    | Cons (x, f) -> Cons (x, take (n-1) f)

(* take first n elements from a sequence. *)
let rec drop n seq =
  if n = 0
  then seq
  else
    match seq () with
    | Nil -> empty
    | Cons (_, f) -> drop (n-1) f

(* take elements from a sequence that meets the predicate. *)
let rec take_while pred seq () =
  match seq () with
  | Nil -> Nil
  | Cons (x, f) ->
    if pred x
    then Cons (x, take_while pred f)
    else Nil

(* repeat a sequence n times. *)
let rec repeat n seq () =
  if n = 0
  then Nil
  else append seq (repeat (n-1) seq) ()

(* repeat every elements in a sequnce n times. *)
let rec repeat_each n seq () =
  let rec lo i x () =
    if i = 0
    then Nil
    else Cons(x, lo (i-1) x)
  in
  match seq () with
  | Nil -> Nil
  | Cons(x, f) -> append (lo n x) (repeat_each n f) ()

(* repeat a sequence endlessly. *)
let rec cycle seq =
  append seq @@ cycle seq

(* accumulate n seq returns a sequence A that
 * A(0) = n
 * A(i) = \sum_{k=0}^{i} A(k).
 *)
let rec accumulate n seq () =
  match seq () with
  | Nil -> Cons (n, empty)
  | Cons (x, f) -> Cons (n, accumulate (n+x) f)

let rec windows size seq =
  if size = 0 then invalid_arg "Seq.windows"
  else (fun () ->
    match seq () with
    | Nil -> Nil
    | Cons (_, f) ->
      if length seq < size
      then Nil
      else Cons (take size seq, windows size f))

(* return a sequence of sequence that [ seq; drop 1 seq; drop 2 seq; ... ] as a sequence. *)
let rec trail seq () =
  match seq () with
  | Nil -> Nil
  | Cons(_, f) -> Cons(seq, trail f)

(* Sequence searching *)

let rec for_all pred seq =
  match seq () with
  | Nil -> true
  | Cons (x, f) ->
    pred x && for_all pred f

let rec exists pred seq =
  match seq () with
  | Nil -> false
  | Cons (x, f) ->
    pred x || exists pred f

(* find first elements that meets the predicate from a sequence. *)
let rec find_opt pred seq =
  match seq () with
  | Nil -> None
  | Cons (x, f) ->
    if pred x
    then Some x
    else find_opt pred f

let find pred seq =
  match find_opt pred seq with
  | None -> raise Not_found
  | Some x -> x

(* Structures *)

(* sequence of integers starting from n. *)
let integer n =
  unfold (fun n -> Some (n, n+1)) n
