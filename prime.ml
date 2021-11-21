open Seq

(* sequence of primes. *)
let primes =
  let rec sieve seq () =
    match seq () with
    | Nil -> Nil
    | Cons (x, f) -> Cons (x, sieve @@ filter (fun n -> n mod x <> 0) f)
  in
  cons 2 (sieve @@ unfold (fun p -> Some (p, p+2)) 3)

(* test n is prime *)
let is_prime n =
  if n <= 1 then false
  else if n mod 2 = 0 then n = 2
  else
    match
      for i = 3 to truncate @@ sqrt @@ float n do
        if n mod i = 0 then raise Exit else ()
      done
    with () -> true
       | exception Exit -> false

(* returns prime factors of integer n. *)
let factors n =
  let rec factor n seq () =
    if n = 1 (* base case *)
    then Nil
    else
      match seq () with
      | Nil -> Cons (n, empty)
      | Cons (x, f) ->
        if n mod x = 0
        then Cons (x, factor (n/x) @@ cons x f)
        else factor n f ()
  in
  factor n primes
