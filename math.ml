(* Extended GCD *)
let rec egcd a b =
  if b = 0 then 1, 0
  else
    let q = a/b
    and r = a mod b
    in
    let (s, t) = egcd b r in
    (t, s - q*t)

(* Find inverse of modulo *)
let mod_inv a b =
  let x, y = egcd a b in
  if a*x + b*y = 1 then Some x else None

(* Chinese Reminder Theorem *)
(* Poorly implemented *)
let crt cong =
  let open List in
  let ml, al = split cong in
  let m = fold_left Int.mul 1 ml in
  let big_ml = map (fun n -> m / n) ml in
  let invs = map2 mod_inv big_ml ml |> map Option.get
  in
  map2 Int.mul big_ml invs
  |> map2 Int.mul al
  |> fold_left Int.add 0
  |> fun n -> n mod m

(* pow_mod n m modn is n^m mod modn. *)
let pow_mod n m modn =
  let rec pow_tail m a1 a2 =
    match m with
      0 -> 1
    | 1 -> (a1 * a2) mod modn
    | m when m mod 2 = 0 -> pow_tail (m/2) ((a1*a1) mod modn) a2
    | m -> pow_tail (m-1) a1 ((a1*a2) mod modn)
  in
  pow_tail m n 1
