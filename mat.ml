(* Matrix, 2D array *)

type 'a t = 'a array array

let make = Array.make_matrix

let init dimx dimy f =
  Array.init dimx
    (fun x -> Array.init dimy (fun y -> f x y))

let get mat x y =
  mat.(x).(y)

let set mat x y v =
  mat.(x).(y) <- v

let[@inline] dimx mat = Array.length mat

let[@inline] dimy mat = Array.length mat.(0)

let[@inline] dim mat = dimx mat, dimy mat

let copy mat =
  Array.(map copy mat)

let sub mat (sx, sy) (lenx, leny) =
  let open Array in
  sub mat sx lenx |> map (fun a -> sub a sy leny)

let transpose mat =
  init (dimx mat) (dimy mat) (fun x y -> get mat y x)

let concat_horiz mats =
  assert(mats <> []);
  let d = dimx @@ List.hd mats in
  assert(List.for_all (fun m -> dimx m = d) mats);
  Array.concat mats

let concat_vert mats =
  assert(mats <> []);
  let d = dimy @@ List.hd mats in
  assert(List.for_all (fun m -> dimy m = d) mats);
  let open Array in
  init d (fun i -> List.map (Fun.flip get i) mats |> concat)

let concat mats =
  List.map concat_vert mats |> concat_horiz

let map f mat =
  Array.(map (map f) mat)

let mapi f mat =
  Array.mapi (fun x a -> Array.mapi (f x) a) mat

let iter f mat =
  Array.(iter (iter f) mat)

let iteri f mat =
  Array.(iteri (fun x l -> iteri (f x) l) mat)

let iter_row f eorf mat =
  for y = 0 to dimy mat - 1 do
    for x = 0 to dimx mat - 1 do
      f @@ get mat x y
    done;
    eorf ()
  done

let iter_col f eocf mat =
  Array.iter
    (fun l -> Array.iter f l; eocf ())
    mat

let of_seq seq =
  Seq.map Array.of_seq seq |> Array.of_seq

let to_seq mat =
  Array.to_seq mat |> Seq.map Array.to_seq

(* Operators *)

let (.%(;..)) mat i = get mat i.(0) i.(1)
let (.%(;..)<-) mat i v = set mat i.(0) i.(1) v
