(* Block of single elements *)

(* Sign Type *)
module type SignType = sig
  type t

  val default: t

  val of_char: char -> t
  val to_char: t -> char
end

(* Block module *)
module type S = sig
  type elt
  type t

  val make: int -> int -> t

  val get: t -> int -> int -> elt
  val set: t -> int -> int -> elt -> unit

  val dimx: t -> int
  val dimy: t -> int

  val sub: t -> int * int -> int * int -> t

  val copy: t -> t

  (* iterations *)
  val iteri: (int -> int -> elt -> unit) -> t -> unit

  (* print and parse *)
  val print: t -> unit
  val parse: string list -> t
  val parse_raw: int -> int -> string -> t * string

  (* count occurences of an element *)
  val count: (elt -> bool) -> t -> int
  val count_occur: elt -> t -> int

  (* Conversions *)
  val of_matrix: elt array array -> t
  val to_matrix: t -> elt array array
end

module Make(Sign: SignType)
  : (S with type elt = Sign.t) =
struct
  type elt = Sign.t
  type t = elt array array

  (* make empty block *)
  let make dimx dimy = Array.make_matrix dimx dimy Sign.default

  (* get/set a position of block *)
  let get block x y =
    try block.(x).(y)
    with _ -> failwith "Block.get"

  let set block x y n =
    try block.(x).(y) <- n
    with _ -> failwith "Block.set"

  let dimx block = Array.length block
  let dimy block = Array.length block.(0)

  let sub block (sx, sy) (lenx, leny) =
    let open Array in
    sub block sx lenx
    |> map (fun a -> sub a sy leny)

  let copy block = Array.(map copy block)

  (* iterate through block *)
  let iteri f block =
    for x = 0 to Array.length block - 1 do
      for y = 0 to Array.length block.(0) - 1 do
        f x y (get block x y)
      done
    done

  (* debug: print block *)
  let print block =
    for y = 0 to dimy block - 1 do
      for x = 0 to dimx block - 1 do
        get block x y |> Sign.to_char |> print_char
      done;
      print_newline ()
    done;
    print_newline ()

  (* parse a matrix *)
  let parse sl =
    (* converting string list to char list list *)
    let sl = List.map (fun s -> String.to_seq s |> List.of_seq) sl in
    let dimx = List.(length (hd sl))
    and dimy = List.length sl in
    let mat = make dimx dimy in
    sl |> List.iteri (fun y s ->
         s |> List.iteri (fun x c -> set mat x y (Sign.of_char c)));
    mat

  let parse_raw dimx dimy str =
    let seq = ref @@ String.to_seq str
    and block = make dimx dimy in
    for y = 0 to dimy - 1 do
      for x = 0 to dimx - 1 do
        let c, s = match !seq () with
          | Seq.Nil -> Sign.default, Seq.empty
          | Seq.Cons (c, f) -> Sign.of_char c, f
        in
        seq := s;
        set block x y c
      done
    done;
    block, String.of_seq !seq

  let count f block =
    let count = ref 0 in
    iteri (fun _ _ c ->
        if f c then incr count else ()) block;
    !count

  (* count occurence of element t *)
  let count_occur t block =
    count ((=) t) block

  let of_matrix mat = mat
  let to_matrix b = b

end
