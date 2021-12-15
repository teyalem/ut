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

  val make : int -> int -> t

  val get : t -> int -> int -> elt
  val set : t -> int -> int -> elt -> unit

  val dimx : t -> int
  val dimy : t -> int

  val sub : t -> int * int -> int * int -> t
  val copy : t -> t

  val iteri : (int -> int -> elt -> unit) -> t -> unit

  (* print and parse *)
  val print : t -> unit
  val parse : string list -> t
  val parse_raw : int -> int -> string -> t * string

  val count : (elt -> bool) -> t -> int
  val count_occur : elt -> t -> int

  (* Conversions *)
  val of_matrix : elt Mat.t -> t
  val to_matrix : t -> elt Mat.t
end

module Make(Sign: SignType) :
  (S with type elt = Sign.t and type t = Sign.t Mat.t) =
struct

  type elt = Sign.t
  type t = elt Mat.t

  let make dimx dimy = Mat.make dimx dimy Sign.default

  let get = Mat.get
  let set = Mat.set

  let dimx = Mat.dimx
  let dimy = Mat.dimy

  let sub = Mat.sub
  let copy = Mat.copy

  let iteri = Mat.iteri

  (* debug: print block *)
  let print block =
    let pc e = Printf.printf "%c" @@ Sign.to_char e in
    let pn () = Printf.printf "\n" in
    Mat.iter_row pc pn block;
    pn ()

  (* parse a matrix *)
  let parse sl =
    let open List in
    (* converting string list to char list list *)
    let sl = map (fun s -> String.to_seq s |> of_seq) sl in
    let dimx = length @@ hd sl and dimy = length sl in
    let mat = make dimx dimy in
    iteri
      (fun y s -> iteri
          (fun x c -> set mat x y @@ Sign.of_char c)
          s)
      sl;
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
    iteri (fun _ _ c -> if f c then incr count) block;
    !count

  (* count occurence of element t *)
  let count_occur t block =
    count ((=) t) block

  let of_matrix = Fun.id
  let to_matrix = Fun.id
end
