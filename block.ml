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

  let parse sl =
    List.to_seq sl
    |> Seq.map (fun s -> String.to_seq s)
    |> Seq.map (Seq.map Sign.of_char)
    |> Mat.of_seq

  let parse_raw dimx dimy str =
    let open Seq in
    let seq = String.to_seq str in
    let rec split n seq : 'a Seq.t Seq.t =
      match seq () with
      | Nil -> empty
      | _ -> cons (take n seq) (split n @@ drop n seq)
    in
    let block =
      take (dimx*dimy) seq
      |> split dimx
      |> map (map Sign.of_char)
      |> Mat.of_seq
    in
    block, String.of_seq @@ drop (dimx*dimy) seq

  let count f block =
    let count = ref 0 in
    Mat.iter (fun c -> if f c then incr count) block;
    !count

  (* count occurence of element t *)
  let count_occur t block =
    count ((=) t) block

  let of_matrix = Fun.id
  let to_matrix = Fun.id
end
