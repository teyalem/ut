(** Block *)

(** Input signiture of functor [Block.Make]. *)
module type SignType = sig
  type t

  val default : t

  val of_char : char -> t
  val to_char : t -> char
end

(** Output signiture of functor [Block.Make]. *)
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

(** Functor *)
module Make(Sign : SignType) :
  (S with type elt = Sign.t and type t = Sign.t Mat.t)
