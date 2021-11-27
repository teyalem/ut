(** Array of bits *)

(** type of Bitarray *)
type t

(** [get bs i] returns {i ith} bits from LSB of bs. *)
val get : t -> int -> int

(** [set bs i b] sets ith bits from LSB of bs to n. n must be 0 or 1. If
 * not, it throws Invalid_argument exception. *)
val set : t -> int -> int -> unit

(** [copy bs] returns a new bitarray the same as bs. *)
val copy : t -> t

(** [of_int n] returns a bitarray identical to n. *)
val of_int : int -> t

(** [to_int bs] returns an integer identical to bs. *)
val to_int : t -> int
