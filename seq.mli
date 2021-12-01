(* Sequence Utilities *)
include module type of Stdlib.Seq

(* Properties *)
val nth_opt: int -> 'a t -> 'a option
val nth: int -> 'a t -> 'a

val length: 'a t -> int

(* Generators *)
val init: int -> (int -> 'a) -> 'a t

(* Taking/Dropping *)
val take: int -> 'a t -> 'a t
val take_while: ('a -> bool) -> 'a t -> 'a t

(* Iterators *)
val map_tail: ('a -> 'a t -> 'a t) -> 'a t -> 'a t

val repeat_each: int -> 'a t -> 'a t
val repeat: int -> 'a t -> 'a t
val cycle: 'a t -> 'a t

val accumulate: int -> int t -> int t
val windows: int -> 'a t -> 'a t t
val trail: 'a t -> 'a t t

(* Sequence searching *)
val for_all: ('a -> bool) -> 'a t -> bool
val exists: ('a -> bool) -> 'a t -> bool

val find_opt: ('a -> bool) -> 'a t -> 'a option
val find: ('a -> bool) -> 'a t -> 'a

(* Structures *)
val integer: int -> int t
