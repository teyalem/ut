(** {1 Sequence Utilities } *)

(** This module is an extension of Stdlib.Seq module. It contains
    useful functions that Ocaml 4.13 or less do not contain.
    NOTE: this module is not needed after Ocaml 4.14. *)

include module type of Stdlib.Seq

(** [nth_opt n seq] returns Some (nth element of seq). If seq is shorter than
    n, it returns None. *)
val nth_opt: int -> 'a t -> 'a option

(** [nth n seq] returns nth element of seq. If seq is shorter than n, it
    raises Not_found exception. *)
val nth: int -> 'a t -> 'a

(** [length seq] returns the length of seq. *)
val length: 'a t -> int

(** [init n f] is the same as Seq.cons (f 0) @@ Seq.cons (f 1) ...
    Seq.Nil. *)
val init: int -> (int -> 'a) -> 'a t

(** [take n seq] returns a sequence that has first n elements of seq. *)
val take: int -> 'a t -> 'a t

val take_while: ('a -> bool) -> 'a t -> 'a t

val map_tail: ('a -> 'a t -> 'a t) -> 'a t -> 'a t

(** [repeat_each n seq] returns a sequence that every elements in seq
    repeats n times.*)
val repeat_each: int -> 'a t -> 'a t

(** [repeat n seq] repeat seq n times. *)
val repeat: int -> 'a t -> 'a t

(** [cycle seq] cycles seq endlessly. *)
val cycle: 'a t -> 'a t

(** accumulate n seq returns a sequence A that
    A(0) = n
    A(i) = \sum_{k=0}^{i} A(k).  *)
val accumulate: int -> int t -> int t

(** [windows n seq] returns a sequence over all contiguous windows of
    length n. The windows overlap. If the slice is shorter than n, the
    sequence returns Seq.Nil.
    If n is 0, it raises Invalid_argument.

    {i(comment copied from rust std::slice::windows)} *)
val windows: int -> 'a t -> 'a t t

(** [trail seq] returns a sequence of sequence such that [ seq; drop 1
    seq; drop 2 seq; ... ] as a sequence. *)
val trail: 'a t -> 'a t t

(** [for_all f seq] returns [true] when all elements of seq meet the
 * predicate f, otherwise [false]. If seq is empty, it returns [true]. *)
val for_all: ('a -> bool) -> 'a t -> bool

(** [exists f seq] returns [true] when an element of seq meets the
 * predicate f, otherwise [false]. If seq is empty, it returns false. *)
val exists: ('a -> bool) -> 'a t -> bool

(** [find_opt f seq] returns first elements that meets the predicate f.
 * If [exists f seq = false], returns None. *)
val find_opt: ('a -> bool) -> 'a t -> 'a option

(** [find f seq] does the same job as [find_opt], but it raises
 * Not_found exception when there's no such element. *)
val find: ('a -> bool) -> 'a t -> 'a

(** increasing integers starting from n. *)
val integer: int -> int t
