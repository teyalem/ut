(** {1 Sequence Utilities } *)

(** This module is an extension of Stdlib.Seq module. It contains
    useful functions that Ocaml 4.13 or less do not contain.
    NOTE: many functions of this module are going to be included in
    stdlib in Ocaml 4.14. *)

include module type of Stdlib.Seq

val nth_opt: int -> 'a t -> 'a option
[@@deprecated "renamed to nth."]

(** [nth n seq] returns Some (nth element of seq). If seq is shorter than
    n, it returns None. *)
val nth: int -> 'a t -> 'a option

(** [nth_exn n seq] returns nth element of seq. If seq is shorter than n, it
    raises Not_found exception. *)
val nth_exn: int -> 'a t -> 'a

(** [repeat_each n seq] returns a sequence that every elements in seq
    repeats n times.*)
val repeat_each: int -> 'a t -> 'a t

(** [times n seq] repeat seq n times. formerly repeat.*)
val times: int -> 'a t -> 'a t

(** accumulate n seq returns a sequence A that
    A(0) = n
    A(i) = \sum_{k=0}^{i} A(k).  *)
val accumulate: int -> int t -> int t
[@@deprecated "use Stdlib.Seq.scan (+) 0 instead."]

(** [windows n seq] returns a sequence over all contiguous windows of
    length n. The windows overlap. If the slice is shorter than n, the
    sequence returns Seq.Nil.
    If n is 0, it raises Invalid_argument.

    {i(comment copied from rust std::slice::windows)} *)
val windows: int -> 'a t -> 'a t t

(** [trail seq] returns a sequence of sequence such that [ seq; drop 1
    seq; drop 2 seq; ... ] as a sequence. *)
val trail: 'a t -> 'a t t

(** [find_opt f seq] returns first elements that meets the predicate f.
 * If [exists f seq = false], returns None. *)
val find_opt: ('a -> bool) -> 'a t -> 'a option
[@@deprecated "use Seq.find instead."]

(** [find_exn f seq] does the same job as [find_opt], but it raises
 * Not_found exception when there's no such element. *)
val find_exn: ('a -> bool) -> 'a t -> 'a

(** increasing integers starting from n. *)
val integer: int -> int t
[@@deprecated "use Seq.ints instead."]
