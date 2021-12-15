(** {1 Matrices, i.e. 2D Arrays *)

(** Matrix type *)
type 'a t = 'a array array

(** [make dimx dimy e] returns a new matrix of dimx by dimy, filled with
    e. *)
val make : int -> int -> 'a -> 'a t

(** [init dimx dimy f] returns a new matrix of dimx by dimy, whose
    element at (x, y) is f x y. *)
val init : int -> int -> (int -> int -> 'a) -> 'a t

(** [get mat x y] returns the element of mat at (x, y).
    It raises Invalid_argument if the index is out of bounds. *)
val get : 'a t -> int -> int -> 'a

val set : 'a t -> int -> int -> 'a -> unit

(** getting dimensions *)

val dimx : 'a t -> int
val dimy : 'a t -> int
val dim : 'a t -> int * int

(** copy matrix. *)
val copy : 'a t -> 'a t

val sub : 'a t -> int * int -> int * int -> 'a t

val transpose : 'a t -> 'a t

(** concatenate horizontally *)
val concat_horiz : 'a t list -> 'a t

(** concatenate vertically *)
val concat_vert : 'a t list -> 'a t

(** concatenate *)
val concat : 'a t list list -> 'a t

(** {1 Map } *)

val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (int -> int -> 'a -> 'b) -> 'a t -> 'b t

(** {1 Iterators } *)

val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> int -> 'a -> unit) -> 'a t -> unit
val iter_row : ('a -> unit) -> (unit -> unit) -> 'a t -> unit
val iter_col : ('a -> unit) -> (unit -> unit) -> 'a t -> unit

(** {1 Arrays and Sequences } *)

val of_seq : 'a Seq.t Seq.t -> 'a t
val to_seq : 'a t -> 'a Seq.t Seq.t

val (.%(;..)) : 'a t -> int array -> 'a
val (.%(;..)<-) : 'a t -> int array -> 'a -> unit
