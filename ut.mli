(** Utilities for Problem-Solving *)

(** {0 Functions} *)

(** [take n xs] takes n elements from xs. *)
val take : int -> 'a list -> 'a list

(** [rev_array arr] returns reversed array. obsolete in 4.14 *)
val rev_array : 'a array -> 'a array

(** [sum ns] returns sum of all elements in ns. *)
val sum : int list -> int

(** [starts_with pat str] checks that string str starts with pattern pat. *)
val starts_with : string -> string -> bool

(** [group_count xs] returns (element, number) list, where element is in
 * xs and number is the number of the element in xs. List must be
 * sorted beforehand. *)
val group_count: 'a list -> ('a * int) list


(** {0 Modules } *)

module IO = Io (** quick I/O operations. *)

module Range = Range (** range *)

module Delim = Delim (** CSV Parser *) 

module Coord = Coord (** Coordinates *)

module Seq = Seq (** Utility for Seq module *)

module Prime = Prime (** Prime utilities *)

module Math = Math (** Math functions (especially modular things) *)

module Fasta = Fasta (** FASTA Parser *)

(** {1 Modules for AoC 2020 } *)

module Graph = Graph (** Graph *)

module Block = Block (** 2D Matrix functor *)

module Bitarray = Bitarray (** Bitarray *)

(** Functor for building Cellular Automata *)
module CellularAutomata = Cellautomata

(** 2D Matrix Board for CellularAutomata module *)
module BlockBoard = Blockboard

(** {1 Modules for Aoc 2019 } *)

module IntCode = Intcode (** full-featured Intcode VM *)

(** Sparse representation of Block module *)
module SparseBlock = Sparseblock

(** {1 Type aliases } *)

type graph = Graph.t (** alias of Graph.t *) 

type range = Range.t (** alias of Range.t *)
