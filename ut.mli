(** Utilities for Problem-Solving *)

(** {0 Functions} *)

(** [take n xs] takes n elements from xs. *)
val take : int -> 'a list -> 'a list

val drop : int -> 'a list -> 'a list

(** [sum ns] returns sum of all elements in ns. *)
val sum : int list -> int

val product : int list -> int

(** [group_count xs] returns (element, number) list, where element is in
 * xs and number is the number of the element in xs. List must be
 * sorted beforehand. *)
val group_count : 'a list -> ('a * int) list

val sign : int -> int

(** {0 Modules } *)

(** {1 Common Modules } *)

module IO = Io (** quick I/O operations. *)

module Delim = Delim (** CSV Parser *) 

module Seq = Seq (** Utility for Seq module *)

module Prime = Prime (** Prime utilities *)

module Math = Math (** Math functions (especially modular things) *)

module Mat = Mat (** Matrix, i.e. 2D Array *)

module Fasta = Fasta (** FASTA format parser *)

module Pheap = Pheap
module Heap = Heap

module Pathfind = Pathfind

module Neigh = Neigh

(** {1 Modules for AoC 2020 } *)

module Graph = Graph (** Graph *)

module Block = Block (** 2D Matrix functor *)

module Bitarray = Bitarray (** Bitarray *)

(** Functor for building Cellular Automata *)
module CellularAutomata = Cellautomata

(** 2D Matrix Board for CellularAutomata module *)
module BlockBoard = Blockboard

(** {1 Modules for Aoc 2019 } *)

(** full-featured Intcode VM *)
module IntCode = Intcode

(** Sparse representation of Block module *)
module SparseBlock = Sparseblock

(** {0 Type aliases } *)

type graph = Graph.t (** alias of Graph.t *) 
