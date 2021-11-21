(* take n elements from list. *)
val take : int -> 'a list -> 'a list

(* reverse array. *)
val rev_array : 'a array -> 'a array

(* return sum of all elements in list. *)
val sum : int list -> int

(* starts_with pat str checks that string str starts with pattern pat. *)
val starts_with : string -> string -> bool

(* group and count same elements *)
val group_count: 'a list -> ('a * int) list

(* Common modules *)
module IO = Io (* Useful IO Operations *)
module Range = Range
module Delim = Delim
module Coord = Coord
module Useq = Useq
module Prime = Prime

(* For AoC 2020 *)
module Graph = Graph
module Block = Block
module Bitarray = Bitarray
module CellularAutomata = Cellautomata
module BlockBoard = Blockboard

(* For Aoc 2019 *)
module IntCode = Intcode
module SparseBlock = Sparseblock

(* types *)
type graph = Graph.t
type range = Range.t
