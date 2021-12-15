(* Utilities *)

let rec take n = function
  | [] -> []
  | x::xs ->
    if n = 0 then []
    else x :: take (n-1) xs

let rev_array arr =
  Array.to_list arr
  |> List.rev
  |> Array.of_list

let sum l = List.fold_left Int.add 0 l

let starts_with pat str =
  let pat = Str.regexp ("^" ^ pat) in
  Str.string_match pat str 0

let group_count ns =
  List.fold_left
    (fun (ps, pre) n ->
       match pre with
       | None -> ps, Some (n, 1)
       | Some (k, i) ->
         if n = k
         then ps, Some (n, i+1)
         else (k, i)::ps, Some (n, 1))
    ([], None)
    ns

  |> (fun (ps, p) ->
      match p with
      | None -> ps
      | Some p -> p::ps)

(* Common modules *)
module IO = Io
module Fasta = Fasta
module Range = Range
module Delim = Delim
module Seq = Seq
module Prime = Prime
module Math = Math
module Mat = Mat

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
