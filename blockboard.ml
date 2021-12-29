(* 2D board that can be used for Cellautomata.make_automata *)
open Block
open Cellautomata

let neighbor_positions (x, y) =
  let all_neighs =
    [ -1, -1; 0, -1; 1, -1;
      -1,  0;        1,  0;
      -1,  1; 0,  1; 1,  1 ]
  in
  List.map (fun (dx, dy) -> x + dx, y + dy) all_neighs

module type BoardType = sig
  include Block.S
  include HoneycombType
    with type elt := elt
     and type pos = int * int
     and type t := t
end

module type CellType = sig
  include SignType
  val next : t list -> t -> t
end

module Make (Cell : CellType)
  : (BoardType with type elt = Cell.t
                and type pos = int * int) =
struct
  include Block.Make(Cell)
  type pos = int * int

  let next = Cell.next

  let get b (x, y) =
    try Some (get b x y)
    with _ -> None

  let set b (x, y) elt = set b x y elt

  let neighbors b pos =
    neighbor_positions pos
    |> List.filter_map (get b)

  let iteri f b =
    iteri (fun x y c -> f (x, y) c) b
end
