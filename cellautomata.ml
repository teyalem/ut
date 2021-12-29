(* Functor for Cellular Automata *)

module type HoneycombType = sig
  type elt
  type pos
  type t

  val neighbors: t -> pos -> elt list
  val next: elt list -> elt -> elt

  val get: t -> pos -> elt option
  val set: t -> pos -> elt -> unit

  val iteri: (pos -> elt -> unit) -> t -> unit
end

let make_automata (type t)
    (module M : HoneycombType with type t = t)
    board =
  let open M in
  let updates = ref [] in (* update list *)
  let collect_update pos elt =
    let neighs = neighbors board pos in
    let next_elt = next neighs elt in

    (* update only if the element is changed *)
    if next_elt <> elt then
      updates := (pos, next_elt) :: !updates
  in

  iteri collect_update board;
  (* perform update *)
  List.iter (fun (pos, elt) -> set board pos elt) !updates;
  board
