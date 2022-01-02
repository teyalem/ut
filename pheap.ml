(* Pairing Heap *)

module type OrderedType = Heap.OrderedType

module type S = sig
  type elt
  type t

  val create : unit -> t
  val find_min : t -> elt
  val insert : t -> elt -> t
  val delete_min : t -> t
end

module Make(Ord : OrderedType) : S with type elt = Ord.t =
struct
  type elt = Ord.t
  type t =
    | Empty
    | Tree of elt * t list

  let create () = Empty

  let find_min = function
    | Empty -> failwith "Pheap.find_min"
    | Tree (e, _) -> e

  let meld a b =
    match a with
    | Empty -> b
    | Tree (ea, ca) ->
      match b with
      | Empty -> a
      | Tree (eb, cb) ->
        if compare ea eb < 0
        then Tree (ea, b::ca)
        else Tree (eb, a::cb)

  let insert h e =
    meld (Tree (e, [])) h

  let rec merge_pairs = function
    | [] -> Empty
    | [x] -> x
    | a::b::rest ->
      meld (meld a b) (merge_pairs rest)

  let delete_min = function
    | Empty -> failwith "Pheap.delete_min"
    | Tree (_, c) -> merge_pairs c

end
