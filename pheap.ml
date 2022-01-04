(* Pairing Heap *)

module type OrderedType = Heap.OrderedType

module type S = sig
  type key
  type 'a t

  val empty : 'a t
  val find_min : 'a t -> key * 'a
  val insert : key -> 'a -> 'a t -> 'a t
  val delete_min : 'a t -> 'a t
end

module Make(Ord : OrderedType) : S with type key = Ord.t =
struct
  type key = Ord.t
  type 'a t =
    | Empty
    | Tree of { k: key; v: 'a; c: 'a t list }

  let empty = Empty

  let find_min = function
    | Empty -> failwith "Pheap.find_min"
    | Tree {k; v; _} -> k, v

  let meld a b =
    match a with
    | Empty -> b
    | Tree {k=ka; v=va; c=ca} ->
      match b with
      | Empty -> a
      | Tree {k=kb; v=vb; c=cb} ->
        if Ord.compare ka kb < 0
        then Tree {k=ka; v=va; c = b::ca}
        else Tree {k=kb; v=vb; c = a::cb}

  let insert k v h =
    meld (Tree {k; v; c = []}) h

  let rec merge_pairs = function
    | [] -> Empty
    | [x] -> x
    | a::b::rest ->
      meld (meld a b) (merge_pairs rest)

  let delete_min = function
    | Empty -> failwith "Pheap.delete_min"
    | Tree {c; _} -> merge_pairs c

end
