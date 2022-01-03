module type StateSpace = sig
  type space
  type state
  type data

  val data_id : data
  val is_end : space -> state * data -> bool
  val neighbors : space -> state * data -> (state * data) list
end

type ('space, 'state, 'data, 'out) pathfind_alg

val dfs : ('a, 'b, 'c, 'c) pathfind_alg
val bfs : ('a, 'b, 'c, 'c) pathfind_alg
val dfs_collect : ('a, 'b, 'c, 'c list) pathfind_alg
val bfs_collect : ('a, 'b, 'c, 'c list) pathfind_alg

module type WeightType = sig
  include Heap.OrderedType
  val zero : t
  val add : t -> t -> t
end

module type WeightedGraph = sig
  include StateSpace
  type weight

  val neighbors : space -> state * data -> (weight * state * data) list
end

val dijkstra :
  (module WeightType with type t = 'weight)
  -> (module WeightedGraph with type space = 'space
                            and type state = 'state
                            and type data = 'data
                            and type weight = 'weight)
  -> 'space
  -> start: 'state
  -> 'weight * 'data
