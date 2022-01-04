(* base abstraction of state space *)
module type StateSpace = sig
  type space
  type state
  type data

  val data_id : data
  val is_end : space -> state * data -> bool
  val neighbors : space -> state * data -> (state * data) list
end

(* type for state pools (like stacks, queues, priority queues...) *)
module type PoolType = sig
  type 'a pool

  (* create a new pool *)
  val create : unit -> 'a pool

  (* check pool is empty *)
  val is_empty : 'a pool -> bool

  (* add state into a pool *)
  val add : 'a pool -> 'a -> unit

  (* get next state for searching from a pool *)
  val take : 'a pool -> 'a
end

module StackPool : PoolType = struct
  type 'a pool = 'a Stack.t

  let create = Stack.create
  let is_empty = Stack.is_empty
  let add pool x = Stack.push x pool
  let take = Stack.pop
end

module QueuePool : PoolType = struct
  type 'a pool = 'a Queue.t

  let create = Queue.create
  let is_empty = Queue.is_empty
  let add pool x = Queue.push x pool
  let take = Queue.pop
end

let search
  (type space state data)
  (module P : PoolType)
  (module S : StateSpace
    with type space = space and type state = state and type data = data)
  space
  ~start =
  let pool = P.create () in
  let visited = Hashtbl.create 100 in
  let rec next () =
    let s, d = P.take pool in
    if Hashtbl.mem visited s then next () else s, d
  in

  let rec aux (s, d) =
    Hashtbl.add visited s true;
    if S.is_end space (s, d) then d
    else begin
      S.neighbors space (s, d) |> List.iter (P.add pool);
      aux @@ next ()
    end
  in
  aux (start, S.data_id)

let collect
  (type space state data)
  (module P : PoolType)
  (module S : StateSpace
    with type space = space and type state = state and type data = data)
  space
  ~start =
  let pool = P.create () in
  let visited = Hashtbl.create 100 in
  let ds = ref [] in

  let rec aux () =
    if P.is_empty pool then !ds
    else begin
      let s, d = P.take pool in
      if not @@ Hashtbl.mem visited s then begin
        Hashtbl.add visited s true;
        if S.is_end space (s, d)
        then ds := d :: !ds
        else S.neighbors space (s, d) |> List.iter (P.add pool)
      end;
      aux ()
    end
  in
  P.add pool (start, S.data_id); aux ()

type ('space, 'state, 'data, 'out) pathfind_alg =
  (module StateSpace
    with type space = 'space
     and type state = 'state
     and type data = 'data)
  -> 'space
  -> start: 'state
  -> 'out

let dfs
    (type space state data)
    (module S : StateSpace
      with type space = space and type state = state and type data = data) =
  search (module StackPool) (module S)

let bfs
    (type space state data)
    (module S : StateSpace
      with type space = space and type state = state and type data = data) =
  search (module QueuePool) (module S)

let dfs_collect
    (type space state data)
    (module S : StateSpace
      with type space = space and type state = state and type data = data) =
  collect (module StackPool) (module S)

let bfs_collect
    (type space state data)
    (module S : StateSpace
      with type space = space and type state = state and type data = data) =
  collect (module QueuePool) (module S)

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

let dijkstra
  (type space state data weight)
  (module W : WeightType with type t = weight)
  (module S : WeightedGraph with type space = space
                             and type state = state
                             and type data = data
                             and type weight = weight)
  space
  ~start =
  let module H = Pheap.Make(W) in
  let heap = ref H.empty in
  let visited = Hashtbl.create 100 in

  let add (w, s, d) = heap := H.insert w (s, d) !heap in
  let rec next () =
    let c, (s, d) = H.find_min !heap in
    heap := H.delete_min !heap;
    if Hashtbl.mem visited s
    then next ()
    else c, s, d
  in

  let rec aux (c, s, d) =
    Hashtbl.add visited s true;
    if S.is_end space (s, d) then c, d
    else begin
      S.neighbors space (s, d)
      |> List.map (fun (w, s', d') -> W.add c w, s', d')
      |> List.iter add;
      aux @@ next ()
    end
  in
  aux (W.zero, start, S.data_id)
