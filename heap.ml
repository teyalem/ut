module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t

  exception Empty

  val create : int -> t

  val push : t -> elt -> unit

  val pop : t -> elt
  val pop_opt : t -> elt option

  val top : t -> elt
  val top_opt : t -> elt option

  val clear : t -> unit
  val copy : t -> t

  val is_empty : t -> bool

  val length : t -> int

  val iter : (elt -> unit) -> t -> unit
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
end

(* binary min heap *)
module Make(Ord : OrderedType) : S with type elt = Ord.t =
struct
  type elt = Ord.t
  type t = {
    mutable last : int;
    mutable data : elt option array;
  }

  exception Empty

  let empty = 0
  let root = 1
  let parent i = i/2
  let childs i = 2*i, 2*i+1

  let get { data; _ } i =
    Option.get data.(i)

  let set { data; _ } i e =
    data.(i) <- Some e

  let swap h i j =
    let e = get h i in
    set h i (get h j);
    set h j e

  let expand1 h =
    let open Array in
    h.last <- h.last + 1;
    if h.last >= length h.data then
      let arr = make (2 * length h.data) None in
      blit h.data 0 arr 0 (length h.data);
      h.data <- arr

  let shrink1 h =
    h.data.(h.last) <- None;
    h.last <- h.last - 1

  let is_prior h i j =
    let a = get h i and b = get h j in
    Ord.compare a b < 0

  let rec upheap h i =
    let p = parent i in
    if i <> root && is_prior h i p then begin
      swap h i p;
      upheap h p
    end

  let exists h i =
    i <= h.last

  let rec downheap h i =
    let l, r = childs i in
    let d =
      match exists h l, exists h r with
      | false, false -> None
      | true, false -> Some l
      | false, true -> Some r
      | true, true -> Some (if is_prior h l r then l else r)
    in

    match d with
    | None -> ()
    | Some j ->
      if is_prior h j i then begin
        swap h i j;
        downheap h j
      end

  let create size = {
    last = 0;
    data = Array.make (size+1) None;
  }

  let length { last; _ } =
    last

  let is_empty { last; _ } =
    last = empty

  let push h e =
    expand1 h;
    set h h.last e;
    upheap h h.last

  let top h =
    if length h = 0 then raise Empty else get h root

  let top_opt h =
    if length h = 0 then None else Some (get h root)

  let pop h =
    let e = top h in
    swap h root h.last;
    shrink1 h;
    downheap h root;
    e

  let pop_opt h =
    try Some (pop h) with _ -> None

  let copy { data; last } = {
    last;
    data = Array.copy data;
  }

  let clear h =
    Array.fill h.data 0 h.last None;
    h.last <- empty

  let iter f { data; _ } =
    Array.to_seq data
    |> Seq.filter_map Fun.id
    |> Seq.iter f

  let fold f x { data; _ } =
    Array.fold_left
      (fun p n -> match n with
        | None -> p
        | Some x -> f p x)
      x
      data
end
