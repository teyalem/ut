(* Block for Sparse elements
 * Warning: some of the functions are not implemented. *)

open Block

module Make(Sign: SignType)
  : (S with type elt = Sign.t) =
struct
  type elt = Sign.t
  type pos = int * int
  type t = (pos, elt) Hashtbl.t

  let make len _ = Hashtbl.create len

  let get b x y =
    Hashtbl.find_opt b (x, y)
    |> Option.value ~default: Sign.default

  let set b x y c =
    Hashtbl.remove b (x, y);
    Hashtbl.add b (x, y) c

  let dimx b = Hashtbl.length b
  let dimy _ = assert false

  let sub _ _ _ = assert false

  let copy b = Hashtbl.copy b

  let iteri f b =
    Hashtbl.iter (fun (x, y) c -> f x y c) b

  (* FIXME: implement these *)
  let print _ = assert false
  let parse _ = assert false
  let parse_raw _ _ _ = assert false
    
  let count f b =
    Hashtbl.fold
      (fun _ c count -> count + if f c then 1 else 0)
      b 0
    
  let count_occur elt b =
    count ((=) elt) b

  let of_matrix _ = assert false
  let to_matrix _ = assert false
      
end
