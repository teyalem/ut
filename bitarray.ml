type t = int ref

let get bs i = (!bs lsr i) land 0x1

let set bs i n =
  if n = 0 || n = 1 then
    let mask = 0x1 lsl i in
    bs := (if n = 0
           then !bs land lnot mask
           else !bs lor mask)
  else
    raise (Invalid_argument "Bitarray.set")

let copy bs = ref !bs

let of_int = ref
let to_int = (!)
