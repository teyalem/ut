type t  = int * int

let map f (x, y) = f x, f y

let map2 f (ax, ay) (bx, by) = f ax bx, f ay by

let fold f (x, y) = f x y

let add a b = map2 Int.add a b
let sub a b = map2 Int.sub a b
let mul a b = map2 Int.mul a b
let div a b = map2 Int.div a b

type direction = Left | Right | Up | Down
