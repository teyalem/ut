(* neighborhoods of 2D matrix *)

type neighbors = (int * int) list

let von_neumann = [ -1, 0; 0, -1; 1, 0; 0, 1 ]

let moore = [
  -1, -1;  0, -1;  1, -1;
  -1,  0;          1,  0;
  -1,  1;  0,  1;  1,  1; ]

let hex = [ 0, -1; 1, -1; -1, 0; 1, 0; -1, 1; 0, 1; ]

let neighbors neigh (x, y) =
  neigh |> List.map (fun (dx, dy) -> x+dx, y+dy)
