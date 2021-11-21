(* Complete IntCode VM *)

(* states of VM *)
type state = Running (* Running normally *)
           | Paused (* Pause for some reason (e.g. program is in need of input) *)
           | Halt (* Program ended *)

(* IntCode VM *)
type t = { mutable pc: int; (* instruction pointer *)
           mutable rel_base: int; (* relative base register *)
           mutable state: state; (* state indicator *)
           mutable interact: bool; (* interactive mode switch *)
           mutable input: int Queue.t; (* Input queue *)
           mutable output: int Queue.t; (* Output queue *)
           mutable mem: int array; (* program memory *)
           mutable ex_mem: (int, int) Hashtbl.t; } (* extended memory *)

(* make machine and load program *)
let load ns = { pc = 0;
                rel_base = 0;
                input = Queue.create ();
                state = Paused;
                interact = false;
                output = Queue.create ();
                mem = Array.of_list ns;
                ex_mem = Hashtbl.create 100; }

(* copy machine *)
let copy m = { pc = m.pc;
               rel_base = m.rel_base;
               input = Queue.copy m.input;
               state = m.state;
               interact = m.interact;
               output = Queue.create ();
               mem = Array.copy m.mem;
               ex_mem = Hashtbl.copy m.ex_mem; }

(* return true if machine is halted *)
let is_halt m = m.state = Halt

(* return length of loaded code *)
let code_length m = Array.length m.mem

(* switch interactive mode *)
let set_interactive m b = m.interact <- b

(*
 * I/O
 *)

let is_output_empty m = Queue.is_empty m.output

let set_input m n = Queue.push n m.input
let get_output m = Queue.pop m.output

(* flush ASCII and return first non-ASCII output *)
let rec flush_output m =
  let o = get_output m in
  if o > 127
  then o
  else flush_output m

let rec collect_output m =
  if is_output_empty m
  then []
  else get_output m :: collect_output m

let print_output m =
  while not @@ is_output_empty m do
    print_char @@ char_of_int @@ get_output m
  done

(* get/set *)
let get m i =
  if i < Array.length m.mem
  then m.mem.(i)
  else Hashtbl.find_opt m.ex_mem i
       |> Option.value ~default: 0

let set m i n =
  if i < Array.length m.mem
  then m.mem.(i) <- n
  else Hashtbl.add m.ex_mem i n

(* indirect get/set *)
let iget m i = get m @@ get m i
let iset m i n = set m (get m i) n

(* relative get/set *)
let rget m i = get m (get m i + m.rel_base)
let rset m i n = set m (get m i + m.rel_base) n

let fetch m = m.mem.(m.pc)

(* decode instruction *)
let decode inst =
  let op, inst = inst mod 100 , inst/100 in
  let m1 = inst mod 10 in
  let m2 = inst mod 100 / 10 in
  let m3 = inst mod 1000/ 100 in
  op, m1, m2, m3

(* load integer from memory *)
let load_i m mode off =
  match mode with
  | 0 -> iget m (m.pc + off) (* position mode *)
  | 1 -> get m (m.pc + off) (* immediate mode *)
  | 2 -> rget m (m.pc + off) (* relative mode *)
  | _ -> assert false

(* store integer to memory *)
let store_i m mode off i =
  match mode with
  | 0 -> iset m (m.pc + off) i
  | 2 -> rset m (m.pc + off) i
  | _ -> assert false

let set_pc m i = m.pc <- i
let offset_pc m off = set_pc m (m.pc + off)

(* for four code *)
let execute_4 m f m1 m2 m3 =
  let s1 = load_i m m1 1
  and s2 = load_i m m2 2 in
  store_i m m3 3 @@ f s1 s2;
  offset_pc m 4

(* execute jump instruction *)
let jump m f m1 m2 =
  if f @@ load_i m m1 1
  then set_pc m @@ load_i m m2 2
  else offset_pc m 3

(* execute set compare instruction *)
let set_compare m f m1 m2 m3 =
  let s a b = if f a b then 1 else 0 in
  execute_4 m s m1 m2 m3

(* run a step of an IntCode Computer. returns offset for program counter to
 * move. *)
let step m =
  let inst = fetch m in
  let op, m1, m2, m3 = decode inst in
  match op with
  | 1 -> execute_4 m Int.add m1 m2 m3 (* ADD *)
  | 2 -> execute_4 m Int.mul m1 m2 m3 (* MUL *)

  | 3 -> (* Input *)
      if Queue.is_empty m.input
      then m.state <- Paused (* wait for input *)
      else begin
        let input = Queue.pop m.input in
        store_i m m1 1 input;
        offset_pc m 2
      end

  | 4 -> (* Output *)
    Queue.push (load_i m m1 1) m.output;
    offset_pc m 2

  | 5 -> jump m ((<>) 0) m1 m2 (* jump if true *)
  | 6 -> jump m ((=) 0) m1 m2 (* jump if false *)

  | 7 -> set_compare m (<) m1 m2 m3 (* less then *)
  | 8 -> set_compare m (=) m1 m2 m3 (* equals *)

  | 9 -> (* adjust relative base *)
    m.rel_base <- m.rel_base + load_i m m1 1;
    offset_pc m 2

  | 99 -> m.state <- Halt (* Program Termination *)
  | _ -> raise (Invalid_argument "step")

let rec run m =
  m.state <- Running; (* resume machine *)

  while m.state = Running do
    (* debug *)
    (*
    Printf.printf "%d %d %d %d\n" (get m m.pc) (load_i m 1 1) (load_i m 1 2) (load_i m 1 3);
    ignore @@ read_line ();
       *)
    step m;

    if m.interact && not @@ is_output_empty m
    then
      let output = get_output m in
      try char_of_int output |> print_char
      with _ -> begin
          m.state <- Halt;
          Queue.push output m.output
        end
    else ()
  done;

  if m.interact && m.state = Paused
  then begin
    read_line ()
    |> (fun s -> s ^ "\n")
    |> String.iter (fun c -> set_input m @@ int_of_char c);
    run m
  end
  else ()

(* parse intcode *)
let parse_code str =
  Delim.split "[,\n]" str |> List.map int_of_string
