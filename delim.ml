(* DELIMITER OPERATIONS *)
let split pat str = Str.(split (regexp pat) str)
let split_line str = Str.(split (regexp "\n") str)
