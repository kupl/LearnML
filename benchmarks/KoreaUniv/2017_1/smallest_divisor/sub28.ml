(* problem 2. *)

let rec smallest_divisor : int -> int
= fun n ->

let rec find_divisor : int -> int -> int
= fun a b ->
if a mod b = 0 then b
else find_divisor a (b+2)

in
if n = 0 then 0
else if n mod 2 = 0  then 2
else find_divisor n 3