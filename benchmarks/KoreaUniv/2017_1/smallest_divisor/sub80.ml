(* problem 2*)

let smallest_divisor : int -> int
= fun n -> for i=2 to sqrt n do
  if n mod i = 0 then i;; 