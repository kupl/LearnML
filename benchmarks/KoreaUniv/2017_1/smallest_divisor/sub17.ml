(* problem 2*)

let smallest_divisor : int -> int
= fun n -> (*TODO*)
  let rec iterator i =
    if n mod i = 0 then i
    else if i * i > n then n
    else iterator (i + 1) in
  iterator 2;;