
(* problem 4*)

let product : (int -> int) -> int -> int -> int
= fun f a b -> (*TODO*)
  let rec impl n =
    if n == b then (f b)
    else (f n) * (impl (n+1)) in
  impl a;;
