exception NO_NEGATIVE_INTEGERS

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> 
if a > b then raise ANSWER_TO_BE_1
else
  let rec loop f a n =
    if n == b then f b
    else
      f n * loop f a (n+1) in loop f a a
