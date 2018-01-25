(* problem 2*)
let smallest_divisor : int -> int
= fun n ->
  let rec test a n =
    if n mod a = 0 then a
    else test (a+1) n in
    test 2 n