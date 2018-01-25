(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
  let square x = x*x in
  if (n <= 1) then b
  else if (n mod 2 = 0) then square(fastexpt b (n/2))
  else b*(fastexpt b (n-1))
;;