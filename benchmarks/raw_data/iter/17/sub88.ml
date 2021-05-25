let rec iter (n, f) x =
  if n=0 then x
  else iter (n-1, f) (f x)
(*
let _ = print_int(iter(3, function x -> 2*x) 3); print_newline()

let a31 = iter (3, function x -> 2+x) 0
let a32 = iter (0, function x -> 2*x) 4
let a33 = iter (11, function x -> 2*x+1) 7

let _ = print_int a31; print_int a32; print_int a33*)
