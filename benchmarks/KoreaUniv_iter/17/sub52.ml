(* problem 3*)

let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
  let rec iter_inter (n,f) x =
  if n = 0 then x
  else if n = 1 then (f x) 
  else (f x) * (iter_inter ((n-1),f) x) in
  iter_inter (n,f)