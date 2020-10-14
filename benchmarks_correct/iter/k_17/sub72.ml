(* problem 3*)
let rec iter : int * (int -> int) -> (int -> int) =
  fun (n,f) -> if n = 0 then (fun x -> x) 
  else let g = iter(n-1, f) in (fun x -> f(g(x)));;