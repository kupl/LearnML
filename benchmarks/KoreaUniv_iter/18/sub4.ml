let identity = fun x -> x;;

let compose = fun f g x -> f(g(x));;

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
  if n = 0 then identity
  else compose f (iter(n-1, f));;
  
  
iter(5, fun x -> 2+x) 0;;
iter(10, fun x -> x*2) 1;;
