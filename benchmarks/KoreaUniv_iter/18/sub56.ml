let rec iter f n x =
  if n <= 0 then
    x
  else
    iter f (n-1) (f x);;

iter(fun x -> 2 + x) 0 0;;
iter(fun x -> 2 + x) 2 2;;
iter(fun x -> 2 * x) 3 3;;


(* identity function f(x) = x *)