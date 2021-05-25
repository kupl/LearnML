let compose f g = fun x -> f(g(x));;
(*val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun> *)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
  if n < 0 then raise (Failure "n is eumsu")
  else if n = 0 then (function x -> x)
  else compose f (iter (n-1, f));; (* (iter~) 이부분이 compose 함수의 g 부분*)
    
    
(*Test Case*)
(* iter(0, function x -> x + 2) 1 ;;*)