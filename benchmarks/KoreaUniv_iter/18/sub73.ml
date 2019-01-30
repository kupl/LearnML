let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->  fun x -> if n=0 then x  else f(iter ((n-1), f))in iter(n,f) ;;

iter(3,fun x ->x+2) ;;
