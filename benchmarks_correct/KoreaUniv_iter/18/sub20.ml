let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> let rec temp_f cnt f a = if cnt <=0 then a else f(temp_f (cnt-1) f a) in temp_f n f ;;

iter(11, fun x->x+2) 0 ;;
iter(3, fun x->x*x) 2;;