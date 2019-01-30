let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> (*TODO*)
if n > 0 then fun x ->f (iter (n-1, f) x)
else fun x-> x ;;
