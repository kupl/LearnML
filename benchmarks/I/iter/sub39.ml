let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> fun x -> if n=1 || n=0 then f x else f (iter (n-1, f) x);;

(*
iter(5,fun x -> 2+x) 0;;
*)