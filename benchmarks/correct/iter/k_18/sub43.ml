let rec 
iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> if n = 0 then fun k -> k else fun k -> iter (n-1, f) (f k);;

iter (7, fun x -> 2+x) 0;;
iter (7, fun x -> 3+x) 0;;