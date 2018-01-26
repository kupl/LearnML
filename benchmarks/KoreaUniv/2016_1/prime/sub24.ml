let rec prime : int -> bool
= fun n -> let rec r :  int * int -> int = fun(n, k) -> if n = 1 then 1 else if n = 2 then 0 else if n mod k = 0 then 1 else if k = 2 then 0 else r(n, k-1) in if r(n, n-1) = 0 then true else false;; 
