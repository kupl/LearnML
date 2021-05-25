let rec iter : int * (int -> int) -> (int -> int)
= fun (n, f) -> 
  let composite a b = fun x -> a (b x) in
  let rec repeat c n = 
    if n <= 0 then fun x -> x
    else composite c (repeat c (n-1)) 
  in
  match n with
    0 -> fun x -> x
    |_ -> repeat f n;; 
    
iter(0, fun x -> x+2) 0;;
iter(3, fun x -> x*2) 2;;
iter(5, fun x -> x+2) 0;;
