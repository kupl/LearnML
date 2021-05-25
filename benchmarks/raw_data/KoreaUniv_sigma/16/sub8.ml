let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 0;; 
(*match f with
 (fun x -> x) -> (if a <= b then a + (sigma f (a+1) b) else 0)
|(fun x -> x*x) -> (if a <= b then a * (sigma f (a+1) b) else 0)
| 0;;*)
