(* problem 4*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a = b then f a 
               else if a > b then 0
               else (f a) * (product f (a+1) b)

(* problem 5*)
let dfact : int -> int
= fun n -> if n mod 2 = 0 then (product (fun x -> x*2) 1 (n/2))
           else (product (fun x -> (x*2-1)) 1 ((n+1)/2))
