(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n = 0 then 0 else if n = 1 then 1 else fib(n - 1) + fib (n - 2);;
(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n2 = 0 || n1 = n2 then 1
else pascal(n1 - 1, n2 - 1) + pascal(n1 - 1, n2);;

(* Problem 3 *)
let rec prime : int -> bool = fun n ->
        let rec sub_prime d
            = if n = 0 || n = 1 then false 
        		else if d * d > n then true
                else if n mod d = 0 then false
                    else sub_prime (d + 1) in
                        sub_prime 2;;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a = b then f b
else sigma f a (b - 1) + sigma f b b;;


