(* Problem 1 *)
let rec fib : int -> int
= fun n -> match n with 0 | 1 -> n | _ -> fib (n-1) + fib (n-2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n2 = 0 || n1 = n2 then 1 else pascal (n1-1,n2-1) + pascal (n1-1,n2)

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> let rec nd i = i * 2 > n || (nd (i+1) && n mod i <> 0) in n <> 0 && n <> 1 && nd 2

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a = b then f a else f a + sigma f (a+1) b


