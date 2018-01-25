(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n <= 1 then
        if n = 0 then 0 else 1
        else fib (n-1) + fib(n-2);;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n1 = 0 && n2 = 0 then 1 else
        if n1 < 0 || n2 < 0 then 0 else
                pascal (n1 - 1, n2) + pascal (n1 - 1, n2 - 1);;

(* Problem 3 *)
let prime = fun n -> let a = n in
        let rec isDivisable d = if d < 2 then false else
                                if (a mod d) = 0 then true || isDivisable (d-1) else
                                                        false || isDivisable (d-1) in
                if a<2 then false else not (isDivisable (a-1));;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a > b then -1 else
                        if a = b then f a else
                                sigma f a (b - 1) + f b;;
