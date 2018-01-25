(* problem 1*)

let fastexpt : int -> int -> int
= fun b n -> let rec f b n = if n == 0 then 1
                             else if n mod 2 == 0 then let x = f b (n/2) in x*x
                             else b*f b (n-1) in
             f b n

(* problem 2*)

let smallest_divisor : int -> int
= fun n -> let est = 3 in
           let rec f n est = if n mod 2 == 0 then 2
                             else if est*est > n then n
                             else if n  mod est == 0 then est 
                             else f n (est+2) in
           f n est

(* problem 3*)

let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> fun x -> let rec g (n, f) = if n == 0 then x else f(g (n-1, f)) in
               g (n, f) 

(* problem 4*)

let product : (int -> int) -> int -> int -> int
= fun f a b -> let rec g a b f = if a == b then f (a) else f (a) * g (a+1) b f in
               g a b f

(* problem 5*)

let dfact : int -> int
= fun n -> let rec f n = if n == 1 || n == 2 then n 
                         else n*f (n-2) in
           f n

(* problem 6*)

let drop : 'a list -> int -> 'a list
= fun l n -> let rec f l n = if n == 0 then l 
                             else match l with
                                  |[] -> []
                                  |h::t -> f t (n-1) in
             f l n

(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> let l1 = [] in
             let l2 = [] in
	     let rec f = fun lst l1 l2 -> match lst with 
	                              |[] -> (l1, l2)
				      |h::t -> match h with
			                     |(x, y) -> f t (l1@[x]) (l2@[y]) in

             f lst l1 l2

(* problem 8*)

let change : int list -> int -> int
= fun coins amount -> let rec f coins amount = if amount == 0 then 1 
                                               else if amount < 0 then 0 
                                               else match coins with 
	                                            |[] -> 0
		                                    |h::t -> f coins (amount-h) + f t amount in
                      f coins amount
