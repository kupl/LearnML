(* Problem 1 *)
let rec fib : int -> int
= fun n -> match n with
| 0 -> 0
| 1 -> 1
| _ -> fib(n-1)+ fib(n-2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> match (n1, n2) with
| (_, 0) -> 1
| (0, _) -> 1
| _ -> if n1=n2 then 1 else pascal(n1-1, n2)+pascal(n1-1,n2-1)

(*
| _ -> pascal(n1-1, n2) + pascal(n1-1, n2-1) *)

let rec subtest : int * int -> bool
= fun (n1, n2) ->
	if n2*n2 > n1 then true
	else if n1 - n1/n2*n2 = 0 then false
	else subtest(n1, n2+1)

(* Problem 3 *)
let rec prime : int -> bool
(* = fun n -> let i = ref 1 -> let status = ref true ->
while !i < n and n % !i != 0 do
	i := !i + 1;
done -> match i with
| !n -> true
| _ -> false
*)
= fun n -> subtest(n, 2)


(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> match b-a with
| 0 -> f(a)
| _ -> f(a) + sigma f (a+1) b
