exception Illegal_input

(* Problem 1 *)
let rec fib : int -> int
= fun n -> (* TODO *)
	if n<0 then raise Illegal_input
	else if n=0 then 0
	else if n=1 then 1
	else fib (n-1) + fib (n-2)


(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) ->  (* TODO *)
	if n1<n2 then raise Illegal_input
	else if n2<0 then raise Illegal_input
	else if n1<0 then raise Illegal_input
	else if n1=0 then 1
	else if n2=0 then 1
	else if n1=n2 then 1
	else pascal (n1-1,n2-1) + pascal (n1-1,n2)


(* Problem 3 *)
let rec check : int * int -> bool
= fun (n, p) ->
	if p*p>n then true
	else if n mod p = 0 then false
	else check (n, p+1)

let rec prime : int -> bool
= fun n -> (* TODO *)
	if n<=0 then raise Illegal_input
	else if n=1 then false
	else check (n, 2)

(* Problem 4 *)
let rec sum : (int -> int) -> int -> int -> int
= fun f a b ->
	if a>b then 0
	else f a + sum f (a+1) (b)

let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->  (* TODO *)
	if (a)>(b) then raise Illegal_input
	else sum f a b

