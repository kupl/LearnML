type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp;;

let rec e_to_f : exp -> (int -> int)
= fun e ->
	match e with
| X -> (fun x -> x)
| INT n -> (fun x -> n)
| ADD (e1,e2) -> (fun x -> (((e_to_f e1) x) + ((e_to_f e2) x)))
| SUB (e1,e2) -> (fun x -> (((e_to_f e1) x) - ((e_to_f e2) x)))
| MUL (e1,e2) -> (fun x -> (((e_to_f e1) x) * ((e_to_f e2) x)))
| DIV (e1,e2) -> (fun x -> (((e_to_f e1) x) / ((e_to_f e2) x)))
| _ -> (fun x -> 0);;

let rec sigma : int * int * (int -> int) -> int
= fun (n1,n2,f) ->
	if n1=n2 then f n1
		else (f n1) + (sigma (n1+1,n2,f));;

let rec reculse : exp -> int
= fun e ->
match e with
	X -> raise (Failure "fail!!")
| INT n -> n
| ADD (INT n1,INT n2) -> n1+n2
| ADD (e1, e2) -> reculse e1 + reculse e2
| SUB (INT n1,INT n2) -> n1-n2
| SUB (e1, e2) -> reculse e1 - reculse e2
| MUL (INT n1,INT n2) -> n1*n2
| MUL (e1, e2) -> reculse e1 * reculse e2
| DIV (INT n1,INT n2) -> n1/n2
| DIV (e1, e2) -> reculse e1 / reculse e2
| SIGMA (INT n1, INT n2, e) -> sigma (n1 ,n2, (e_to_f e))
| SIGMA (e1, e2, e3) -> sigma (reculse e1, reculse e2, (e_to_f e3));;

let calculator : exp -> int
= fun e ->
	(match e with
	X -> 0
| INT n -> n
| ADD (e1,e2) -> (reculse e1 + reculse e2)
| SUB (e1,e2) -> (reculse e1 - reculse e2)
| MUL (e1,e2) -> (reculse e1 * reculse e2)
| DIV (e1,e2) -> (reculse e1 / reculse e2)
| SIGMA (e1,e2,e3) -> sigma (reculse e1, reculse e2, (e_to_f e3)));;

