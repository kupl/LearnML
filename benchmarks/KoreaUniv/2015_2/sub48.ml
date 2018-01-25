(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = [];;
let rec filter pred lst =
				match lst with
				[] -> [] |
				h::t -> if (pred h) then h::(filter pred t)
														else (filter pred t);;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> [];;
let rec zipper (l1, l2) =
				match (l1, l2) with
				([],[]) -> [] |
				(_,[]) -> l1 |
				([],_) -> l2 |
				(h1::t1,h2::t2) -> [h1;h2] @ (zipper (t1,t2));;

(*******************)
(* Problem 3: iter *)
(*******************)
let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> f;;

let rec iter (n,f) =
					if n=0 then (fun x->x)
					else (fun x->(iter (n-1,f) (f x)));;

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> aexp;;

let rec diff (aexp, x) =
				match aexp with
				Const (a) -> Const 0 |
				Var (s) -> if s=x then Const 1 else Const 0 |
				Power (s,a) -> if a=1 then Const 1
															else Times [Const a; Power (s,a-1)] |
				Times lst -> (match lst with
									[] -> Const 0 |
									h::t -> if t = [] then Sum [Times [(diff (h,x)); Const 1]; Times [h;(diff (Times t,x))]]
											else Sum [Times [(diff (h,x)); Times t]; Times [h;(diff (Times t,x))]]) |

				Sum lst2 -> match lst2 with
										[] -> Const 0 |
										h::t -> Sum [(diff (h,x));(diff (Sum t,x))];;			


(*************************)
(* Problem 5: Calculator *)
(*************************)

type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
let calculator : exp -> int
=fun e -> 0;;

let calculator e =
		let rec oper t =
			(match t with
			X -> raise (Failure "Freevalue") |
			INT n1 -> n1 |
			ADD (n1, n2) -> (oper n1) + (oper n2) |
			SUB (n1, n2) -> (oper n1) - (oper n2) |
			MUL (n1, n2) -> (oper n1) * (oper n2) |
			DIV (n1, n2) -> (oper n1) / (oper n2) |
			SIGMA (n1, n2, n3) ->
                let rec cal k =
                   (match k with
                   X -> (fun x->x) |
                   INT i -> (fun x->i) |
                   ADD (p,q) -> (fun x->((cal p) x)+((cal q) x)) |
                   SUB (p,q) -> (fun x->((cal p) x)-((cal q) x)) |
                   MUL (p,q) -> (fun x->((cal p) x)*((cal q) x)) |
                   DIV (p,q) -> (fun x->((cal p) x)/((cal q) x)) |
                   SIGMA (p,q,r) -> raise (Failure "impossible"))
								 in
                 let rec sigma v1 v2 =
                   if v1>v2 then 0
                   else if v1=v2 then ((cal n3) v1)
                   else (((cal n3) v1) + (sigma (v1+1) v2))
                 in (sigma (oper n1) (oper n2)))
		in
		match e with
		X -> 0 |
		INT a -> a |
		ADD (a,b) -> (oper (ADD (a,b))) |
		SUB (a,b) -> (oper (SUB (a,b))) |
		MUL (a,b) -> (oper (MUL (a,b))) |
		DIV (a,b) -> (oper (DIV (a,b))) |
		SIGMA (a,b,c) -> (oper (SIGMA (a,b,c)));;

