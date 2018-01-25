(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
match lst with
| []-> lst
| h::t -> if pred h = true then h::filter pred t else filter pred t

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match (a,b) with
| [] , b -> b
| a , [] -> a
| h1::t1 , h2::t2 -> if h1<h2 then h1::zipper (t1,b) else if h1=h2 then h1::zipper (t1,t2) else h2::zipper (a,t2)

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) x -> if n=0 then x else iter (n-1,f)(f x)  
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
=fun (aexp,x) -> match aexp with
| Const n-> Const 0
| Var v-> if v=x then Const 1 else Const 0 
| Power (v,n) -> if v=x then Times [Const n;Power(v,n-1)] else Const 0
| Times l->(match l with 
	| []-> Const 0
	| h::t -> Sum[Times (diff (h,x)::t); Times(h::diff(Times t,x)::[])]) 
| Sum l->let rec f l ll= (match l with
	| [] -> Sum ll
	| h::t-> let ll=ll@((diff(h,x))::[]) in f t ll) in f l []
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
=fun e ->  let rec eval e v b n = (match e with
	| X -> if b = 0 then raise (Failure "Failed") else n
	| INT n -> n
	| ADD (e1,e2) -> (eval e1 v b n) + (eval e2 v b n)
	| SUB (e1,e2) -> (eval e1 v b n) - (eval e2 v b n)
	| MUL (e1,e2) -> (eval e1 v b n) * (eval e2 v b n)
	| DIV (e1,e2) -> if (eval e2 v b n) = 0 then raise (Failure "Failed") else (eval e1 v b n) / (eval e2 v b n)
	| SIGMA (e1,e2,e3) ->	 let e4 = eval e1 v b n in
				 let e5 = eval e2 v b n in
				 let n = e4 in
				 let b = 1 in	 (if e4>e5 then v else
                        	 let v = v + (eval e3 v b n) in	eval (SIGMA (INT (e4+1),INT e5,e3)) v b n) )  in eval e 0 0 0

