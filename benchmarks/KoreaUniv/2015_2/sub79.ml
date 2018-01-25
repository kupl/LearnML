(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst =
match lst with
[]-> []
|h::t-> if pred h then h::(filter pred t) else filter pred t

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) ->
match a with
[]-> b
|[x]-> x::b
|h::t-> h::(zipper (b,t))

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) ->
match n with
0-> (fun x-> x)
|_-> (fun x-> f (iter(n-1,f) x))

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
=fun (aexp,x) ->
match aexp with
Const i-> Const 0
|Var a-> if a="x" then Const 1 else Const 0
|Power (a,i)-> Times[Const i; Power (a,i-1)]
|Times (h::t)-> Times (h::[diff (Sum t,x)])
|Sum (h::t)-> Sum ((diff (h,x))::[diff (Sum t,x)])
|_-> Const 0


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

let rec replaceX : exp->int-> exp
= fun e n ->
match e with
|X -> INT n
|INT x-> INT x
|ADD(x,y)-> if x=X then (if y=X then ADD(INT n, INT n)
				else ADD(INT n, replaceX y n))
		   else (if y=X then ADD(replaceX x n, INT n)
				else ADD(replaceX x n, replaceX y n))
|SUB(x,y)-> if x=X then (if y=X then SUB(INT n, INT n)
				else SUB(INT n, replaceX y n))
		   else (if y=X then SUB(replaceX x n, INT n)
				else SUB(replaceX x n, replaceX y n))
|MUL(x,y)-> if x=X then (if y=X then MUL(INT n, INT n)
				else MUL(INT n, replaceX y n))
		   else (if y=X then MUL(replaceX x n, INT n)
				else MUL(replaceX x n, replaceX y n))
|DIV(x,y)-> if x=X then (if y=X then DIV(INT n, INT n)
				else DIV(INT n, replaceX y n))
		   else (if y=X then DIV(replaceX x n, INT n)
				else DIV(replaceX x n, replaceX y n))
|SIGMA(a, b, f)-> SIGMA(a, b, replaceX f n)

let rec calculator : exp -> int
=fun e->
match e with
|X-> raise (Failure "Unknown X!")
|INT x-> x
|ADD (x,y)-> (match x with
	    |INT a-> (match y with
		     INT b-> a+b
		     |_-> a+calculator y)
	    |_-> (match y with
		  INT b-> calculator x+b
		  |_-> calculator x+calculator y))
|SUB (x,y)-> (match x with
	    |INT a-> (match y with
		     INT b-> a-b
		     |_-> a-calculator y)
	    |_-> (match y with
		  INT b-> calculator x-b
		  |_-> calculator x-calculator y))
|MUL (x,y)-> (match x with
	    |INT a-> (match y with
		     INT b-> a*b
		     |_-> a*calculator y)
	    |_-> (match y with
		  INT b-> calculator x*b
		  |_-> calculator x*calculator y))
|DIV (x,y)-> (match x with
	    |INT a-> (match y with
		     INT b-> a/b
		     |_-> a/calculator y)
	    |_-> (match y with
		  INT b-> calculator x/b
		  |_-> calculator x/calculator y))
|SIGMA (a,b,f)-> if (calculator (SUB(b,a)))<0 then raise(Failure "Range Error")
     else if (calculator (SUB(b,a)))=0 then calculator (replaceX f (calculator a))
     else calculator (ADD( (replaceX f (calculator a)) , (SIGMA ((ADD(a,INT 1)),b,f)) ))

		
		