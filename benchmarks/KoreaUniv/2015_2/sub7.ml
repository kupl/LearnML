(*********************)
(* Problem 1: filter *)
(*********************)

let rec filter pred lst = 
	match lst with
	| [] -> raise (Failure "empty list")
	| [a] -> if pred(a) then [a]
			 else []
	|hd::tl -> if pred(hd) then hd::(filter pred tl)
			   else filter pred tl
;;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) ->
	match a with
	| [] -> b
	| a_hd::a_tl -> 
		(match b with
		 | [] -> a
		 | b_hd::b_tl -> a_hd::b_hd::(zipper(a_tl, b_tl))
		)
;;
(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
	match n with
	| 0 -> fun x -> x
	| 1 -> f
	|_ -> fun x -> f (iter(n-1, f) x)
;;

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
	| Const (i) -> Const (0)
	| Var (s) -> 
		if s = x then Const (1)
		else Var (s)
	| Power (s, i) -> 
		if s = x then Times ([Const (i); Power(s, i-1)])
		else Power (s, i)
 (*Times는 반드시 [계수;aexp] 구조를 갖는다고 가정  *)
	| Times (hd::hd'::tl) -> Times (hd::diff(hd', x)::[])
	| Sum (li) -> let rec diffForExpList : aexp list -> aexp list
						= fun (expList) ->
							match expList with
							| [] -> raise (Failure "empty list")
							| [one] -> diff(one, x)::[]
							| hd::tl -> diff(hd, x)::diffForExpList(tl)
					in Sum (diffForExpList(li))
;;

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

let rec calculator : exp -> int
=fun e ->
	match e with
	| X -> raise (Failure "not applicable")
	| SIGMA(e1, e2, e3) ->
		let rec evaluateExpInSig : exp -> int -> int
				= fun expWithX xValue ->
				match expWithX with
				| X -> xValue
				| INT (i) -> i
				| ADD (e1, e2) -> (evaluateExpInSig e1 xValue) + (evaluateExpInSig e2 xValue)
				| SUB (e1, e2) -> (evaluateExpInSig e1 xValue) - (evaluateExpInSig e2 xValue)
				| MUL (e1, e2) -> (evaluateExpInSig e1 xValue) * (evaluateExpInSig e2 xValue)
				| DIV (e1, e2) -> (evaluateExpInSig e1 xValue) / (evaluateExpInSig e2 xValue)
			(*  | DIV (e1, e2) -> (evaluateExpInSig(e1, xValue)) / (evaluateExpInSig(e2, xValue)) 
				 이렇게 하면 오류남... ㅠㅠㅠㅠㅠㅠㅠㅠㅠㅠ 왜인지 질문해서 확실하게 알자*)
				| SIGMA (e'1, e'2, e'3) -> raise (Failure "nested sigma is not applicable for a expression with only one variable \"X\"")
				
			in let rec calcSum : int-> int-> exp -> int
						= fun start en expWithX ->
							if start > en then 0
							else (evaluateExpInSig expWithX start) + (calcSum (start+1) en expWithX)
					in (calcSum (evaluateExpInSig e1 0) (evaluateExpInSig e2 0) e3)
								                    (*second parameter 0 is meaningless here*)
	| INT (i) -> i
	| ADD (e1, e2) -> calculator(e1) + calculator(e2)
	| SUB (e1, e2) -> calculator(e1) - calculator(e2)
	| MUL (e1, e2) -> calculator(e1) * calculator(e2)
	| DIV (e1, e2) -> calculator(e1) / calculator(e2)
;;

(* test
diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1], "x");;
diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1], "y");;
diff(Sum [ Sum [Power ("x", 4); Times [Const 2; Var "x"]; Const 1];
	  Sum [ Times [ Const 2; Power ("x", 3) ]; Const 1];
	  Sum [Power ("x", 2)]], "x");;
calculator (SIGMA(INT 9, INT 10, SUB(MUL(X, X), INT 1))) ;;
*)