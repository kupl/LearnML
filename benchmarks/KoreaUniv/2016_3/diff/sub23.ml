(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

(*********************)
(*     Problem 1     *)
(*********************)
module Problem1 = struct
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> 
match exp with
Const a -> Const 0
| Var x -> if x=var then Const 1 else Var x
| Power (x, set) -> if x = var then Times[Const set; Power (x,set-1)] else Power(x,set)
| Times l -> begin  match l with [] -> Const 1 
						 | [e] -> diff (e, var)
						 | hd::tl -> Sum ( Times (diff(hd,var)::tl) :: Times(hd::diff(Times(tl),var)::[])::[])end
| Sum l -> match l with
					[] -> Const 0
					| [e] -> diff (e, var)
					| hd::tl -> Sum (diff(hd, var)::diff(Sum(tl),var)::[]) (* TODO *)
end

(*********************)
(*     Problem 2     *)
(*********************)
module Problem2 = struct
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
	and weight = int

	let rec sumweight : mobile -> int
	= fun mob -> 
match mob with
(SimpleBranch (_,a) , SimpleBranch(_,b)) -> a + b
|(SimpleBranch (_,a), CompoundBranch (_,b)) -> a + sumweight b
|(CompoundBranch(_,a), SimpleBranch(_,b)) -> sumweight a + b
| (CompoundBranch(_,a), CompoundBranch(_,b)) -> sumweight a + sumweight b
  let rec  balanced : mobile -> bool
  = fun mob ->
match mob with
(SimpleBranch (a,b) , SimpleBranch (c,d)) -> if a*b = c*d then true else false
|(SimpleBranch (a,b) , CompoundBranch (c,d)) -> if a*b = c * (sumweight d) && balanced d then true else false
|(CompoundBranch (a,b) , SimpleBranch (c,d)) -> if a*(sumweight b) = c * d && balanced b then true else false
|(CompoundBranch(a,b) , CompoundBranch (c,d)) -> if a * (sumweight b) = c * (sumweight d) && balanced b && balanced d then true else false (* TODO *)
end

(*********************)
(*     Problem 3     *)
(*********************)
module Problem3 = struct
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

	exception No_Value
	let rec eval : exp * exp -> int
	= fun (exp, num) ->
	match exp with
	X -> eval(num, num)
	|INT a -> a
	|ADD(a,b) -> eval(a,num) + eval(b, num)
	|SUB(a,b) -> eval(a,num) - eval(b, num)
	|MUL(a,b) -> eval(a,num) * eval(b, num)
	|DIV(a,b) -> eval(a,num) / eval(b, num)
	|SIGMA (a,b,c) -> if eval(X,a) = eval(X,b) then eval (c,a) else eval(SIGMA(a, SUB(b, INT 1), c), INT 0) + eval(c, b)
  let calculator : exp -> int
  = fun exp -> if exp = X then raise No_Value else eval(exp,X)(* TODO *)
end

(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
	and var = string

	let rec save : exp -> var list
	= fun exp ->
	match exp with
	V a -> []
 |P (a,b) -> a::[]
 |C (a,b) -> save(a) @ save(b)

	let rec search : exp * var list -> bool
	= fun (exp, all) ->
match exp with
	V a ->begin  match all with [] -> false |  hd::tl  -> if hd = a then true else search (V a, tl) end
	|P(a,b) -> search (b, (a::all))
	|C(a,b) -> search(a,all) && search(b, all)

	let rec check : exp -> bool
  = fun exp -> 
	match exp with
	V a -> false
	|P (a , b) ->  search(b, a::[])
	|C (a , b) -> check (a) && check (b)(* TODO *)
end

