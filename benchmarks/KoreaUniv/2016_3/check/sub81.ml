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
		|Const a->Const 0
		|Var str -> if str = var then Const 1 else Var str
		|Power(str,a) -> if str = var then Times[Const a;Power(str,(a-1))] else Power(str,a)
		|Times lst ->Sum (difftime(lst,var))
		|Sum  lst -> Sum (diffsum(lst,var))

and diffsum(lst,var) = 
		match lst with [] -> []
				|hd::tl -> [diff(hd,var)] @ diffsum(tl,var)
and difftime(lst,var) = 
		match lst with [] ->[]
				|hd::tl -> if tl = [] then [Times([diff2(hd,var)] @ tl)]
										 else ( [Times([diff2(hd,var)] @ tl)] @ [Times([hd] @[ Sum (difftime(tl,var))]) ])
and diff2 : aexp*string ->aexp
   = fun (hd,var) ->match hd with
		|Const a -> Const 0
		|Var str -> if str = var then Const 1 else Const 0
		|Power(str,a) -> if str = var then Times[Const a ; Power(str,(a-1))] else Const 0
		|Times lst -> diff(Times lst,var)
		|Sum lst -> diff(Sum lst,var) 
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

	let rec subweight mob = match mob with
		(SimpleBranch(a,b), SimpleBranch(c,d))->b+d
	|	(SimpleBranch(a,b), CompoundBranch(c,d))-> b+subweight(d)
	|	(CompoundBranch(a,b), SimpleBranch(c,d)) -> d+ subweight(b)
	|	(CompoundBranch(a,b), CompoundBranch(c,d)) -> subweight(b) + subweight(d)

  let rec balanced : mobile -> bool
  = fun mob ->match mob with
		(SimpleBranch(a,b), SimpleBranch(c,d))-> if (a*b) = (c*d) then true else false
	|	(SimpleBranch(a,b), CompoundBranch(c,d))->( if(a*b) = (c*subweight(d)) then true else false) &&balanced(d)
	|	(CompoundBranch(a,b), SimpleBranch(c,d)) ->( if (a*subweight(b)) = (c*d) then true else false) && balanced(b)
	|	(CompoundBranch(a,b), CompoundBranch(c,d)) ->( if (a*subweight(b)) = (c*subweight(d)) then true else false) &&balanced(b) &&balanced(d)
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

  let rec cal : exp * exp -> exp
					=fun(exp,value) ->
					match exp with
					X ->(match value with |INT a -> INT a |_ ->X)
					|INT a -> INT a
					|ADD(a,b) -> (match (a,b) with
											(INT aa, INT bb) -> INT (aa+bb)
											|_-> ADD( cal(a,value), cal(b,value) ))
					|SUB(a,b) ->( match (a,b) with
											(INT aa, INT bb) -> INT (aa-bb)
											|_-> SUB (cal(a,value), cal(b,value)))
					|MUL(a,b) -> (match (a,b) with
												(INT aa, INT bb) -> INT (aa*bb)
												|_-> MUL (cal(a,value), cal(b, value)))
					|DIV (a,b) -> (if b = INT 0 then raise NotImplemented else
															(match (a,b) with |(INT aa, INT bb) -> INT (aa / bb)
																								|_-> DIV(cal(a,value), cal(b,value))))
					|SIGMA (a,b,c) -> (match (a,b,c) with
																|(INT aa, INT bb, c) -> (sigma(aa,bb,c))
																|_-> (SIGMA(cal(a,value), cal(b,value),c)))
and sigma : int*int* exp ->exp
		= fun (a,b,c) ->
				if a=b then cal(c, INT b)
				else if a<b then cal(ADD(cal(c,INT a), sigma(a+1,b,c)), X)
				else INT 0
let rec calculator : exp -> int
= fun exp ->	match cal(exp,X) with
							|INT a -> a
							|_-> calculator (cal(exp,X))

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

let rec isin (str,expr) = match (str,expr) with
			|(str,V a) -> false
			|(str, P(a,b)) -> if a= str then true else isin(str,b)
			|(str, C(a,b)) ->false

let rec findvar exp expression = match exp with	
					|V var -> isin(var, expression)
					|P (var,expr) -> ((findvar expr expression) || (findvar expr exp))
					|C (expr1,expr2) -> ((findvar expr1 expression) ||( findvar expr1 expr1)) && ((findvar expr2 expression)||(findvar expr2 expr2))

  let rec check : exp -> bool
  = fun exp -> match exp with
				|V var ->false
				|P (var,expr) -> (findvar expr exp)
				|C(expr1,expr2) -> (check expr1) &&(check expr2)
end

