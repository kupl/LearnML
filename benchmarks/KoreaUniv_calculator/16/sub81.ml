exception NotImplemented
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
