type exp=X
		|INT of int
		|ADD of exp * exp
		|SUB of exp * exp
		|MUL of exp * exp
		|DIV of exp * exp
		|SIGMA of exp * exp * exp

let rec mecalculate
= fun (exp, x)->
			match (exp, x) with
			|(X, INT x)-> x
			|(INT k, _) -> k
			|(ADD (e1, e2),_)->
						(match e1, e2 with
						|INT k, INT n -> k+n
						|_,_ ->mecalculate (e1, x)+ mecalculate (e2, x))
			|(SUB (e1, e2),_)->
						(match e1, e2 with
						|INT k, INT n -> k-n
						|_, _ -> mecalculate (e1, x)- mecalculate (e2, x))
			|(MUL (e1, e2),_) ->
						(match e1, e2 with
						|INT x, INT y -> x*y
						|_,_ -> mecalculate (e1, x) * mecalculate (e2, x))
			|(DIV (e1, e2),_) ->
						(match e1, e2 with
						|INT x, INT y -> x/y
						|_,_ -> mecalculate (e1, x) / mecalculate (e2, x))
			|(SIGMA (exp1, exp2, exp3),_)->
						let a = mecalculate (e1, x) in
						let b = mecalculate (e2, x) in
						( if a<b then 0
							else if (a=b) then mecalculate (exp3, INT a)
							else mecalculate (exp3, INT a) + mecalculate (SIGMA (INT (a+1), INT b, exp3), x))

let calculator :exp -> int
=fun f-> mecalculate (f, X)
;;

