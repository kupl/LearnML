type exp =
    X
	|INT of int
	|REAL of float
	|ADD of exp * exp
	|SUB of exp * exp
	|MUL of exp * exp
	|DIV of exp * exp
	|SIGMA of exp * exp * exp
	|INTEGRAL of exp * exp *exp

let rec applyX : float*exp -> exp=
    fun (n,e)->
	    match e with
		|X -> REAL n
		|INT m -> INT m
		|REAL fl -> REAL fl
		|ADD (lexp,rexp)-> ADD ((applyX (n,lexp)),(applyX (n,rexp))) 
		|SUB (lexp,rexp) -> SUB ((applyX (n,lexp)),(applyX (n,rexp)))
		|MUL (lexp,rexp) -> MUL ((applyX (n,lexp)),(applyX (n,rexp)))
		|DIV (lexp,rexp) -> DIV ((applyX (n,lexp)),(applyX (n,rexp)))
	    |SIGMA (start_exp, end_exp, body_exp) -> SIGMA ((applyX (n,start_exp)),(applyX (n,end_exp)),body_exp)
        |INTEGRAL (start_exp,end_exp,body_exp) -> INTEGRAL((applyX (n,start_exp)),(applyX (n,end_exp)),body_exp)
let applyXint : int*exp -> exp=
    fun (n,e)->applyX (float_of_int n,e)

exception FreeVariable

let rec galculator : exp -> float=
    fun e->
	    match e with
		|X -> raise FreeVariable
		|INT n -> float_of_int n
		|REAL fl -> fl
		|ADD (lexp,rexp) -> (galculator lexp) +. (galculator rexp)
		|SUB (lexp,rexp) -> (galculator lexp) -. (galculator rexp)
		|MUL (lexp,rexp) -> (galculator lexp) *. (galculator rexp)
		|DIV (lexp,rexp) -> (galculator lexp) /. (galculator rexp)
	    |SIGMA (start_exp, end_exp, body_exp) -> 
            let start_num = int_of_float (galculator start_exp) in
	        let end_num = int_of_float (galculator end_exp) in
	        if start_num<=end_num 
               then (galculator (applyXint(start_num,body_exp))) +. (galculator (SIGMA((INT (start_num+1)),(INT end_num),body_exp)))
	        else 0.0
		|INTEGRAL (start_exp, end_exp, body_exp) ->
		    let start_num = (galculator start_exp) in
			let end_num = (galculator end_exp) in
			if end_num-.start_num>=0.1
			   then ((galculator (applyX(start_num,body_exp)))*.0.1) +. (galculator (INTEGRAL (REAL (start_num+.0.1),(REAL end_num),body_exp)))
	        else 0.0
