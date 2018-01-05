exception FreeVariable
type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp
let rec getfun e t =
	match e with
	| X -> t	
	| INT x	-> (float)x
	| REAL x ->  x
	| ADD (x,y) -> 
		(match (x,y) with 
			| (X,y)->t +. (getfun y t)
			| (x,X)->(getfun x t) +. t
			| (X,X)->t +. t
			| (a,b)->(getfun a t) +.(getfun b t)
		)	
        | SUB (x,y) -> 
		 (match (x,y) with
                        | (X,y)->t -. (getfun y t)
                        | (x,X)->(getfun x t) -. t
                        | (X,X)->t -. t
                        | (a,b)->(getfun a t) -.(getfun b t)
                )

        | MUL (x,y) -> 
		 (match (x,y) with
                        | (X,y)->t *. (getfun y t)
                        | (x,X)->(getfun x t) *. t
                        | (X,X)->t *. t
                        | (a,b)->(getfun a t) *.(getfun b t)
                )

        | DIV (x,y) -> 
		 (match (x,y) with
                        | (X,y)->t /. (getfun y t)
                        | (x,X)->(getfun x t) /. t
                        | (X,X)->t /. t
                        | (a,b)->(getfun a t) /.(getfun b t)

				)
let rec sigma (a,b,func,sum) =
        if a>b then sum 
        else
                sigma((a +1),b,func,sum+.(func (float_of_int a)))

let rec otsigma (a,b,func,sum) =
        if a+.0.1>b then sum 
        else
                otsigma((a +. 0.1),b,func,sum+.((func a)*.0.1))
	
let rec galculator (e: exp) : float =
        match e with
        | X -> raise FreeVariable
        | INT x -> (float)x
        | REAL x -> x
        | ADD (x,y) -> (galculator x) +. (galculator y)
        | SUB (x,y) -> (galculator x) -. (galculator y)
        | MUL (x,y) -> (galculator x) *. (galculator y)
        | DIV (x,y) -> (galculator x) /. (galculator y)
        | SIGMA (a,b,f) -> (match (a,b) with
                                | (X,_) -> raise FreeVariable
                                | (_,X) -> raise FreeVariable
                                | (_,_) ->
			let func= getfun f 
			in
			let init= galculator a in
			let fin= galculator b in
                                        sigma((int_of_float init),(int_of_float fin),func,0.0)
                                )
	| INTEGRAL (a,b,f) -> (match (a,b) with
                                | (X,_) -> raise FreeVariable
                                | (_,X) -> raise FreeVariable
                                | (_,_) ->
                        let func= getfun f
                        in
                        let init=galculator a in
                        let fin=galculator b in
			if a>b then (-1.0) *. otsigma(fin,init,func,0.0) else
                                        otsigma(init,fin,func,0.0)
                                )
 
