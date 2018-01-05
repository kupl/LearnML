type exp = X
| INT of int
| REAL of float
| ADD of exp*exp
| SUB of exp*exp
| MUL of exp*exp
| DIV of exp*exp
| SIGMA of exp*exp*exp
| INTEGRAL of exp*exp*exp;;

exception FreeVariable;;
exception InvalidSigma;;

let rec input expr jg =
 match expr with
 |INT i-> (float_of_int i)
 |REAL r -> r
 |X -> jg
 |ADD (n1,n2) -> (input n1 jg) +. (input n2 jg)
 |SUB (n1,n2) -> (input n1 jg) -. (input n2 jg)
 |MUL (n1,n2) -> (input n1 jg) *. (input n2 jg)
 |DIV (n1,n2) -> (input n1 jg) /. (input n2 jg)
 |SIGMA (n1,n2,n3) ->  if ((input n1 jg) = (float_of_int (int_of_float (input n1 jg)))) && ((input n2 jg) = (float_of_int (int_of_float (input n2 jg)))) then  let x = SIGMA (REAL (input n1 jg),REAL (input n2 jg),n3) in mathemadica x else raise InvalidSigma
 |INTEGRAL (n1,n2,n3) -> mathemadica (INTEGRAL (REAL (input n1 jg),REAL (input n2 jg), n3))

and mathemadica exp =
	let rec sigmaf f t expr = 
	 if f = t then (input expr f) else (input expr f) +. (sigmaf (f+.1.) t expr)
	and inf f t expr =
	 if f+.0.1 >= t then (t-.f)*.(input expr f) else 0.1*.(input expr f) +. (inf (f+.0.1) t expr) 
	in
match exp with
|INT i -> (float_of_int i)
|REAL r -> r
|X -> raise FreeVariable
|ADD (n1,n2) -> (mathenatica n1) +. (mathenatica n2)
|SUB (n1,n2) -> (mathenatica n1) -. (mathenatica n2)
|MUL (n1,n2) -> (mathenatica n1) *. (mathenatica n2)
|DIV (n1,n2) -> (mathenatica n1) /. (mathenatica n2)
|SIGMA (n1,n2,n3) -> if (m_int n1) > (m_int n2) then raise InvalidSigma else sigmaf (float_of_int (m_int n1)) (float_of_int (m_int n2)) n3
|INTEGRAL (n1,n2,n3) -> if(mathenatica n1) > (mathenatica n2) then 0. -. (inf (mathenatica n2) (mathenatica n1) n3) else inf (mathenatica n1) (mathenatica n2) n3

and m_int x =
match x with
|INT i -> i
|REAL _ -> raise InvalidSigma
|X -> raise FreeVariable
|ADD (n1,n2) -> (m_int n1) + (m_int n2)
|SUB (n1,n2) -> (m_int n1) - (m_int n2)
|MUL (n1,n2) -> (m_int n1) * (m_int n2)
|DIV (n1,n2) -> if (m_int n1) mod (m_int n2) = 0 then (m_int n1) / (m_int n2) else raise InvalidSigma
|SIGMA _ | INTEGRAL _ -> if (mathenatica x) = (float_of_int(int_of_float (mathenatica x))) then (int_of_float (mathenatica x)) else raise InvalidSigma;;
