type exp =
 X 
| INT of int 
| ADD of exp * exp 
| SUB of exp * exp 
| MUL of exp * exp 
| DIV of exp * exp 
| SIGMA of exp * exp * exp 
let calculator : exp -> int = fun exp -> 
	let rec cal exp num fl result = (
match exp with
X-> if fl = 0 then raise(Failure "match failure exception") else num
|INT(k) -> k
|ADD(x,y)-> (cal x num fl result) + (cal y num fl result)
|SUB(x,y)-> (cal x num fl result) - (cal y num fl result)
|MUL(x,y)-> (cal x num fl result)*(cal y num fl result)
|DIV(x,y)-> if (cal y num fl result) = 0 then raise(Failure "divide by zero") else (cal x num fl result)/(cal y num fl result)
|SIGMA(x,y,z)-> 
		let num = cal x num fl result in
			let fl = 1 in
				(if (cal x num fl result)>(cal y num fl result) then result
					else let result = result + (cal z num fl result) in
						cal (SIGMA(INT ((cal x num fl result)+1),INT (cal y num fl result), z)) num fl result)
) in cal exp 0 0 0;;
