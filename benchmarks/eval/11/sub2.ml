type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list

let rec eval e = 
	let rec find_max (max_i,input_list) = 
		match input_list with
		| []	-> max_i
		| a::l	-> 
			if (eval(a)>max_i) then find_max(eval(a),l)
			else find_max(max_i,l)
	in
		match e with
		| NUM(i)	-> i
		| PLUS(e1,e2)	-> eval(e1) + eval(e2)
		| MINUS(e1,e2)	-> eval(e1) - eval(e2)
		| MULT(e1,e2)	-> eval(e1) * eval(e2)
		| DIVIDE(e1,e2)	-> eval(e1) / eval(e2)
		| MAX([])	-> 0
		| MAX(a::l)	-> find_max(eval(a),l)
