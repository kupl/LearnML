type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list
exception DividebyZero
let rec eval exp =
	let comp x y = if (eval x) > (eval y) then x else y in
	match exp with
	NUM(i)->i
	|PLUS (e1,e2)->(eval e1) + (eval e2)
	|MINUS (e1,e2)->(eval e1) - (eval e2)
	|DIVIDE (e1,e2)->if (eval e2)=0 then raise DividebyZero
					 else (eval e1) / (eval e2)
	|MULT (e1,e2)->(eval e1) * (eval e2)
	|MAX l -> if l = [] then 0 else (eval (List.fold_right comp l (List.hd l)))

