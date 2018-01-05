exception DividedByZero

type expr = NUM of int
		  | PLUS of expr * expr
		  | MINUS of expr * expr
		  | MULT of expr * expr
		  | DIVIDE of expr * expr
		  | MAX of expr list

let getMax lst =
 let rec getMax_sub (maxval, rest) =
  match rest with
  [] -> maxval
  |h::t -> (if h > maxval then (getMax_sub (h, t))
	    else (getMax_sub (maxval, t)))
 in
 match lst with
 [] -> 0
 |h::t -> (getMax_sub (h,t))


let rec eval exp =
 match exp with
 (NUM n) -> n
 |(PLUS (e1, e2)) -> (eval e1) + (eval e2)
 |(MINUS (e1, e2)) -> (eval e1) - (eval e2)
 |(MULT (e1, e2)) -> (eval e1) * (eval e2)
 |(DIVIDE (e1, e2)) -> (let eval1 = (eval e1) in
	                let eval2 = (eval e2) in
			if (eval2) = 0 then raise DividedByZero
			else (eval1 / eval2))
 |(MAX explist) -> (let evallist = List.map eval explist in
 		    (getMax evallist))
