type expr = NUM of int
	  | PLUS of expr * expr
	  | MINUS of expr * expr
	  | MULT of expr * expr
	  | DIVIDE of expr * expr
	  | MAX of expr list

exception DivideByZero

(* eval : expr -> int *)
let rec eval e =
 (* getMax : int -> expr list -> int  - MAX를 계산해주는 함수 *)
 let rec getMax candit lst =
  match lst with
   h::t -> (let comp = (eval h) in
   	    if (candit>comp)
	    then (getMax candit t)
	    else (getMax comp t)
   	   )
   |[] -> candit
 in

 (* 함수 본문 *)
 match e with
  NUM i -> i
  |PLUS (e1,e2) -> ((eval e1) +
  		    (eval e2)
		   )
  |MINUS (e1,e2) -> ((eval e1) -
  		     (eval e2)
		    )
  |MULT (e1,e2) -> ((eval e1) *
  		    (eval e2)
		   )
  |DIVIDE (e1,e2) -> (let i1 = (eval e1) in
  		      let i2 = (eval e2) in
		      if (i2=0)
		      then (raise DivideByZero)
		      else (i1 / i2)
  		     )
  |MAX (h::t) -> (getMax (eval h) t)
  |MAX [] -> 0
