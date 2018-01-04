type formula = TRUE
	     | FALSE
	     | NOT of formula
	     | ANDALSO of formula * formula
	     | ORELSE of formula * formula
	     | IMPLY of formula * formula
	     | LESS of expr * expr
and expr = NUM of int
	 | PLUS of expr * expr
	 | MINUS of expr * expr


let rec eval form = 
 (* calc : expr -> int ; expr을 하나의 int값으로 계산해주는 함수 *)
 let rec calc e = 
  match e with
   NUM n -> n
   |PLUS (e1, e2) -> ((calc e1) + (calc e2))
   |MINUS (e1, e2) -> ((calc e1) - (calc e2))
 in

 (* 함수 본문 *)
 match form with
  TRUE -> true
  |FALSE -> false
  |NOT f -> not (eval f)
  |ANDALSO (f1, f2) -> ((eval f1) &
  		        (eval f2))
  |ORELSE (f1, f2) -> ((eval f1) or
  		       (eval f2))
  |IMPLY (f1, f2) -> (if (((eval f1) = true) &
  			  ((eval f2) = false))
  		      then (false)
		      else (true)
  		     )
  |LESS (e1, e2) -> (if ((calc e1) < (calc e2))
  		     then (true)
		     else (false)
  		    )
;;
