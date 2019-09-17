(*Exercise 8 Though wornning is exist, this program run well*)
exception Error of string
type lambda =
    V of string
  | P of string * lambda
  | C of lambda * lambda
                                                    
 let rec check met=	 	
	let rec check a me =
	match me with
	| V x ->(
			if x=a then true
			else false
			)
	| P (q, V b ) -> (
			if a=b then true
			else false		
			)
	| P (x,C(t, p)) ->(
				   match (t,p) with
			           |(V e, V f)-> (check x t) || (check x p)
				   | _-> (check x t) && (check x p)
				   )
	| P (x,(P (e,f))) -> (check x (P (e,f))) && (check e f)
	| C (x, y) -> (
				   match (x,y) with
			           |(V e, V f)-> (check a x) || (check a y)
				   | _-> (check a x) && (check a x)
				   )
	in
	match met with
	| P (b, c)-> (check b c)
	| _ -> raise ( Error "Illegal input")	
;;

