(* C:\Documents and Settings\Administrator\¹ÙÅÁ È­¸é\pl_first_homework\Excercise8.ml *)

(* Computer S&E/2007-15612/park sungjun*)
(*Exercise 8 Though wornning is exist, this program run well*)
exception Error of string
type metro =
    STATION of string
  | AREA of string * metro
  | CONNECT of metro * metro
                                                    
 let rec checkMetro met=	 	
	let rec check a me =
	match me with
	| STATION x ->(
			if x=a then true
			else false
			)
	| AREA (q, STATION b ) -> (
			if a=b then true
			else false		
			)
	| AREA (x,CONNECT(t, p)) ->(
				   match (t,p) with
			           |(STATION e, STATION f)-> (check x t) || (check x p)
				   | _-> (check x t) && (check x p)
				   )
	| AREA (x,(AREA (e,f))) -> (check x (AREA (e,f))) && (check e f)
	| CONNECT (x, y) -> (
				   match (x,y) with
			           |(STATION e, STATION f)-> (check a x) || (check a y)
				   | _-> (check a x) && (check a x)
				   )
	in
	match met with
	| AREA (b, c)-> (check b c)
	| _ -> raise ( Error "Illegal input")	
;;

