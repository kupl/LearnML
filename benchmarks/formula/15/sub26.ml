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

  let rec cal expr = match expr with
                  | NUM n -> n
                  | PLUS(e1,e2) ->(cal e1) + (cal e2)
                  | MINUS(e1,e2)->(cal e1) - (cal e2)
        
  let rec eval f = match f with
	     |TRUE -> true
	     |FALSE -> false
	     |NOT f1 -> if eval f1 then false
			else true
	     |ANDALSO(f1,f2) -> if eval f1 = true && eval f2 = true then true
			else false
 	     |ORELSE(f1,f2) -> if eval f1 = false && eval f2 = false then false
			else true
	     |IMPLY(f1,f2) -> if eval f1 && eval f2 then true
                   else if eval f1 = false then true
                   else false
 	     |LESS(e1,e2) -> 
                  if cal e1 < cal e2 then true
                  else false

