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


let rec eval a = 
        match a with
        |TRUE -> true
        |FALSE -> false
        |NOT b -> if (eval b) = false then true
                        else true

        |ANDALSO(b,c) -> if (eval b) = (eval c) = true then true
                                else false 
        |ORELSE(b,c) -> if (eval b) = true || eval c = true then true
                                else false
        
        
        |IMPLY(b,c) -> if (eval b) = true && (eval c) = false then false
                        else true
        


        |LESS(b,c) -> if b < c then true
                        else false
       
