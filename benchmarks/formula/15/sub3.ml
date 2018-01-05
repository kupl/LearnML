(*2007-10575 조용훤*)
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
   | TRUE -> true
   | FALSE -> false  
   | NOT one -> (match one with 
              | _ -> false
              | TRUE -> false
              | FALSE -> true)

   | ANDALSO (left, right) -> (match left, right with
                              | _, _ -> false
                              | FALSE, FALSE -> false
                              | FALSE, TRUE -> false
                              | TRUE, FALSE -> false
                              | TRUE, TRUE -> true)
                               
   | ORELSE (left, right) -> (match left, right with
                 | _, _ -> false
                 | TRUE, FALSE -> true
                 | TRUE, TRUE -> true
                 | FALSE, TRUE -> true
                 | FALSE, FALSE -> false)

   | IMPLY (left, right)-> (match left, right with 
                            | _, _ -> false
                            | FALSE, FALSE -> true
                            | FALSE, TRUE -> true
                            | TRUE, FALSE -> false
                            | TRUE, TRUE -> true)
  
   | LESS (left, right) -> if left < right 
                             then true
                             else false 
                           
  
