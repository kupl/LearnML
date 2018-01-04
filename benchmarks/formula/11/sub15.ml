type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

type formula = TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr


    
let rec eval f =
    let rec val_of_expr expr = 
        match expr with
        NUM a -> a
        | PLUS (a,b) -> (val_of_expr a)+(val_of_expr b)
        | MINUS (a,b) -> (val_of_expr a)-(val_of_expr b)
    in
        
    match f with
    TRUE -> true
    | FALSE -> false
    | NOT a -> not (eval a)
    | ANDALSO (a,b) -> (eval a)&(eval b)
    | ORELSE (a,b) -> (eval a)||(eval b)
    | IMPLY (a,b) -> (not (eval a))||(eval b)
    | LESS (e1,e2) -> (val_of_expr e1)<(val_of_expr e2)
      
