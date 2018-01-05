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

let rec eval(f : formula) =
  let rec cal(ex : expr) =
     match ex with
          NUM a -> a
        | PLUS(a,b) -> cal(a) + cal(b)
        | MINUS(a,b) -> cal(a) - cal(b) in

    match f with
          TRUE -> true
        | FALSE -> false
        | NOT a -> if eval(a)=true then false
                        else true
        | ANDALSO(a,b) -> if eval(a)=false then false
                                     else if eval(b)=false then false
                                     else true
        | ORELSE(a,b) -> if eval(a)=true then true
                                   else if eval(b)=true then true
                                   else false
        | IMPLY (a,b) -> if eval(a)=true then
                                    if eval(b)=true then true
                                    else false
                               else true
        | LESS (a,b) -> if (cal(a)<cal(b)) then true
                               else false;;                                      