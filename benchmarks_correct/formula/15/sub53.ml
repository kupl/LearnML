(*컴공 2014-10618 이세영 1-4*)
type formula=TRUE
            |FALSE
            |NOT of formula
            |ANDALSO of formula * formula
            |ORELSE of formula * formula
            |IMPLY of formula * formula
            |LESS of expr * expr
and expr=NUM of int
        |PLUS of expr * expr
        |MINUS of expr * expr;;
let rec eval=function
    |TRUE->true
    |FALSE->false
    |NOT x->if (eval x)=true then false else true
    |ANDALSO (x,y)->if (eval x)=true && (eval y)=true then true else false
    |ORELSE (x,y)->if (eval x)=false && (eval y)=false then false else true
    |IMPLY (x,y)->if (eval x)=true && (eval y)=false then false else true
    |LESS (x,y)->let rec exp sum=function
                     |NUM x->x
                     |PLUS (x,y)->(exp 0 x)+(exp 0 y)
                     |MINUS (x,y)->(exp 0 x)-(exp 0 y) in
    if (exp 0 x)<(exp 0 y) then true else false;;
