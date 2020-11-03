(*컴공 2014-10618 이세영 1-4*)
type formula=True
            |False
            |Not of formula
            |AndAlso of formula * formula
            |OrElse of formula * formula
            |Imply of formula * formula
            |Equal of exp * exp
and exp=Num of int
        |Plus of exp * exp
        |Minus of exp * exp;;
let rec eval=function
    |True->true
    |False->false
    |Not x->if (eval x)=true then false else true
    |AndAlso (x,y)->if (eval x)=true && (eval y)=true then true else false
    |OrElse (x,y)->if (eval x)=false && (eval y)=false then false else true
    |Imply (x,y)->if (eval x)=true && (eval y)=false then false else true
    |Equal (x,y)->let rec exp sum=function
                     |Num x->x
                     |Plus (x,y)->(exp 0 x)+(exp 0 y)
                     |Minus (x,y)->(exp 0 x)-(exp 0 y) in
    if (exp 0 x)=(exp 0 y) then true else false;;
