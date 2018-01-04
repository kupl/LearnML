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
;;

let rec eval(f : formula) : bool=
    (* type * type은 (type, type)을 인자로 받는다는 뜻이므로, 식은 아마 아래처럼 정의되어 있을 것이다. 여기서 알 수 있듯이, type constructor는 함수로서의 역할을 한다.*)
    match f with
    TRUE -> true
    | FALSE -> false
    | NOT (form) -> not (eval(form)) (*(not eval(form))으로 쓰면 인자를 잘못 받아들이게 된다.*)
    | ANDALSO (form1, form2) -> (eval(form1) && eval(form2))
    | ORELSE (form1, form2) -> (eval(form1) || eval(form2))
    | IMPLY (form1, form2) -> ((not (eval(form1))) || eval(form2))
    | LESS (expr1, expr2) -> (ex(expr1) < ex(expr2))

and ex(expre) = 
    match expre with
    NUM (exp0) -> (exp0)
    | PLUS (exp1, exp2) -> (ex(exp1) + ex(exp2))
    | MINUS (exp1, exp2) -> (ex(exp1) - ex(exp2))
;;
