type formula = True
                    | False
                    | Not of formula
                    | AndAlso of formula * formula
                    | OrElse of formula * formula
                    | Imply of formula * formula
                    | Equal of exp * exp
and exp = Num of int
            | Plus of exp * exp
            | Minus of exp * exp
;;

let rec eval(f : formula) : bool=
    (* type * type은 (type, type)을 인자로 받는다는 뜻이므로, 식은 아마 아래처럼 정의되어 있을 것이다. 여기서 알 수 있듯이, type constructor는 함수로서의 역할을 한다.*)
    match f with
    True -> true
    | False -> false
    | Not (form) -> not (eval(form)) (*(not eval(form))으로 쓰면 인자를 잘못 받아들이게 된다.*)
    | AndAlso (form1, form2) -> (eval(form1) && eval(form2))
    | OrElse (form1, form2) -> (eval(form1) || eval(form2))
    | Imply (form1, form2) -> ((not (eval(form1))) || eval(form2))
    | Equal (exp1, exp2) -> (ex(exp1) = ex(exp2))

and ex(expe) = 
    match expe with
    Num (exp0) -> (exp0)
    | Plus (exp1, exp2) -> (ex(exp1) + ex(exp2))
    | Minus (exp1, exp2) -> (ex(exp1) - ex(exp2))
;;
