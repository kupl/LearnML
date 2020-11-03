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
        (* eval 을 rec 로 만들지 말고 이 안에 새로운 함수를 만들어 comp가 반복되지 않게 한다.*)

let rec eval arg =
    let rec comp k =
        match k with
            | Num a -> a
            | Plus (b,c) -> (comp b) + (comp c)
            | Minus (d,e) -> (comp d) - (comp e) in
    match arg with
        | True -> true
        | False -> false
        | Not v1 -> not (eval v1)
        | AndAlso (v2,v3) -> (eval v2)&&(eval v3)
        | OrElse (v4,v5) -> (eval v4)||(eval v5)
        | Imply (v6,v7) -> if (eval v6) = false then true else eval v7
        | Equal (v8,v9) -> if (comp v8) = (comp v9) then true else false

