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
        (* eval 을 rec 로 만들지 말고 이 안에 새로운 함수를 만들어 comp가 반복되지 않게 한다.*)

let rec eval arg =
    let rec comp k =
        match k with
            | NUM a -> a
            | PLUS (b,c) -> (comp b) + (comp c)
            | MINUS (d,e) -> (comp d) - (comp e) in
    match arg with
        | TRUE -> true
        | FALSE -> false
        | NOT v1 -> not (eval v1)
        | ANDALSO (v2,v3) -> (eval v2)&&(eval v3)
        | ORELSE (v4,v5) -> (eval v4)||(eval v5)
        | IMPLY (v6,v7) -> if (eval v6) = false then true else eval v7
        | LESS (v8,v9) -> if (comp v8) < (comp v9) then true else false

