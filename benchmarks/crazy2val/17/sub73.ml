type crazy2 = NIL
            |ZERO of crazy2
            |ONE of crazy2
            |MONE of crazy2

let rec calc (i, a) =
    if i = 0 then a
    else calc(i-1, a*2)

let rec eval (expr, i, v) = 
    match expr with
    |NIL -> v
    |ZERO a -> eval(a, i+1, v)
    |ONE a -> eval(a, i+1, v + calc(i, 1))
    |MONE a -> eval(a, i+1, v - calc(i, 1))

let rec crazy2val expr = eval(expr, 0, 0)
