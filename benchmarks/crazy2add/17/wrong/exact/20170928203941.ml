(*call crazy2*)
type crazy2 = 
    |NIL
    |ZERO of crazy2
    |ONE of crazy2
    |MONE of crazy2

let rec full(x, y, c) = 
    (*c = carry, x, y = crazy2*)
    match x with
    |NIL -> (match y with
            |NIL ->     c
            |ZERO yy -> c + 2 * full(x, yy, 0)
            |ONE yy ->  if c = 1 then 2 * full(x, yy, 1)
                        else if c = 0 then 1 + 2*full(x, yy, 0)
                        else 2*full(x, yy, 0)
            |MONE yy -> if c = 1 then 2*full(x, yy, 0)
                        else if c = 0 then -1 + 2*full(x, yy, 0)
                        else 2*full(x, yy, -1)
            )
    |ZERO xx -> (match y with
            |NIL ->     c + 2*full(xx, y, 0)
            |ZERO yy -> c + 2*full(xx, yy, 0)
            |ONE yy ->  if c = 1 then 2*full(xx, yy, 1)
                        else if c = 0 then 1 + 2*full(xx, yy, 0)
                        else 2*full(xx, yy, 0)
            |MONE yy -> if c = 1 then 2*full(xx, yy, 0)
                        else if c = 0 then -1 + 2*full(xx, yy, 0)
            else 2*full(xx, yy, -1)
            )
    |ONE xx -> (match y with
            |NIL ->     if c = 1 then 2 * full(xx, y, 1)
                        else if c = 0 then 1 + 2*full(xx, y, 0)
                        else 2*full(xx, y, 0)
            |ZERO yy -> if c = 1 then 2*full(xx, yy, 1)
                        else if c = 0 then 1 + 2*full(xx, yy, 0)
                        else 2*full(xx, yy, 0)
            |ONE yy ->  if c = 1 then 1 + 2*full(xx, yy, 1)
                        else if c = 0 then 2*full(xx, yy, 0)
                        else 1 + 2*full(xx, yy, 0)
            |MONE yy -> c + 2*full(xx, yy, 0)
            )
    |MONE xx-> (match y with
            |NIL ->     if c = 0 then -1 + 2*full(xx, y, 0)
                        else if c = 1 then 2*full(xx, y, 0)
                        else 0 + 2*full(xx, y, -1)
            |ZERO yy -> if c = 1 then 2*full(xx, yy, 0)
                        else if c = 0 then -1 + 2*full(xx, yy, 0)
                        else 2*full(xx, yy, -1)
            |ONE yy ->  c + 2*full(xx, yy, 0)
            |MONE yy -> if c = 1 then -1 + 2*full(xx, yy, 0)
                        else if c = 0 then 2*full(xx, yy, -1)
                        else -1 + 2*full(xx, yy, -1)
            )
let rec crazy2add (x, y) = full(x, y, 0)

