type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let crazy2val: crazy2 -> int = fun c ->
    let rec aux (crazy_val, result, base) =
        match crazy_val with
        | NIL -> result
        | ZERO(inner_val) -> aux (inner_val, result, base * 2)
        | ONE(inner_val) -> aux (inner_val, result + base, base * 2)
        | MONE(inner_val) -> aux (inner_val, result - base, base * 2)
    in aux(c, 0, 1)