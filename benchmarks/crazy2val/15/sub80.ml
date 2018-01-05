type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let crazy2val: crazy2 -> int = fun c ->
    let rec aux (crazy_val, result, base) =
        match crazy_val with
        | NIL -> result
        | ZERO(inner_val) -> aux (inner_val, result, base * 2)
        | ONE(inner_val) -> aux (inner_val, result + base, base * 2)
        | MONE(inner_val) -> aux (inner_val, result - base, base * 2)
    in aux(c, 0, 1)

let crazy2add: (crazy2 * crazy2) -> crazy2 = fun (a, b) ->
    let rec list2crazy (l, result) =
        match l with
        | [] -> result
        | e::ll -> ( if e == 0 then list2crazy(ll, ZERO(result))
        else (if e == 1 then list2crazy(ll, ONE(result))
        else list2crazy(ll, MONE(result))))
    and val2list (n, result) =
        match n with
        | 0 -> result
        | _ -> (if (n mod 2 == 0) then val2list(n / 2, 0::result)
        else (if (n < 0) then val2list(n / 2, (-1)::result)
        else val2list(n / 2, 1::result)))
    in list2crazy (val2list ( (crazy2val(a) + crazy2val(b)), []), NIL)
