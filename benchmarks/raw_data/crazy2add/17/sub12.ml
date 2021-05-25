type crazy2 =
    | NIL
    | ZERO of crazy2
    | ONE of crazy2
    | MONE of crazy2

let rec c2l c = match c with
    | NIL -> []
    | ZERO cr -> 0 :: (c2l cr)
    | ONE cr -> 1 :: (c2l cr)
    | MONE cr -> -1 :: (c2l cr)

let rec l2c l = match l with
    | [] -> NIL
    | h::t -> 
        if h == 0 then ZERO(l2c t)
        else if h == 1 then ONE(l2c t)
        else MONE(l2c t)

let crazy2add (l, r) =  
    let ll = c2l l in 
    let rl = c2l r in

    let rec ladd (l, r) c = match (l, r) with 
        | (h::t, []) -> 
            if c == 0 then l
            else if c == h then 0 :: (ladd (t, []) c)
            else (c + h) :: (ladd (t, []) 0)
        | (hl::tl, hr::tr) -> 
            let v = hl + hr + c in
            let nc = 
                if v >= 2 then 1
                else if v <= -2 then -1
                else 0
            in
            let s =
                if v > 0 && (v mod 2) == 1 then 1
                else if (v mod 2) == 0 then 0
                else -1
            in
            s :: (ladd (tl, tr) nc)
        | ([], _) -> [c] (* l == [] -> r == [] guaranteed *)
    in

    if (List.length ll) < (List.length rl) then l2c (ladd (rl, ll) 0)
    else l2c (ladd (ll, rl) 0)
