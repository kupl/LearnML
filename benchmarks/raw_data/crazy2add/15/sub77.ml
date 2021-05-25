type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

type crazy2bit = Z | O | M

let val2bit = fun x ->
    match x with
    | NIL | ZERO(_) -> Z
    | ONE(_) -> O
    | MONE(_) -> M

let append_bit bit x =
    match bit with
    | Z -> ZERO(x)
    | O ->  ONE(x)
    | M -> MONE(x)

let crazy_inner x =
    match x with
    |     NIL -> NIL
    |  ONE(y) -> y
    | MONE(y) -> y
    | ZERO(y) -> y

let crazy2add: (crazy2 * crazy2) -> crazy2 = fun (a, b) ->
    let halfadd a b =
        match (a, b) with
        | (O, O) -> (Z, O)
        | (O, M) -> (Z, Z)
        | (M, O) -> (Z, Z)
        | (M, M) -> (Z, M)
        | (_, Z) -> (a, Z)
        | (Z, _) -> (b, Z)
    in
    let fulladd a b c =
        let sum1 = (halfadd a b) in
        let sum2 = (halfadd c (fst sum1)) in
        ( (fst sum2), (fst (halfadd (snd sum1) (snd sum2))) )
    in
    let rec crazy2add_carry a b carry =
        match (a, b) with
        | (NIL, NIL) -> (
            match carry with
            | Z -> NIL
            | O -> ONE(NIL)
            | M -> MONE(NIL))
        | _ ->
                let sum = (fulladd (val2bit a) (val2bit b) carry) in
                (append_bit (fst sum) (crazy2add_carry (crazy_inner a) (crazy_inner b) (snd sum)))
    in (crazy2add_carry a b Z)
