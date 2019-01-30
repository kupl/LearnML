type exp =
    X
    | INT of int
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | SIGMA of exp * exp * exp

type maybe = Just of int | Nothing

let rec calculator : exp -> int
= fun exp ->
    let rec calc' x e =
        match e with
        | X -> (match x with Just n -> n | Nothing -> failwith "unbounded variable")
        | INT n -> n
        | ADD (l, r) -> (calc' x l) + (calc' x r)
        | SUB (l, r) -> (calc' x l) - (calc' x r)
        | MUL (l, r) -> (calc' x l) * (calc' x r)
        | DIV (l, r) -> (calc' x l) / (calc' x r)
        | SIGMA (l, r, s) ->
            let nl = calc' x l in
            let nr = calc' x r in
            let rec sigma n =
                if n > nr then 0
                else calc' (Just n) s + sigma (n + 1) in
            sigma nl
    in calc' Nothing exp
;;
