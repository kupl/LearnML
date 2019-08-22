type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let checker (a:var) (b:var) : bool =
    if a = b then true
    else false

let rec checkHelper ((a:lambda),(l:var list)) : bool =
    match a with
    | V x -> List.exists(checker x) l
    | P(x,y) -> checkHelper(y,x::l)
    | C(x,y) -> checkHelper(x,l) && checkHelper(y,l)

let check (a:lambda) : bool =
    checkHelper(a,[])
