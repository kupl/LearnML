exception NOLIST

type expr= NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list


let max (a,b) =
    if a > b then
        a else b

let rec maxlist (a:expr list) =
    match a with 
    | [] -> raise NOLIST
    | [hd] -> eval(hd)
    | hd::td -> max(eval(hd),maxlist(td))
    

and eval (exp) =
    match exp with
    | NUM a -> a
    | PLUS (a,b) -> eval(a) + eval(b)
    | MINUS (a,b) -> eval(a) - eval(b)
    | MULT (a,b) -> eval(a) * eval(b)
    | DIVIDE (a,b) -> eval(a) / eval(b)
    | MAX a -> maxlist(a)
