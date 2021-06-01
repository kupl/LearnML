type formula =
    | True
    | False 
    | Not of formula 
    | AndAlso of formula * formula 
    | OrElse of formula * formula 
    | Imply of formula * formula 
    | Equal of exp * exp

and exp = 
    | Num of int 
    | Plus of exp * exp 
    | Minus of exp * exp 

let rec eval_exp e =
    match e with
        | Num i -> i
        | Plus (e1, e2) -> eval_exp e1 + eval_exp e2 
        | Minus (e1, e2) -> eval_exp e1 - eval_exp e2;; 

let rec eval : formula -> bool
= fun f -> match f with 
    | True -> true
    | False -> false
    | Not f1 -> 
        let v1 = eval f1 in 
        begin
            match v1 with 
                | true -> false
                | false -> true
        end
    | AndAlso (f1, f2) -> 
        let v1 = eval f1 in
        let v2 = eval f2 in 
        begin
            match v1, v2 with
                | true, true -> true
                | _, _ -> false
        end
    | OrElse (f1, f2) -> 
        let v1 = eval f1 in
        begin
            match v1 with 
                | true -> true
                | _ -> let v2 = eval f2 in 
                    begin
                        match v2 with 
                            | true -> true
                            | _ -> false
                    end
        end
    | Imply (f1, f2) -> 
        let v1 = eval f1 in
        let v2 = eval f2 in 
        begin
            match v1, v2 with 
                | true, _ -> v2
                | _, _ -> true
        end
    | Equal (e1, e2) -> eval_exp e1 = eval_exp e2;;