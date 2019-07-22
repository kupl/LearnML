type formula = TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

let rec eval (f : formula) : bool =
    (
        match f with
            TRUE -> true
        |   FALSE -> false
        |   NOT (g) -> not(eval g)
        |   ANDALSO ((g), (h)) -> (eval g) && (eval h)
        |   ORELSE ((g), (h)) -> (eval g) || (eval h)
        |   IMPLY ((g), (h)) -> (not(eval g)) || (eval h)
        |   LESS ((d), (e)) ->
                let rec eval_expr (e : expr) : int = 
                    (
                        match e with
                            NUM (a) -> a
                        |   PLUS ((b), (c)) -> (eval_expr b) + (eval_expr c)
                        |   MINUS ((b), (c)) -> (eval_expr b) - (eval_expr c)
                    ) in
                let x : int = (eval_expr d) in
                let y : int = (eval_expr e) in
                if(x < y) then true
                else false
    )
