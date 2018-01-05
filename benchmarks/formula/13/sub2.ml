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
        |   NOT (g : formula) -> not(eval g)
        |   ANDALSO ((g : formula), (h : formula)) -> (eval g) && (eval h)
        |   ORELSE ((g : formula), (h : formula)) -> (eval g) || (eval h)
        |   IMPLY ((g : formula), (h : formula)) -> (not(eval g)) || (eval h)
        |   LESS ((d : expr), (e : expr)) ->
                let rec eval_expr (e : expr) : int = 
                    (
                        match e with
                            NUM (a : int) -> a
                        |   PLUS ((b : expr), (c : expr)) -> (eval_expr b) + (eval_expr c)
                        |   MINUS ((b : expr), (c : expr)) -> (eval_expr b) - (eval_expr c)
                    ) in
                let (x : int) = (eval_expr d) in
                let (y : int) = (eval_expr e) in
                if(x < y) then true
                else false
    )
