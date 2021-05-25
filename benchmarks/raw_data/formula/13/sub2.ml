type formula = True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp = Num of int
| Plus of exp * exp
| Minus of exp * exp

let rec eval (f : formula) : bool =
    (
        match f with
            True -> true
        |   False -> false
        |   Not (g) -> not(eval g)
        |   AndAlso ((g), (h)) -> (eval g) && (eval h)
        |   OrElse ((g), (h)) -> (eval g) || (eval h)
        |   Imply ((g), (h)) -> (not(eval g)) || (eval h)
        |   Equal ((d), (e)) ->
                let rec eval_exp (e : exp) : int = 
                    (
                        match e with
                            Num (a) -> a
                        |   Plus ((b), (c)) -> (eval_exp b) + (eval_exp c)
                        |   Minus ((b), (c)) -> (eval_exp b) - (eval_exp c)
                    ) in
                let x : int = (eval_exp d) in
                let y : int = (eval_exp e) in
                if(x = y) then true
                else false
    )
