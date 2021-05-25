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

let rec evalexp exp : int =
    match exp with
    |Num a -> a
    |Plus (a,b) -> (evalexp a)+(evalexp b)
    |Minus (a,b) -> (evalexp a)-(evalexp b)

let rec eval form : bool =
	match form with
    |True->true
    |False->false
    |Not f -> (match eval f with
               |true->false
               |false->true
              )
    |AndAlso (f1, f2) -> (match (eval f1),(eval f2) with
                          |true,true -> true
                          |true,false
                          |false,true
                          |false,false -> false
                         )
    |OrElse (f1,f2) -> (match (eval f1),(eval f2) with
                        |true,true
                        |true,false
                        |false,true -> true
                        |false,false -> false
                       )
    |Imply (f1,f2) -> (match (eval f1),(eval f2) with
                       |true,false -> false
                       |true,true
                       |false,true
                       |false,false -> true
                      )
    |Equal (e1,e2) -> ((evalexp e1) = (evalexp e2))

  
