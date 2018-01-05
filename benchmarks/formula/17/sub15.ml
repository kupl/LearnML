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

let rec evalexp exp : int =
    match exp with
    |NUM a -> a
    |PLUS (a,b) -> (evalexp a)+(evalexp b)
    |MINUS (a,b) -> (evalexp a)-(evalexp b)

let rec eval form : bool =
	match form with
    |TRUE->true
    |FALSE->false
    |NOT f -> (match eval f with
               |true->false
               |false->true
              )
    |ANDALSO (f1, f2) -> (match (eval f1),(eval f2) with
                          |true,true -> true
                          |true,false
                          |false,true
                          |false,false -> false
                         )
    |ORELSE (f1,f2) -> (match (eval f1),(eval f2) with
                        |true,true
                        |true,false
                        |false,true -> true
                        |false,false -> false
                       )
    |IMPLY (f1,f2) -> (match (eval f1),(eval f2) with
                       |true,false -> false
                       |true,true
                       |false,true
                       |false,false -> true
                      )
    |LESS (e1,e2) -> ((evalexp e1) < (evalexp e2))

  
