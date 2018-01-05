(*컴퓨터공학부 2010-11779 박진영 1.2*)
exception Rec_exception

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

let eval fm =

let rec cal e =
match e with
| NUM i -> e
| PLUS (e1, e2) -> 
  (match (cal e1, cal e2) with
  | (NUM i1, NUM i2) -> NUM (i1+i2)
  | _ -> raise Rec_exception
  ) 
| MINUS (e1, e2) ->
  (match (cal e1, cal e2) with
  | (NUM i1, NUM i2) -> NUM (i1-i2)
  | _ -> raise Rec_exception
  ) 
in

let rec evalfunc fm =
match fm with
| TRUE -> TRUE
| FALSE -> FALSE
| NOT f -> 
  ( match evalfunc f with
  | TRUE -> FALSE
  | FALSE -> TRUE
  | _ -> raise Rec_exception
  )
| ANDALSO (f1, f2) ->
  ( match (evalfunc f1, evalfunc f2) with
  | (TRUE, TRUE) -> TRUE
  | _ -> FALSE
  )
| ORELSE (f1, f2) ->
  ( match (evalfunc f1, evalfunc f2) with
  | (FALSE, FALSE) -> FALSE
  | _ -> TRUE
  )
| IMPLY (f1, f2) ->
  ( match (evalfunc f1, evalfunc f2) with
  | (TRUE, FALSE) -> FALSE
  | _ -> TRUE
  )
| LESS (e1, e2) ->
  (match (cal e1, cal e2) with
  | (NUM i1, NUM i2) ->
    ( if i1 < i2 then TRUE
      else FALSE
    )
  | _ -> raise Rec_exception
  )
in

match evalfunc fm with
| TRUE -> true
| FALSE -> false
| _ -> raise Rec_exception
