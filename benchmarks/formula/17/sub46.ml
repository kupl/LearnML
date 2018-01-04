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

let rec calc (a : expr) : int =
match a with
  | NUM a -> a
  | PLUS (b, c) -> (calc b) + (calc c)
  | MINUS (b, c) -> (calc b) - (calc c)

let rec eval (input : formula) : bool =
match input with
  | TRUE -> true
  | FALSE -> false
  | NOT a ->
    (
      match eval a with
      | true -> false
      | false -> true
    )
  | ANDALSO (a, b) ->
    (
      match eval a with
      | true -> eval b
      | false -> false
    )
  | ORELSE (a, b) ->
    (
      match eval a with
      | true -> true
      | false -> eval b
    )
  | IMPLY (a, b) ->
    (
      match eval a with
      | true -> eval b
      | false -> true
    )
  | LESS (c, d) ->
      if (calc c) < (calc d) then true
      else false

