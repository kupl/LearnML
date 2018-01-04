exception DividedByZero;;

type expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr
  | MULT of expr * expr
  | DIVIDE of expr * expr
  | MAX of expr list;;

let rec eval expr =
  match expr with
  | NUM x -> x
  | PLUS (x, y) -> eval x + eval y
  | MINUS (x, y) -> eval x - eval y
  | MULT (x, y) -> eval x * eval y
  | DIVIDE (x, y) ->
    let y = eval y in
    if y <> 0 then eval x / y else raise DividedByZero
  | MAX [] -> 0
  | MAX (x :: xs) -> List.fold_left max (eval x) (List.rev_map eval xs);;
