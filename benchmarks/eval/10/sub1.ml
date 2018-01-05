type expr =
	NUM of int
|	PLUS of expr * expr
|	MINUS of expr * expr
|	MULT of expr * expr
|	DIVIDE of expr * expr
|	MAX of expr list
;;

let _max x = List.nth (List.sort compare x) (List.length x - 1) ;;

let rec eval x = match x with
	NUM a -> a
|	PLUS (a, b) -> eval a + eval b
|	MINUS (a, b) -> eval a - eval b
|	MULT (a, b) -> eval a * eval b
|	DIVIDE (a, b) -> eval a / eval b
|	MAX a -> _max ( List.map eval a )
;;