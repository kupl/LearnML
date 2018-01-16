exception EMPTY_LIST of string

type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list

let rec eval e = 

let rec find_max lst buf = match lst with
| e::l -> if (eval e) > buf then (find_max l (eval e))
          else (find_max l buf)
| [] -> buf in

match e with
| NUM a -> a
| PLUS (e1, e2) -> (eval e1) + (eval e2)
| MINUS (e1, e2) -> (eval e1) - (eval e2)
| MULT (e1, e2) -> (eval e1) * (eval e2)
| DIVIDE (e1, e2) -> (eval e1) / (eval e2)
| MAX e_list -> (find_max e_list 0)
