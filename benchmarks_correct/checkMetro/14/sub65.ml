type lambda = V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let streq a b =
  a = b

let rec check_lst mtr lst = 
  match mtr with
    | V x -> List.exists (streq x) lst
    | P (x, y) -> check_lst y (x::lst)
    | C (x, y) -> (check_lst x lst) && (check_lst y lst)

let check mtr = 
  check_lst mtr []
