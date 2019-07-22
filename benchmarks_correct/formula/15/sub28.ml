type formula =
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr
;;

let rec eval formula =
  match formula with
  | TRUE ->
      true
  | FALSE ->
      false
  | NOT f ->
      not (eval f)
  | ANDALSO (a, b) ->
      (eval a) && (eval b)
  | ORELSE (a, b) ->
      (eval a) || (eval b)
  | IMPLY (a, b) ->
      (not (eval a)) || (eval b)
  | LESS (a, b) ->
      let rec calc expr =
        match expr with
        | NUM i ->
            i
        | PLUS (e1, e2) ->
            (calc e1) + (calc e2)
        | MINUS (e1, e2) ->
            (calc e1) - (calc e2)
      in (calc a) < (calc b)
;;
(*
let _ = 
  let print_bool x = 
    print_endline (string_of_bool x) in 
  print_bool (true = eval TRUE); 
  print_bool (false = eval FALSE); 
  print_bool (false = eval (NOT TRUE)); 
  print_bool (true = eval (NOT FALSE)); 
  print_bool (true = eval (ANDALSO (TRUE, TRUE))); 
  print_bool (false = eval (ANDALSO (TRUE, FALSE))); 
  print_bool (false = eval (ANDALSO (FALSE, TRUE))); 
  print_bool (false = eval (ANDALSO (FALSE, FALSE))); 
  print_bool (true = eval (ORELSE (TRUE, TRUE))); 
  print_bool (true = eval (ORELSE (TRUE, FALSE))); 
  print_bool (true = eval (ORELSE (FALSE, TRUE))); 
  print_bool (false = eval (ORELSE (FALSE, FALSE))); 
  print_bool (false = eval (IMPLY (TRUE, FALSE))); 
  print_bool (true = eval (IMPLY (TRUE, TRUE))); 
  print_bool (true = eval (IMPLY (FALSE, TRUE))); 
  print_bool (true = eval (IMPLY (FALSE, FALSE))); 
  print_bool (true = eval (LESS (NUM 3, NUM 5))); 
  print_bool (false = eval (LESS (NUM 3, NUM 3))); 
  print_bool (false = eval (LESS (NUM 3, NUM 1))); 
  print_bool (false = eval 
  (LESS (PLUS (NUM 3, NUM 4), MINUS (NUM 5, NUM 1)))); 
  print_bool (true = eval 
  (LESS (PLUS (NUM 10, NUM 12), MINUS (NUM 10, NUM (-13))))); 

print_string "End of Test\n";;
*)
