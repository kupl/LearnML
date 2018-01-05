(* 2010-11753 snucse Taekmin Kim *)
(* HW 1-4 *)

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

let rec solve : expr -> int = fun (e) ->
  match e with
  |NUM x -> x
  |PLUS (se1, se2) -> solve(se1) + solve(se2)
  |MINUS (se1, se2) -> solve(se1) - solve(se2)

let rec eval : formula -> bool = fun(f) -> 
  match f with
  |TRUE -> true
  |FALSE -> false
  |NOT f ->
      if(eval(f) == true) then false else true
  |ANDALSO (f1, f2) ->
      if(eval(f1) == true && eval(f2) == true) then true else false
  |ORELSE (f1, f2) -> 
      if(eval(f1) == true || eval(f2) == true) then true else false
  |IMPLY (f1, f2) ->
      if(eval(f1) == true && eval(f2) == false) then false else true
  |LESS (e1, e2) -> 
      if(solve(e1) < solve(e2)) then true else false

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
  *)
