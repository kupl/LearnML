(* 2010-11753 snucse Taekmin Kim *)
(* HW 1-4 *)

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

let rec solve : exp -> int = fun (e) ->
  match e with
  |Num x -> x
  |Plus (se1, se2) -> solve(se1) + solve(se2)
  |Minus (se1, se2) -> solve(se1) - solve(se2)

let rec eval : formula -> bool = fun(f) -> 
  match f with
  |True -> true
  |False -> false
  |Not f ->
      if(eval(f) == true) then false else true
  |AndAlso (f1, f2) ->
      if(eval(f1) == true && eval(f2) == true) then true else false
  |OrElse (f1, f2) -> 
      if(eval(f1) == true || eval(f2) == true) then true else false
  |Imply (f1, f2) ->
      if(eval(f1) == true && eval(f2) == false) then false else true
  |Equal (e1, e2) -> 
      if(solve(e1) = solve(e2)) then true else false

      (*
let _ = 
  let print_bool x = 
    print_endline (string_of_bool x) in 
  print_bool (true = eval True); 
  print_bool (false = eval False); 
  print_bool (false = eval (Not True)); 
  print_bool (true = eval (Not False)); 
  print_bool (true = eval (AndAlso (True, True))); 
  print_bool (false = eval (AndAlso (True, False))); 
  print_bool (false = eval (AndAlso (False, True))); 
  print_bool (false = eval (AndAlso (False, False))); 
  print_bool (true = eval (OrElse (True, True))); 
  print_bool (true = eval (OrElse (True, False))); 
  print_bool (true = eval (OrElse (False, True))); 
  print_bool (false = eval (OrElse (False, False))); 
  print_bool (false = eval (Imply (True, False))); 
  print_bool (true = eval (Imply (True, True))); 
  print_bool (true = eval (Imply (False, True))); 
  print_bool (true = eval (Imply (False, False))); 
  print_bool (true = eval (Equal (Num 3, Num 5))); 
  print_bool (false = eval (Equal (Num 3, Num 3))); 
  print_bool (false = eval (Equal (Num 3, Num 1))); 
  print_bool (false = eval 
  (Equal (Plus (Num 3, Num 4), Minus (Num 5, Num 1)))); 
  print_bool (true = eval 
  (Equal (Plus (Num 10, Num 12), Minus (Num 10, Num (-13))))); 
  *)
