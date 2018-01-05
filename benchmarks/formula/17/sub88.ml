type formula = 
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula*formula
  | LESS of expr* expr
and expr=
  | NUM of int
  | PLUS of expr*expr
  | MINUS of expr*expr

let rec exp_to_int (e: expr): int=
  match e with
  | NUM i -> i
  | PLUS (i1, i2) -> (exp_to_int i1)+ (exp_to_int i2)
  | MINUS (i1, i2) -> (exp_to_int i1)- (exp_to_int i2)


let rec eval (fm: formula) : bool =
  match fm with
  | TRUE -> true
  | FALSE -> false
  | NOT fm2 -> if (eval fm2) then false else true
  | ANDALSO (fm1, fm2) ->
      if ((eval fm1) && (eval fm2) )
      then true
      else false
  | ORELSE (fm1, fm2) ->
      if ((eval fm1) || (eval fm2) )
      then true
      else false
  | IMPLY (fm1, fm2) ->
      if (not (eval fm1) || (eval fm2))
      then true
      else false
  | LESS (exp1, exp2) ->
      if ((exp_to_int exp1) < (exp_to_int exp2))
      then true
      else false

let print_bool b=
  print_endline(string_of_bool b)

let _ = print_bool (eval TRUE); 
print_bool(eval (ANDALSO (TRUE, FALSE))); 
print_bool (eval (LESS (NUM 5, NUM 3)))

