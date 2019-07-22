  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp

  let rec checklst (*true if there is no this value before*)
  = fun lst val1 -> match lst with
  | hd::tl -> if(hd = val1) then false else checklst tl val1
  | [] -> true

  let rec checkExp
  = fun lst exp -> match exp with
  | V var -> if checklst lst var then false else true
  | P (var, exp) -> if checklst lst var then checkExp (var::lst) exp else checkExp lst exp
  | C (exp1, exp2) ->if (checkExp lst exp1 == true && checkExp lst exp2 == true) then true else false

   let check : exp -> bool
  = fun exp -> let varlst = [] in checkExp varlst exp
