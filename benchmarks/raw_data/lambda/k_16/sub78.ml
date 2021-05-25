  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda

  let rec checklst (*true if there is no this value before*)
  = fun lst val1 -> match lst with
  | hd::tl -> if(hd = val1) then false else checklst tl val1
  | [] -> true

  let rec checkExp
  = fun lst lambda -> match lambda with
  | V var -> if checklst lst var then false else true
  | P (var, lambda) -> if checklst lst var then checkExp (var::lst) lambda else checkExp lst lambda
  | C (lambda1, lambda2) ->if (checkExp lst lambda1 == true && checkExp lst lambda2 == true) then true else false

   let check : lambda -> bool
  = fun lambda -> let varlst = [] in checkExp varlst lambda
