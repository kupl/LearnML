type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp = 
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let rec eval : formula -> bool
= fun f -> 
  let rec cnum = fun n ->
    match n with
      Num x-> x
      |Plus(x,y)-> let nx = cnum x in let ny = cnum y in nx+ny
      |Minus(x,y)-> let nx = cnum x in let ny = cnum y in nx-ny 
      in
  match f with
    True -> true
    | False -> false
    | Not b -> 
      let vb = eval b in
      if (vb = true) then false else true 
    | AndAlso (a,b) -> let va = eval a in let vb = eval b in
      if (va = true && vb = true) then true else false
    | OrElse (a,b) ->  let va = eval a in let vb = eval b in
      if (va = false && vb = false) then false else true
    | Imply (a,b) -> let va = eval a in let vb = eval b in
      if(va = true && vb = false) then false else true
    | Equal (a,b) -> let na = cnum a in let nb = cnum b in 
      if (na = nb) then true else false;;
      
    

