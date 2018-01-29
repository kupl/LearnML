
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec chk
  = fun a var->
  match a with
  |hd::tl -> if(var=hd) then true else (chk tl var)
  |_ -> false   
 
  let lst = []    

  let rec chk2
  = fun lst exp ->
  match exp with
  | P(a,b) -> chk2 (a::lst) b
  | C(a,b) -> if((chk2 lst a)&&(chk2 lst b)) then true else false
  | V(a) -> if(chk lst a) then true else false


  let rec check : exp -> bool
  = fun exp -> chk2 [] exp
