
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec chk
  = fun a var->
  match a with
  |hd::tl -> if(var=hd) then true else (chk tl var)
  |_ -> false   
 
  let lst = []    

  let rec chk2
  = fun lst lambda ->
  match lambda with
  | P(a,b) -> chk2 (a::lst) b
  | C(a,b) -> if((chk2 lst a)&&(chk2 lst b)) then true else false
  | V(a) -> if(chk lst a) then true else false


  let rec check : lambda -> bool
  = fun lambda -> chk2 [] lambda
