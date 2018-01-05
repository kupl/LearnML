type nat = ZERO
  | SUCC of nat
  
  let rec eval t = 
  match t with
  ZERO->0
  | SUCC u -> (eval u) + 1
  
  let rec reval n =
  if(n=0) then ZERO
  else SUCC (reval n-1)
  
  let natadd n1 * n2 = reval((eval n1)*(eval n2))
  Ã~
type nat = ZERO
  | SUCC of nat
  
  let rec eval t = 
  match t with
  ZERO->0
  | SUCC u -> (eval u) + 1
  
  let rec reval n =
  if(n=0) then ZERO
  else SUCC(reval(n-1))
  
  let natadd (n1,n2) = reval((eval n1)+(eval n2))
  let natmul (n1,n2) = reval((eval n1)*(eval n2))
