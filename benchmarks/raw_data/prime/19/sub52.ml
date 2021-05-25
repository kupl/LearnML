
let rec rem n x =
  match x with
    |0 -> false
    |1 -> true
    |_-> if n mod x = 0 then false
        else rem n (x-1);;
        
let prime n =
  if n = 1 then false
  else rem n (n-1);;



