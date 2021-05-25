let sigma :int*int*(int->int) -> int = 
  (*Tuple and currying : different!*)
  fun (a, b, func) ->
   let rec sigma_r c d result : int=
     if c<=d
     then sigma_r (c+1) d (result+(func c))
     else result
   in
   sigma_r a b 0
