let sigma func a b =
   let rec sigma_r c d result : int=
     if c<=d
     then sigma_r (c+1) d (result+(func c))
     else result
   in
   sigma_r a b 0
