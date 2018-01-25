  let rec product f a b =
    if a=b then f a
      else (f b)*product f a (b-1);;
      
   let dfact n=
     let fun1 x=2*x in
       let fun2 x=2*x -1 in
         if n mod 2 = 0 then product (fun1) 1 (n/2)
    else product (fun2) 1 ((n+1)/2);;
