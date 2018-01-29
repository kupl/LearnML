(* Problem 7*)
  type digit = ZERO |ONE
  type bin = digit list

  let rec place n = if n > 0 then 2 * place(n-1) else 1
  let rec btd = fun n -> match n with 
  |[] -> 0
  |hd:: tl -> if hd = ONE then (place ((List.length n)-1)) + (btd tl) else (btd tl) 
  
  let rec dtb = fun n -> if n >0 then (if (n mod 2) =1 then (dtb (n/2))@[ONE] else (dtb (n/2)) @[ZERO]) 
  else []

  let bmul : bin -> bin -> bin
  =fun b1 b2 -> let x = ((btd b1 ) *( btd b2)) in dtb x
