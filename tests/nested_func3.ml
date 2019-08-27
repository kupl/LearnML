
let add4: int -> int
= fun x -> 
  let add2: int -> int
  = fun y -> y+2 
in add2 (add2 x)

