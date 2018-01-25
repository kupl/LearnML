(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
  match b-a with
  |0 -> f(b) 
  |_ -> 
    let num  = f(a) in
    let num2 = product f (a+1) b in
    num*num2
