(*Problem 3*)
let comp f g = fun x -> f(g x)

let rec loop  : int * (int -> int) * (int -> int) -> (int -> int)
= fun (n,f,g) -> if(n=1) then g
  else loop((n-1),f,(comp f g))

let iden_func n = n

let error n = (-1) 

let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> if (n<0) then error 
  else(
      if (n=0) then iden_func
      else loop (n,f, f)
      )