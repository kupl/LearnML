exception NO_NEGATIVE_INTEGERS

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
let identity x = (x+0) in
  let compose f g x = f(g x) in
    if n == 0 then identity
    else if n<0 then raise (NO_NEGATIVE_INTEGERS)
    else
      compose f (iter(n-1,f))