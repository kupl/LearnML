let rec p_range n m = if (n+1)=m then [n]
                      else n::(p_range (n+1) m);;
exception Problem
let hd l = 
  match l with
    [] -> raise Problem
    |a::b-> a;;
let tl l =
  match l with
    [] -> raise Problem
    |a::b-> b;;
let rec d_prime l k =
  if l = [] then []
  else if k mod (hd l) =0 then [1]
  else d_prime (tl l) k;;
let prime : int -> bool
= fun n -> if n<2 then false
           else if n=2 then true
           else if d_prime (p_range 2 n) n = [] then true
           else false;;
