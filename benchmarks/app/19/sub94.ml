exception Problem
let hd l = 
  match l with
    []-> raise Problem 
    |a::b -> a;;
let tl l =
  match l with
    []-> raise Problem
    |a::b -> b;;
let rec range a b =
  if a=b then [a]
  else if a<b then a::range (a+1) b
  else raise Problem;;

let rec nth_lst l n =
  if n=0 then raise Problem
  else if n = 1 then hd l
  else nth_lst (tl l) (n-1);;

let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *);;
