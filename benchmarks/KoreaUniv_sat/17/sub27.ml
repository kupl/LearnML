(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let rec sat : formula -> bool
= fun f -> let rec length lst = match lst with
|hd::tl -> length tl + 1 
|[] -> 0 in 
 let rec findvar f = match f with
 |True -> []
 |False -> []
 |Var(x) -> [x]
 |Neg(a) -> findvar(a)
 |And(a,b) -> findvar(a) @ findvar(b)
 |Or(a,b) -> findvar(a) @ findvar(b)
 |Imply(a,b) -> findvar(a) @ findvar(b)
 |Iff(a,b) -> findvar(a) @ findvar(b) in 
 let rec exp n = match n with
  |0 -> 1
  |1 ->2
  |_ -> 2 * exp(n-1) in
   let rec maketup lst n = match lst with
   |hd::tl -> if (n/2) = 0 then [(hd,true)] else
   (match (n mod 2) with
    |1 -> [(hd, true)] @ maketup tl (n/2)
    |0 -> [(hd, false)] @ maketup tl (n/2)
    |_ -> [(hd, false)])
   |_-> [] in
    let rec tup f lst = match lst with
    |hd::tl -> (match hd with
      |(a,b) -> match f with
       |Var(x) -> if a = x then b else tup f tl
       |_ -> tup f tl)
    |_ -> false in
     let rec fm f lst = match f with
     |True -> true
     |False -> false
     |Var(x) -> tup (Var(x)) lst
     |Neg(x) -> if (fm x lst) = true then false else true
     |And(a,b) -> (fm a lst) && (fm b lst)
     |Or(a,b) -> (fm a lst) || (fm b lst)
     |Imply(a,b) -> (fm (Neg(a)) lst) || (fm b lst)
     |Iff(a,b) -> if (fm a lst) = (fm b lst ) then true else false in
     let lst = findvar(f) in let n = exp(length(lst)) in
      if (let rec sum f n1 n2 = if fm f (maketup lst n2) = true then 1 else if (n1 = n2) then 0 else sum f n1 (n2+1) in sum f n 0) = 1 then true else false