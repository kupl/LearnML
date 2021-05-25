type lambda = V of var
  |P of var * lambda
  |C of lambda * lambda
and var = string
let rec checkcorrect l =
  match l with
  |([],station) -> false
  |(a::lst,station) -> if a=station then true else checkcorrect (lst,station)
let rec check2 m =
  match m with
  |(lst,P(left,right))->check2 (left::lst,right)
  |(lst,V(station))->checkcorrect (lst,station)
  |(lst,C(left,right))->check2 (lst,left) && check2 (lst,right)
let check n = check2 ([],n)
