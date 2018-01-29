(*Problem 3*)
type formula = 
   True
  |False
  |Var of string
  |Neg of formula
  |And of formula * formula
  |Or of formula * formula
  |Imply of formula * formula
  |IFF of formula * formula 

let e = []

let rec extend x l = 
  match l with
  |[] -> x::l
  |hd::tl -> if x=hd then l 
             else hd::(extend x tl)

let rec cntelt l =
  match l with
  |[] -> 0
  |hd::tl -> 1 + (cntelt tl)

let rec cnt f e =
  match f with
  |True -> 0
  |False -> 0
  |Var x -> if (cntelt (extend x e))=1 then 0 else 1 
  |Neg f1 -> cnt f1 e
  |And (f1, f2) -> (cnt f1 e) + (cnt f2 e)
  |Or (f1, f2) -> (cnt f1 e) + (cnt f2 e)
  |Imply (f1, f2) -> (cnt f1 e) + (cnt f2 e)
  |IFF (f1, f2) -> (cnt f1 e) + (cnt f2 e)

let rec evalt f = 
  match f with
  |True -> true
  |False -> false
  |Var x -> true
  |Neg f1 -> if (evalt f1)=true then false else true
  |And (f1, f2) -> (evalt f1)&&(evalt f2)
  |Or (f1, f2) -> (evalt f1)||(evalt f2)
  |Imply (f1, f2) -> if ((evalt f1)=true)&&((evalt f2)=false) then false else true  
  |IFF (f1, f2) -> if (evalt f1)=(evalt f2) then true else false

let rec evalf f = 
  match f with
  |True -> true
  |False -> false
  |Var x -> false
  |Neg f1 -> if (evalf f1)=true then false else true
  |And (f1, f2) -> (evalf f1)&&(evalf f2)
  |Or (f1, f2) -> (evalf f1)||(evalf f2)
  |Imply (f1, f2) -> if ((evalf f1)=true)&&((evalf f2)=false) then false else true
  |IFF (f1, f2) -> if (evalf f1)=(evalf f2) then true else false

let sat f =
  if (cnt f e)>0 then true
  else (evalt f)||(evalf f)

