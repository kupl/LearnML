(*problem3*)

type formula=
True
|False
|Var of string
|Neg of formula
|And of formula*formula
|Or of formula * formula
|Imply of formula * formula
|Iff of formula * formula;;

let rec sat1 : formula -> formula 
= fun t -> match t with
|True -> True
|False-> False
|Var s -> Var s
|Neg(t1) -> if (sat1 t1)=True then False else 
if (sat1 t1)=False then True else Neg(sat1 t1)
|And(t1,t2) -> if (sat1 t1)=(sat1 (Neg (t1))) then False
else if ((sat1 t1) = True && (sat1 t1) = True) then True
else if ((sat1 t1) = False && (sat1 t2) = False ) then False
else True
|Or(t1,t2) -> if ((sat1 t1)=False && (sat1 t2)= False) then 
False else True
|Imply(t1,t2) -> if ((sat1 t1)=True && (sat1 t2)=False) then 
False else True
|Iff(t1,t2) -> if (sat1 t1 = sat1 t2) then True else False
;;


let rec sat : formula -> bool= fun f ->
match f with
|True -> true
|False -> false
|Var (a) -> true
|_ -> sat (sat1 f);;
