(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list


let rec getlast lst size = match lst with|[]->raise(Failure "too short")|hd::tl->if size=1 then hd else getlast tl (size-1);;

let rec changelist l n e = match l with|[]->raise(Failure "too short")|hd::tl->if n=0 then e::tl else hd::changelist tl (n-1) e;;
let rec deletelist l n = match l with|[]->raise(Failure "too short")|hd::tl->if n=0 then tl else hd::deletelist tl (n-1);;

let rec diffCal : bool->aexp list->aexp list->string->int->aexp list = fun ty e t v count->
if ty=true then
let n = (length e)-(length t) in
(match t with|[]->if count=0 then [Const 0] else []
|hd::tl->(match hd with
|Const m->(diffCal true e tl v count)
|Var x->if v=x then 
	(if (length e)=1 then [(Const 1)]
		else (Times (deletelist e n)::(diffCal true e tl v (count+1))))
 else (diffCal true e tl v count)
|Power (x,m)->if v=x then
(if m=2 then Times (changelist e n (Times [Const 2;Var "x"]))::(diffCal true e tl v (count+1))
	else if m=1 then
	(if (length e)=1 then [(Const 1)]
		else Times (deletelist e n)::(diffCal true e tl v (count+1)))
else if m=0 then (diffCal true e tl v count)
else Times (changelist e n (Times [Const m;Power (x,m-1)]))::(diffCal true e tl v (count+1))
 ) else (diffCal true e tl v count)
|Times lst->diffCal true ((deletelist e n)@lst) (tl@lst) v count
|Sum lst->
let rst = (diffCal false lst lst v 0) in
if((getlast rst (length rst))=(Const 0)) then
Times (changelist e n (Sum (deletelist rst ((length rst)-1))))::(diffCal true e tl v count)
else Times (changelist e n (Sum rst))::(diffCal true e tl v (count+1))
))
else
match t with|[]->if count=0 then [Const 0] else []|hd::tl->(match hd with
|Const n->if (length e)=1 then (Const 0)::(diffCal false e tl v count) else (diffCal false e tl v count)
|Var x-> if v=x then (Const 1)::diffCal false e tl v (count+1)
else if (length e)=1 then (Const 0)::diffCal false e tl v count else diffCal false e tl v count
|Power (x,n)->if x=v then (
if n=2 then (Times [Const 2; Var "x"])::(diffCal false e tl v (count+1))
else if n=1 then (Const 1)::(diffCal false e tl v (count+1))
else if n=0 then (diffCal false e tl v count)
else (Times [Const n;Power(x,n-1)])::(diffCal false e tl v (count+1))
)
 else if (length e)=1 then (Const 0)::(diffCal false e tl v count) else (diffCal false e tl v count)
|Times lst->
let rst = (diffCal true lst lst v 0) in
if (getlast rst (length rst))=(Const 0) then
if (length e)=1 then (Const 0)::(diffCal false e tl v count)
else (diffCal false e tl v count)
else rst@(diffCal false e tl v 1)
|Sum lst->diffCal false e (tl@lst) v count
);;


let rec diff : aexp * string -> aexp = fun (e,x)->
match e with|Var n->if n=x then Const 1 else Const 0|Const n->Const 0
|Power (a,b)->if a=x then (
if b=1 then (Const 1)
else if b=0 then (Const 0)
else (Times [Const b;Power(a,b-1)])
 ) else (Const 0)
|Times k->let rst = (diffCal true k k x 0) in
	if (getlast rst (length rst))=(Const 0) then (Const 0)
	else Sum rst
|Sum k->let rst = (diffCal false k k x 0) in
	Sum rst;;