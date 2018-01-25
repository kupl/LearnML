(* 2011210039 Kang Seungwoo *)

exception Problem;;

(* Problem 1 *)

let rec pascal (x,y)=
if y=0 then 1
else if x=y then 1
else if x<y then raise Problem
else pascal(x-1,y) + pascal(x-1,y-1);;


(* Problem 2 *)

let rec sigma f a b=
if a=b then f a
else f a + sigma f (a+1) b;;


(* Problem 3 *)

let rec max: int list -> int =
fun l ->
match l with hd::tl ->
if tl=[] then hd
else if hd > max tl then hd
else max tl;;

let rec min: int list -> int =
fun l ->
match l with hd::tl ->
if tl=[] then hd
else if hd < min tl then hd
else min tl;;

(* warning이 뜨지만 if tl=[]에서 이 경우를 잡아내므로 문제없습니다. *)
(* 원래 다른 문제들처럼 함수를 정의하였는데 type을 explicit하게 정의하지 않았을 때 함수의 형식이 a' list -> a'로 정의되어서 이 문제만 type을 정의하는 코딩을 하였습니다. *)


(* Problem 4 *)

type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula;;

let rec eval f =
match f with
True -> true
| False -> false
| Neg s -> not (eval s)
| Or (s, t) -> (eval s) || (eval t)
| And (s, t) -> (eval s) && (eval t)
| Imply (s, t) -> (not (eval s)) || (eval t)
| Equiv (s, t) -> (eval s) = (eval t);;


(* Problem 5 *)

type nat = ZERO | SUCC of nat;;

let rec oneadd a =
match a with
ZERO -> ZERO
| SUCC (a1) -> SUCC (oneadd a1);;

let rec natadd a b =
match b with
ZERO ->  oneadd a
| SUCC (b1) -> SUCC (natadd a b1);;

let rec natmul a b = 
match b with
ZERO -> ZERO
| SUCC (b1) -> natadd a (natmul a b1);;
