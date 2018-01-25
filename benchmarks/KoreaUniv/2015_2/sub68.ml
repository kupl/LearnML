(*********************)
(* Problem 1: filter *)
(*********************)

let rec filter pred lst = [] 
  let rec filter pred lst = match lst with | []-> [] | hd::tl -> if ((pred hd)=true) then (hd::(filter pred tl)) else (filter pred tl) ;;
                                                                   

(*********************)
(* Problem 2: zipper *)
(*********************)

let rec zipper : int list * int list -> int list
=fun (a,b) -> []
let rec zipper lst1 lst2= match lst1  with | []-> lst2 | hd:: tl -> match lst2 with | []-> hd::tl | hd1::tl1-> ( hd::hd1::(zipper tl tl1) );;


(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> f
let rec iter (n,f) num = if (n==0) then num else (iter (n-1 , f) (f num));;

(*********************)
(* Problem 4: Diff   *)
(*********************)

type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->  
match aexp with 
|Const i -> Const 0 
| Var s -> if (s=x) then Const 1 else Const 0; 
| Power ( s, i ) -> if ( (s=x) || (i<>0) ) then (Times [Const i ; Power(s,i-1) ] ) else (Const 0)

| Times (lst) ->( 
	match lst with 
	[] -> Const 0
	| hd :: tl  ->  Sum   (   [ Times  (  ( diff ( hd , x) )  ::  tl )   ]    @   [  Times (   hd ::  [ diff (  Times (tl  ), x )  ]) ]   ) 
)

| Sum (lst) -> (
	match lst with  
		 [] -> Const (0)
		| hd::tl -> Sum   (  [diff ( hd , x)]  @  [ diff ( Sum tl , x) ]   )
);;

(*************************)
(* Problem 5: Calculator *)
(*************************)

type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
=fun e -> 0
 
let rec three e1 a= match e1 with 

|X -> a
|INT i -> i
|ADD(v1,v2) ->  (three v1 a) + (three v2 a) 
|SUB(v1,v2) ->  (three v1 a) - (three v2 a) 
|MUL(v1,v2) ->  (three v1 a) * (three v2 a) 
|DIV(v1,v2) ->  (three v1 a) / (three v2 a) 
|SIGMA(v1,v2,v3) -> calculator e1

let rec calculator e= match e with 

|X -> raise (Failure " WRONG")
|INT i -> i
|ADD(e1,e2) ->  (calculator e1) + (calculator e2)
|SUB(e1,e2) ->  (calculator e1) - (calculator e2)
|MUL(e1,e2) ->  (calculator e1) * (calculator e2)
|DIV(e1,e2) ->  (calculator e1) / (calculator e2)
|SIGMA(e1,e2,e3) ->  if ( (calculator e1)< (calculator e2) ) 
then three e3 (calculator e1) + calculator (SIGMA (ADD (e1,INT (1)),e2,e3)) 
else three e3 (calculator e1) 