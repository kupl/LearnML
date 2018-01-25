(*********************)(*2014210077김준영*)
(* Problem 1: filter *)
(*********************)

let rec filter p l =
   match l with 
   | []-> []
   | hd::tl ->if p hd then hd::(filter p tl) 
    else (filter p tl)

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper (a,b) =
 match b with 
 |[]->a
 |hd::tl->
  match a with
   |[]->b
   |h::t-> h::hd::(zipper (t,tl))

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter (n,f)= 
 if n=0 then (fun x->x+0)
 else if n<0 then raise (Failure "negative value of n")
 else fun x->f((iter(n-1,f) x))

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff (sth,y)=
  match sth with 
   | Const x-> Const 0 
   | Var x -> 
    if x=y then Const 1 else Const 0 
   | Power (x,a)-> 
   if x=y then 
    if a=0 then Const 0 
    else if a=1 then Const 1 
    else if a<0 then raise (Failure "negative powered")  
  else Times[Const a;Power(x,a-1)] 
  else Const 0
   | Times l -> 
   if List.mem (Const 0) l then Const 0 
   else
     (match l with 
     |hd::[] -> diff(hd,y) (*times라는 리스트에 원소가 하나 있을때, 그것만 미분한다 *) 
     |hd::tl ->Sum[Times[diff(hd,y);Times tl];Times[hd;diff(Times(tl),y)]])
     (*여러 요소가 있을 때, f와 g가 x에 관한 함수라고 할때, (f*g)를 미분하면 (f*g)'=f'g+fg' 와 같다를 이용*)
   | Sum l ->
    match l with 
    | hd::[] -> diff(hd,y)
    | hd::tl -> Sum[diff(hd,y);diff(Sum(tl),y)]


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

let rec calf f x =
  match f with 
  |X -> (match x with 
    |INT (n) -> n  
    |_ -> raise (Failure "argument must be INT"))
  |INT n -> n 
  |ADD (a,b)-> (calf a x) + (calf b x)
  |SUB (a,b)-> (calf a x) - (calf b x)
  |MUL (a,b)-> (calf a x) * (calf b x)
  |DIV (a,b)-> (calf a x) / (calf b x)
(*sigma함수를 돕기위한 보조함수, 먼저 선언해주었음. 시그마안에 있는 함수를 f(x)라고 할 때 그 함숫값을 계산해주는 함수이다.*)
  let rec sigma (a,b,c)=
    if (calf a a)(*이렇게 표현하면 calf함수 내에서 INT가 없는 int형으로 반환 가능*)<(calf b b) then (calf c a)+sigma(INT((calf a a)+1),b,c)
    else (calf a c)
(* calculator의 SIGMA를 계산해주는 내용 함수. *)
let rec calculator op= 
  match op with
  | INT (num) -> num
  | ADD (a,b) -> calculator(a) + calculator (b)
  | SUB (a,b) -> calculator(a) - calculator (b)
  | MUL (a,b) -> calculator(a) * calculator (b)
  | DIV (a,b) -> calculator(a) / calculator (b)
  | SIGMA (a,b,c) -> 
    (match c with 
    |SIGMA(q,w,e)-> (calculator(b)-calculator(a)+1)*sigma(q,w,e)
    |_->sigma(a,b,c)
  )







