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

