(*problem 4*)

type aexp=
|Const of int
|Var of string
|Power of string * int
|Times of aexp list
|Sum of aexp list
 
let rec diff:aexp*string->aexp=fun(e,x)->
match e with
|Const a-> Const 0
|Var a-> if a=x then Const 1 else Const 0
|Power(a,b)->if a=x then Times[Const b;Power(a,(b-1))] else Times[Const 0; Var a]
|Times a->(match a with
          |hd::tl->(match tl with
            |[]->diff(hd,x)
            |_->let l1=diff(hd,x)::tl in let l2= [hd]@[diff(Times tl,x)] in let l3 =[Times l1; Times l2] in Sum l3 )
          |[]->Const 0)
|Sum a-> (match a with
          |hd::tl-> (match tl with 
                    |[]->diff(hd,x)
                    |_->let l1=diff(hd,x) in let l2=diff(Sum tl,x) in let l3                    =l1::[l2] in Sum l3) 
          |[]->Const 0)