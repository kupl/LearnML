
  type mobile = branch * branch
  and branch =
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec cnt : branch -> int = fun m->
match m with
|SimpleBranch(a,b)-> b
|CompoundBranch(a,b)-> match b with
        |(x,y)->cnt(x)+cnt(y)

let rec cal :branch -> int = fun m ->
match m with
|SimpleBranch(a,b)-> a*b
|CompoundBranch(a,b)-> match b with
        |(x,y)->if balanced(x,y) then  a*cnt(x)+a*cnt(y)
                else (-1)
and  balanced : mobile -> bool
  = fun mob ->
match mob with
|(a,b) -> if cal(a)=(-1) then false
        else if cal(b)=(-1) then false
        else  cal(a)=cal(b)