  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let balanced : mobile -> bool
  =fun (lb,rb) -> false 

  let rec getw m=
  match m with 
  | (l,r)->
   match l with 
    |SimpleBranch(a,b)->
     (match  r with
     | SimpleBranch(c,d)-> b+d
     | CompoundBranch(e,f)-> getw(f)+b
    )
    |CompoundBranch(a,b)-> 
    (match  r with
     | SimpleBranch(c,d)-> getw(b)+d
     | CompoundBranch(e,f)-> getw(f)+getw(b)
    )

let rec balanced m =
  match m with 
  |(l,r)-> 
   (match l with 
    | SimpleBranch (a,b) ->
     (match r with 
      |SimpleBranch(len,weight)-> 
      if len*weight= a*b then true else false
      |CompoundBranch(c,d)-> 
      if balanced(d) then 
        if c*getw(d)= a*b then true else false
      else false 
    )
    | CompoundBranch(e,f)->
    if balanced(f) then
     (match r with 
      |SimpleBranch(len,weight)-> 
      if len*weight= e*getw(f) then true else false
      |CompoundBranch(c,d)-> 
      if c*getw(d)= e*getw(f) then true else false
    )
    else false
    )
