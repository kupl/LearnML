  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match lb,rb with
| SimpleBranch(a,b),SimpleBranch(c,d) -> if (a*b)=(c*d) then true else false
| CompoundBranch(a,b),SimpleBranch(c,d) -> if balanced(b)=true 
  then balanced(SimpleBranch(a,sumweight(b)),SimpleBranch(c,d))
  else false
| SimpleBranch(a,b),CompoundBranch(c,d) -> if balanced(d)=true
  then balanced(SimpleBranch(a,b),SimpleBranch(c,sumweight(d)))
  else false
| CompoundBranch(a,b),CompoundBranch(c,d) -> if balanced(b)=true && balanced(d)=true
  then balanced(SimpleBranch(a,sumweight(b)),SimpleBranch(c,sumweight(d)))
  else false

and sumweight n = match n with
| SimpleBranch(a,b),SimpleBranch(c,d) -> b+d
| CompoundBranch(a,b),SimpleBranch(c,d) -> sumweight(b)+d
| SimpleBranch(a,b),CompoundBranch(c,d) -> b+sumweight(d)
| CompoundBranch(a,b),CompoundBranch(c,d) -> sumweight(b)+sumweight(d)
