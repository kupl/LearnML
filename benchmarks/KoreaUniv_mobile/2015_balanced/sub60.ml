  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
 
  let rec bal : mobile -> int
  =fun (lb,rb) -> match lb,rb with
  SimpleBranch(l1,w1),SimpleBranch(l2,w2)-> w1+w2
  |CompoundBranch(l1,m1),SimpleBranch(l2,w2) -> bal(m1)+w2
  |SimpleBranch(l1,w1),CompoundBranch(l2,m) -> w1+bal(m)
  |CompoundBranch(l1,m1),CompoundBranch(l2,m2) -> bal(m1)+bal(m2)
  
 let rec balanced : mobile -> bool
  =fun(lb,rb) -> match lb,rb with
  SimpleBranch(l1,w1),SimpleBranch(l2,w2) -> if(l1*w1)!=(l2*w2)
   then false else true
  |CompoundBranch(l1,m1),SimpleBranch(l2,w2) -> if balanced(m1) then (if((bal(m1)*l1)=(l2*w2))
   then true else false) else false
  |SimpleBranch(l1,w1),CompoundBranch(l2,m) -> if balanced(m) then(if((l1*w1)=(bal(m)*l2))
   then true else false) else false
  |CompoundBranch(l1,m1),CompoundBranch(l2,m2) ->if balanced(m1)&&balanced(m2) 
then( if((l1*bal(m1))=(l2*bal(m2))) then true else false) else false
