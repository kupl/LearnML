
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec we : branch -> int =fun b -> match b with |SimpleBranch(l,w)->w |CompoundBranch(l,(b1,b2))->we b1 + we b2;;
  let rec  balanced : mobile -> bool
  = fun mob -> match mob with |(SimpleBranch(l,w),SimpleBranch(l1,w1))-> if l*w=l1*w1 then true else false |(SimpleBranch(l,w),CompoundBranch(l2,m))-> if l*w=l2*we(CompoundBranch(l2,m))&&balanced(m) then true else false | (CompoundBranch(l2,m),SimpleBranch(l,w))->if l*w=l2*we(CompoundBranch(l2,m))&&balanced(m) then true else false |(CompoundBranch(l,m),CompoundBranch(l1,m1))->if l*we(CompoundBranch(l,m))=l1*we(CompoundBranch(l1,m1))&&balanced(m)&&balanced(m1) then true else false;; (* TODO *)
