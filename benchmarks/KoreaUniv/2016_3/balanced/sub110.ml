
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

   let rec mobilew : mobile -> int
   = fun mob ->
   match mob with
   | (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> w1+w2
   | (SimpleBranch(l1,w1),CompoundBranch(l2,mobile)) -> w1 + (mobilew mobile)
   | (CompoundBranch(l1,mobile),SimpleBranch(l2,w2)) -> (mobilew mobile)+w2
   | (CompoundBranch(l1,mobile1),CompoundBranch(l2,mobile2)) -> (mobilew mobile1) + (mobilew mobile2)

  let rec balanced : mobile -> bool
  = fun mob -> (* TODO *)
   match mob with
   | (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> if (l1*w1) = (l2*w2) then true else false

   | (SimpleBranch(l1,w1),CompoundBranch(l2,mobile2)) -> 
      if balanced(mobile2) = false then false 
      else if (l1*w1) = (l2 * (mobilew mobile2)) then true else false

   | (CompoundBranch(l1,mobile1),SimpleBranch(l2,w2)) ->
      if balanced(mobile1) = false then false
      else if (l1 * (mobilew mobile1)) = (l2*w2) then true else false

   | (CompoundBranch(l1,mobile1),CompoundBranch(l2,mobile2)) ->
      if balanced(mobile1) = false || balanced(mobile2) = false then false
      else if (l1 * (mobilew mobile1)) = (l2 * (mobilew mobile2)) then true else false
