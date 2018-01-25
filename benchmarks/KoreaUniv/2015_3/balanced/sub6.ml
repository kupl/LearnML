  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec balanced : mobile -> bool
  =fun (lb,rb) ->
  match lb,rb with
  SimpleBranch(l1,w1),SimpleBranch(l2,w2) -> l1*w1 = l2*w2
  |CompoundBranch(l1,m1),SimpleBranch(l2,w2)-> if balanced m1 then l1*(sum m1) = l2*w2 else false
  |SimpleBranch(l1,w1),CompoundBranch(l2,m2)-> if balanced m2 then l1*w1 = l2*(sum m2) else false
  |CompoundBranch(l1,m1),CompoundBranch(l2,m2)->if balanced m1 && balanced m2 then l1*(sum m1) = l2*(sum m2) else false

  and sum : mobile->int
  =fun (lb,rb)->
  match lb,rb with
  SimpleBranch(l1,w1),SimpleBranch(l2,w2) -> w1+w2
  |CompoundBranch(l1,m1),SimpleBranch(l2,w2)-> (sum m1) + w2
  |SimpleBranch(l1,w1),CompoundBranch(l2,m2)-> w1 + (sum m2)
  |CompoundBranch(l1,m1),CompoundBranch(l2,m2)-> (sum m1) + (sum m2)

  let run : mobile -> bool
  = fun m -> balanced m
