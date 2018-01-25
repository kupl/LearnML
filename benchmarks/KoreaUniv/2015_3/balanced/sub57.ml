  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec balanced : mobile -> bool
  =fun (lb,rb) -> 
          if findl lb * sumwgt lb = findl rb * sumwgt rb then true
          else false
 
  and sumwgt : branch -> int
  = fun br -> match br with
  |SimpleBranch(l,w)-> w
(***********************************)
(***********************************)
  |CompoundBranch(l, (b1,b2)) -> (sumwgt b1) + (sumwgt b2)

  and findl : branch -> int
  = fun br -> match br with
  |SimpleBranch (l,w) -> l
  |CompoundBranch (l,m) -> l
