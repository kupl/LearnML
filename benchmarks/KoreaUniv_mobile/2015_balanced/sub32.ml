  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec total_weight : branch -> int
  =fun br -> match br with
	SimpleBranch (l, w) -> w
	|CompoundBranch (l, m) -> match m with
		(b1, b2) -> total_weight(b1) + total_weight(b2)
  
  let len : branch -> int
  =fun br -> match br with
	SimpleBranch (l, w) -> l
	|CompoundBranch (l, m) -> l
  
  let balanced : mobile -> bool
  =fun (lb,rb) -> len(lb) * total_weight(lb) = len(rb) * total_weight(rb)
