  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec weight(branch) = match branch with
  CompoundBranch(len, mob) -> (match mob with (lb,rb) -> (weight(lb) + weight(rb)))
     | SimpleBranch(len, wei) -> wei

  let tot_weight(lb, rb) = weight(lb) + weight(rb)

  let rec balanced (lb, rb)= match lb with 
  CompoundBranch(l1, m1) -> balanced m1 && (match rb with
	| CompoundBranch(l3, m3) -> balanced m3
	| SimpleBranch(l4, w4) -> if l1*(tot_weight m1) = l4*w4 then true else false)
    | SimpleBranch(l2, w2) -> (match rb with
	| CompoundBranch(l3, m3) -> balanced m3 && (if l3*(tot_weight m3) = l2*w2 then true else false)
	| SimpleBranch(l4, w4) -> (if l2*w2 = l4*w4 then true else false))
