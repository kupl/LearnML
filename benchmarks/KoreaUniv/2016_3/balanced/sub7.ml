
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

 let rec cal_weight: branch -> int
  = fun branch ->
  match branch with
  | SimpleBranch (l, w) -> w
  | CompoundBranch (l, (br1, br2)) -> (cal_weight br1) + (cal_weight br2);;

  let balanced : mobile -> bool
  = fun mob -> 
  match mob with
  | ((SimpleBranch (l1, w1)), (SimpleBranch (l2,w2))) -> if (l1 *w1) == (l2 *w2) then true
    else false
  | ((SimpleBranch (l1, w1)), (CompoundBranch (l2, (br1, br2)))) ->
     if (l1 * w1) == (l2 * cal_weight (CompoundBranch (l2, (br1, br2)))) then true
    else false
  | ((CompoundBranch (l1, (br1, br2))), (SimpleBranch (l2,w2))) ->
     if (l1 * cal_weight (CompoundBranch (l1, (br1, br2)))) == (l2 * w2) then true
    else false
  | ((CompoundBranch (l1, (br11, br12))), (CompoundBranch (l2, (br21, br22)))) ->
    if (l1 * cal_weight (CompoundBranch (l1, (br11, br12)))) ==
    (l2 * cal_weight (CompoundBranch (l2, (br21, br22)))) then true
    else false;;