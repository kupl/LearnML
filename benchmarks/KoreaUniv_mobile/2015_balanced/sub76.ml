  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec balanced : mobile -> bool
  =fun (lb,rb) -> 
    let rec sum_weight m =
      match m with l,r ->
        match l,r with
        | SimpleBranch (_,w1), SimpleBranch (_,w2) -> w1 + w2
        | CompoundBranch (_,mo1), SimpleBranch (_,w1) -> sum_weight mo1 + w1
        | CompoundBranch (_,mo1), CompoundBranch (_,mo2)  -> sum_weight mo1 + sum_weight mo2
        | SimpleBranch (_,w1), CompoundBranch (_,mo1)  -> w1 + sum_weight mo1
    in

    match lb,rb with
    | SimpleBranch (l1,w1), SimpleBranch (l2,w2) -> if l1*w1 == l2*w2 then true else false
    | CompoundBranch (l1,mo1), SimpleBranch (l2,w1) -> if balanced mo1 && (l1*sum_weight mo1 == l2*w1) then true else false
    | CompoundBranch (l1,mo1), CompoundBranch (l2,mo2)  -> if balanced mo1 && balanced mo2 && (l1*sum_weight mo1 == l2*sum_weight mo2) then true else false
    | SimpleBranch (l1,w1), CompoundBranch (l2,mo1)  -> if balanced mo1 && (l2*sum_weight mo1 == l1*w1) then true else false
