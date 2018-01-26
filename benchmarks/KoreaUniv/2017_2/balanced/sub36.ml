type mobile = branch * branch 
    and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
    and length = int
    and weight = int

let balanced : mobile -> bool
= fun m ->
  let rec balanced_ (branch1, branch2) = 
    let lengthOfBranch branch = 
      match branch with
      | SimpleBranch (l,_) -> l
      | CompoundBranch (l,_) -> l 
    in
    let rec weightOfBranch branch = 
      match branch with
      | SimpleBranch (_,w) -> w
      | CompoundBranch (_,m) -> weightOfMobile m 
    and weightOfMobile (branch1, branch2) = 
      (weightOfBranch branch1) +
      (weightOfBranch branch2) 
    in
    let torqueOfBranch branch = 
      (lengthOfBranch branch) *
      (weightOfBranch branch) 
    in
      (balancedBranch branch1) &&
      (balancedBranch branch2) && 
      (torqueOfBranch branch1) = (torqueOfBranch branch2)
  and balancedBranch branch =
    match branch with
    | SimpleBranch _ -> true
    | CompoundBranch (l,m) -> balanced_ m
  in 
  balanced_ m