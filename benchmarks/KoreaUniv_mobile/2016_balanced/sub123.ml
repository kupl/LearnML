
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec getWeight : branch -> int
  = fun br -> match br with
  | SimpleBranch (l1, w1) -> w1
  | CompoundBranch (l1, m)
      -> match m with
      |(br1, br2) -> getWeight(br1) + getWeight(br2)

  let lengthOf : branch -> int
  = fun br -> match br with
  | SimpleBranch (l1, w1) -> l1
  | CompoundBranch (l1, m) -> l1


  let branchBalanced : mobile -> bool
  = fun mob -> match mob with
  | (br1, br2) -> (lengthOf br1) * (getWeight br1) = (lengthOf br2) * (getWeight br2)


  let rec balanced : mobile -> bool
  = fun mob -> match mob with
  | (br1, br2) -> match br1, br2 with
    | SimpleBranch (l1, w1), SimpleBranch (l2, w2) -> (branchBalanced mob)
    | CompoundBranch (l1, m1), SimpleBranch (l2, w2) -> (balanced m1) && (branchBalanced mob)
    | SimpleBranch (l1, w1), CompoundBranch (l2, m2) -> (balanced m2) && (branchBalanced mob)
    | CompoundBranch (l1, m1), CompoundBranch (l2, m2) -> (balanced m1) && (balanced m2)
