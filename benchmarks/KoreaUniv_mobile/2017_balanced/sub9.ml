  (* Problem 6*)
  type mobile = branch * branch (* left and right branches *)
  and branch = SimpleBranch of length * weight
              |CompoundBranch of length * mobile 
  and length = int 
  and weight = int
  
  let rec checkw
  = fun m -> match m with 
  |(l,r) -> match l with 
  |SimpleBranch(l1,w1)-> (match r with
      |SimpleBranch(l2, w2) -> w1 + w2
      |CompoundBranch(l,x) -> checkw(x) +w1)
  |CompoundBranch(l,x) -> (match r with 
      |SimpleBranch(l2,w2) -> checkw(x) +w2
      |CompoundBranch(l,y) -> checkw(x) + checkw(y))


  let rec balanced : mobile -> bool
  = fun m -> (match m with 
      |SimpleBranch(l1,w1), SimpleBranch(l2, w2) -> if (l1*w1) = (l2*w2) then true else false
      |CompoundBranch(l1,y), SimpleBranch(l2,w2) -> if balanced y = true && (checkw(y)*l1)= (l2* w2)  then true else false
      |SimpleBranch(l1,w1), CompoundBranch(l2,x) -> if balanced x =true && (l1*w1) = (l2*checkw(x)) then true else false
      |CompoundBranch(l1,x), CompoundBranch(l2,y) -> if balanced x = true && balanced y =true && (l1* checkw(x)) = (l2* checkw(y)) then true else false)


