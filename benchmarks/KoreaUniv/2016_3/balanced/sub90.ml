
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int


  let rec branchtoweight : branch -> int
  = fun brc ->
  begin
      match brc with
      |SimpleBranch (_,w) -> w
      |CompoundBranch (_,(b1,b2)) -> branchtoweight b1+ branchtoweight b2
  end



  let rec balanced : mobile -> bool
  = fun mob ->  (* TODO *)
  begin
    match mob with
    |SimpleBranch (l1,w1) , SimpleBranch (l2,w2) -> if (l1*w1 != l2*w2) then false else true
    |CompoundBranch (l1,(b1,b2)) , SimpleBranch (l2,w) -> if (l1*(branchtoweight b1) + l1*(branchtoweight b2) != l2*w) then false else balanced (b1,b2)
    |SimpleBranch (l2,w) , CompoundBranch (l1,(b1,b2)) -> if (l1*(branchtoweight b1) + l1*(branchtoweight b2) != l2*w) then false else balanced (b1,b2)
    |CompoundBranch (l1,(b1,b2)) , CompoundBranch (l2,(b3,b4)) -> if (l1*(branchtoweight b1) + l1*(branchtoweight b2) != l2*(branchtoweight b3) + l2*(branchtoweight b4)) then false else balanced (b1,b2) && balanced (b3,b4)