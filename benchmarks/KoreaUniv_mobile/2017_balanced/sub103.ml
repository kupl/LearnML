(* problem 6*)
  type mobile = branch * branch     (* left and rigth branches *)
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
             and length = int
             and weight = int

   let rec balanced : mobile -> bool
   = fun m -> (* TODO*)
   let rec branchweight branch = 
   match branch with
   |SimpleBranch (a,b) -> b
   |CompoundBranch (a,(b1,b2))->(branchweight b1)+(branchweight b2)
   in
   match m with
   |SimpleBranch(a1,b1),SimpleBranch(a2,b2)->if a1*branchweight(SimpleBranch(a1,b1))=a2*branchweight(SimpleBranch(a2,b2)) then true else false
   |SimpleBranch(a1,b1),CompoundBranch(a2,(b2,b3))->if a1*branchweight(SimpleBranch(a1,b1))=a2*branchweight(CompoundBranch(a2,(b2,b3))) then balance(b2,b3) else false
   |CompoundBranch(a1,(b1,b2)),SimpleBranch(a2,b3)->if a1*branchweight(CompoundBranch(a1,(b1,b2)))=a2*branchweight(SimpleBranch(a2,b3)) then balance(b1,b2) else false
   |CompoundBranch(a1,(b1,b2)),CompoundBranch(a2,(b3,b4))->if a1*branchweight(CompoundBranch(a1,(b1,b2)))=a2*branchweight(CompoundBranch(a2,(b3,b4))) then balance(b1,b2)&&balance(b3,b4) else false

   