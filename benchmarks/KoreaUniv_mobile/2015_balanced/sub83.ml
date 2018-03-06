  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec branchWeight : branch -> weight 
  =fun (x) ->
  match x with
  |SimpleBranch(l,w) -> w
  |CompoundBranch(l,w) -> let rec mobileWeight : mobile -> int
  =fun (y) ->
  match y with
  |(a,b) -> branchWeight(a) + branchWeight(b) in mobileWeight(w)

  let rec mobileWeight : mobile -> int
  =fun (m) ->
  match m with
  |(a,b) -> branchWeight(a) + branchWeight(b)

  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match lb with 
  |SimpleBranch (l1,w1) ->(
      match rb with
      |SimpleBranch (l2,w2) ->
         if (l1*w1=l2*w2) then true else false
      |CompoundBranch (l2,w2) -> 
         if (balanced(w2)) then if (l1*w1=l2*mobileWeight(w2)) then true else false else false
            )
  |CompoundBranch (l1,w1) -> 
      (match rb with
      |SimpleBranch (l2,w2) ->
         if (balanced(w1)) then if (l2*w2=l1*mobileWeight(w1)) then true else false else false
      |CompoundBranch (l2,w2) -> 
           if(balanced(w1)&&balanced(w2))  then if (l2*mobileWeight(w2)=l1*mobileWeight(w1)) then true else false else false
    )
