  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec weightbr : branch -> int
  =fun br -> match br with
    | SimpleBranch (l,w) -> w
    | CompoundBranch (l,mb) -> (match mb with
      | (lb, rb) -> (weightbr lb) + (weightbr rb))
  
  let rec calcbr : branch -> int
  = fun br -> match br with
    | SimpleBranch (l,w) -> w * l
    | CompoundBranch (l,mb) -> (match mb with
      | (lb, rb) -> ((weightbr lb) + (weightbr rb)) * l)
  
  let rec evalmob : mobile -> bool
  = fun (lb, rb) -> 
    let calclb = (match lb with
    | SimpleBranch (l,w) -> w * l
    | CompoundBranch (l,mb) -> if (evalmob mb) = false then -1
      else (match mb with
      | (llb, lrb) -> ((weightbr llb) + (weightbr lrb)) * l)
    ) in
    let calcrb = (match rb with
    | SimpleBranch (l,w) -> w * l
    | CompoundBranch (l,mb) -> if (evalmob mb) = false then -2
      else (match mb with
      | (rlb, rrb) -> ((weightbr rlb) + (weightbr rrb)) * l)
    ) in
    if (calclb = calcrb) then true else false
  
  let balanced : mobile -> bool
  =fun (lb,rb) -> evalmob (lb,rb)
