type mobile = branch * branch
and branch = SimpleBranch of length * weight
          | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m ->
  let rec bwei : branch -> int 
  = fun b -> 
    match b with 
    | SimpleBranch(_ , wei) -> wei
    | CompoundBranch( len, (b1,b2)) -> (bwei b1) + (bwei b2)
  in
  let rec bTorq : branch -> int
  = fun b ->
    match b with
    | SimpleBranch(len, wei) -> len * wei
    | CompoundBranch(len , _) -> len * (bwei b)
  in
  match m with 
  | (lb,rb) -> if((bTorq lb) <> (bTorq rb)) then false
    else
    let rec balancedB : branch -> bool
    = fun br -> 
      match br with
      | SimpleBranch(_,_) -> true
      | CompoundBranch(len, mob) -> balanced mob
    in (balancedB lb) && (balancedB rb);;