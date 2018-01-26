
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int
  let rec mobileWeight : mobile -> int
  = fun mob ->
    match mob with
    | (b1,b2) -> 
      match b1 with
      | SimpleBranch(l1,w1) -> 
        match b2 with
        | SimpleBranch(l2,w2) -> w1 + w2
        | CompoundBranch(l2,m2) -> l1 + mobileWeight m2 
      | CompoundBranch(l1,m1) ->
        match b2 with
        | SimpleBranch(l2,w2) -> mobileWeight m1 + w2
        | CompoundBranch(l2,m2) -> mobileWeight m1 + mobileWeight m2

  let balanced : mobile -> bool

  = fun mob -> 
    match mob with
    | (b1,b2) -> 
      match b1 with
      | SimpleBranch(l1,w1) -> 
        match b2 with
        | SimpleBranch(l2,w2) -> l1*w1 = l2*w2
        | CompoundBranch(l2,m2) -> l1*w1 = l2*mobileWeight m2
      | CompoundBranch(l1,m1) ->
        match b2 with
        | SimpleBranch(l2,w2) -> l1*mobileWeight m1 = l2*w2
        | CompoundBranch(l2,m2) -> l1*mobileWeight m1 = l2*mobileWeight m2
