
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec getWeight: mobile -> int
  = fun mob -> match mob with
  | SimpleBranch(al,aw), SimpleBranch(bl,bw) -> aw + bw
  | SimpleBranch(al,aw), CompoundBranch(bl,bm) -> aw + getWeight (bm)
  | CompoundBranch(al,am), SimpleBranch(bl,bw) -> getWeight (am) + bw
  | CompoundBranch(al,am), CompoundBranch(bl,bm) -> getWeight(am) + getWeight(bm)
  let balanced : mobile -> bool
  = fun mob -> match mob with
  | SimpleBranch(al, aw), SimpleBranch(bl, bw) -> if al*aw = bl*bw then true else false
  | SimpleBranch(al, aw), CompoundBranch(bl, bm) -> if al*aw = bl*getWeight(bm) then true else false
  | CompoundBranch(al,am), SimpleBranch(bl, bw) -> if al*getWeight(am) = bl*bw then true else false
  | CompoundBranch(al, am), CompoundBranch(bl, bm) -> if al*getWeight(am) = bl*getWeight(bm) then true else false
