  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec getweight : branch -> int
  =fun b -> match b with
    | SimpleBranch (len, w) -> w
    | CompoundBranch (len, (lb, rb)) -> getweight lb + getweight rb

  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match (lb,rb) with
    | (SimpleBranch (llen, lw), SimpleBranch (rlen, rw)) -> if (llen*lw)=(rlen*rw) then true else false
    | (SimpleBranch (_,_), CompoundBranch (rlen, rw)) -> balanced (lb, (SimpleBranch (rlen, (getweight rb))))
    | (CompoundBranch (llen, lw), SimpleBranch (_,_)) -> balanced ((SimpleBranch (llen, (getweight lb))), rb)
    | (CompoundBranch (llen, lw), CompoundBranch (rlen, rw)) -> balanced((SimpleBranch (llen, (getweight lb))), (SimpleBranch (rlen, (getweight rb))))

