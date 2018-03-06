  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec sumweight : mobile -> int
  =fun (lb, rb) ->  match lb, rb with
  |SimpleBranch (ll, lw), SimpleBranch (rl, rw)-> lw + rw
  |CompoundBranch (ll, lm), SimpleBranch (rl, rw)-> (sumweight lm) + rw
  |SimpleBranch (ll, lw), CompoundBranch (rl, rm) -> lw + (sumweight rm)
  |CompoundBranch (ll, lm), CompoundBranch (rl, rm) -> (sumweight lm) +  (sumweight rm)

  let toque : branch -> int
  =fun b -> match b with
  |SimpleBranch (l, w) -> l * w
  |CompoundBranch (l, m) -> l * sumweight (m)

  let balsub : mobile -> bool
  =fun (lb, rb) -> toque lb = toque rb

  let rec  balanced : mobile -> bool
  =fun (lb,rb) -> match lb, rb with
  |SimpleBranch (ll, lw), SimpleBranch (rl, rw) -> balsub (lb, rb)
  |CompoundBranch (ll, lm), SimpleBranch (rl, rw) -> (balanced lm)&&(balsub (lb, rb))
  |SimpleBranch (ll, lw), CompoundBranch (rl, rm) -> (balanced rm)&&(balsub (lb, rb))
  |CompoundBranch (ll, lm), CompoundBranch (rl, rm) -> (balanced lm)&&(balanced rm)&&(balsub (lb, rb))
