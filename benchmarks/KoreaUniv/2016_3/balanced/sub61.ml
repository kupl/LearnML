
  type mobile = branch * branch
  and branch =
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let balanced : mobile -> bool
  = fun mob -> 
      let rec mob_check m = match m with
      | (  SimpleBranch (ll, lw),   SimpleBranch (rl, rw)) -> (ll * lw = rl * rw, lw + rw)
      | (  SimpleBranch (ll, lw), CompoundBranch (rl, rm)) ->
          let (rb, rw) = mob_check rm in (rb && ll * lw = rl * rw, lw + rw)
      | (CompoundBranch (ll, lm),   SimpleBranch (rl, rw)) ->
          let (lb, lw) = mob_check lm in (lb && ll * lw = rl * rw, lw + rw)
      | (CompoundBranch (ll, lm), CompoundBranch (rl, rm)) ->
          let (lb, lw) = mob_check lm in
          let (rb, rw) = mob_check rm in
          (lb && rb && ll * lw = rl * rw, lw + rw)
      in match mob_check mob with (b, w) -> b