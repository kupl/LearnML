
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let balanced : mobile -> bool
  = fun mob -> 
    let rec subFun : mobile -> weight * bool
    = fun m ->
    match m with
    | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> if l1 * w1 = l2 * w2 then (w1 + w2, true)
                                                             else (w1 + w2, false)
    | (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> let (mw2, mb2) = subFun m2 in
                                                                if mb2 && (l1 * w1 = l2 * mw2) then (w1 + mw2, true)
                                                                else (w1 + mw2, false)
    | (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> let (mw1, mb1) = subFun m1 in
                                                                if mb1 && (l1 * mw1 = l2 * w2) then (mw1 + w2, true)
                                                                else (mw1 + w2, false)
    | (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> let (mw1, mb1) = subFun m1 in 
                                                                  let (mw2, mb2) = subFun m2 in
                                                                  if mb1 && mb2 && (l1 * mw1 = l2 * mw2) then (mw1 + mw2, true)
                                                                  else (mw1 + mw2, false)
  in let (mw, mb) = subFun mob in
    if mb then true else false