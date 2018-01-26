
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec weight : mobile -> int
  = fun mob ->
    let (br1, br2) = mob in
    let w1 =
      (match br1 with
      | SimpleBranch (len, wgh) -> wgh
      | CompoundBranch (len, m) -> weight m) in
    let w2 =
      (match br2 with
      | SimpleBranch (len, wgh) -> wgh
      | CompoundBranch (len, m) -> weight m) in
      w1 + w2

  let rec torque : branch -> int
  = fun br ->
    match br with
    | SimpleBranch (len, wgh) -> len * wgh
    | CompoundBranch (len, mob) -> len * weight mob

  let rec balanced : mobile -> bool
  = fun mob ->
    let (br1, br2) = mob in
    let toq1 = torque br1 in
    let toq2 = torque br2 in
    let chk1 =
      (match br1 with
      | SimpleBranch (_, _) -> true
      | CompoundBranch (_, m) -> balanced m) in
    let chk2 =
      (match br2 with
      | SimpleBranch (_, _) -> true
      | CompoundBranch (_, m) -> balanced m) in
      if chk1 then
        if chk2 then
          if toq1 = toq2 then true else false
        else false
      else false