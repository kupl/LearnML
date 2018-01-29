
  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
    | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
  |(b1,b2) -> if balancing b1 && balancing b2 then (if torque b1=torque b2 then true else false) else false
    and torque bran =
      match bran with
      |SimpleBranch (len,wei) -> len * wei
      |CompoundBranch (len,mob) -> len * (torque2 mob)
    and torque2 immo =
      match immo with
      | (a,b) -> torque3 a + torque3 b
    and torque3 branw =
      match branw with
      | SimpleBranch (_,a) -> a
      | CompoundBranch (_,a) -> torque2 a
    and balancing branc =
      match branc with
      |SimpleBranch (l,w) -> true
      |CompoundBranch (l,m) -> balanced m