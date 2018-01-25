  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec weight brs = 
    match brs with
    |SimpleBranch (a, b) -> b
    |CompoundBranch (a, (lb, rb)) -> weight(lb) + weight(rb)

  let torque br = 
    match br with 
    |SimpleBranch (a, b) -> a * b
    |CompoundBranch (a, (lb, rb)) -> a * ((weight lb) + (weight rb))
  
  let balanced : mobile -> bool
  =fun (lb,rb) -> 
    if torque(lb) = torque(rb) then true else false
