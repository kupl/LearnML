  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let balanced : mobile -> bool
  =fun (lb,rb) -> false

  let rec wei (b) =
    match b with
    |SimpleBranch(n,w) -> w
    |CompoundBranch(n,b) -> match b with |(lb,rb) -> wei(lb)+wei(rb)

  let rec bal mobile = match mobile with |(lb,rb) ->(
    match (lb,rb) with
    |(CompoundBranch (n1,b1),CompoundBranch (n2,b2)) -> if n1 * wei(lb) = n2 * wei(rb) then true&&(bal(b1)&&bal(b2)) else false
    |(SimpleBranch(n1,w1),CompoundBranch (n2,b2)) -> if n1 * w1 = n2 * wei(rb) then true&&bal(b2) else false
    |(CompoundBranch (n1,b1),SimpleBranch(n2,w2)) -> if n1 * wei(lb) = n2 * w2 then true&&bal(b1) else false
    |(SimpleBranch (n1,w1),SimpleBranch(n2,w2)) -> if n1 * w1 = n2 * w2 then true else false
  )

  let balanced mobile = bal(mobile)
