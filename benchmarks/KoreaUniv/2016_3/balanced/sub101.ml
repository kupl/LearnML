
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec cal : branch -> int
  = fun br ->
  match br with
  |SimpleBranch(l, w) -> w 
  |CompoundBranch(l, (b1, b2)) -> (cal (b1))+ (cal (b2))


  let rec balanced : mobile -> bool
  = fun mob -> 
  match mob with
  |(SimpleBranch(l1, w1), SimpleBranch(l2, w2)) ->
    if (l1*w1=l2*w2) then true else false
  |(CompoundBranch( l, m ) ,SimpleBranch(l1, w1) ) -> 
    if (l*(cal (CompoundBranch(l,m))) = l1*w1 ) then true&&(balanced m) else false
  |( SimpleBranch(l1, w1), CompoundBranch( l, m )) ->  
    if (l*(cal (CompoundBranch(l,m))) = l1*w1 ) then true&&(balanced m) else false
  | ( CompoundBranch( l1, m1 ), CompoundBranch( l2, m2 )) ->
    if ( l1*(cal (CompoundBranch(l1, m1))) = l2*(cal (CompoundBranch(l2, m2)))) then true&&((balanced m1)&&(balanced m2)) else false
