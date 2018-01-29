
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let balanced : mobile -> bool
  = fun mob -> 
  let rec weights : mobile -> int
  = fun wt -> match wt with
  |(SimpleBranch(l1,w1) ,SimpleBranch(l2,w2)) -> w1+w2
  |(SimpleBranch(l1,w1) ,CompoundBranch(l2, mob2)) -> w1 + weights mob2
  |(CompoundBranch(l1, mob1) ,SimpleBranch(l2,w2)) -> w2 + weights mob1
  |(CompoundBranch(l1, mob1) ,CompoundBranch(l2, mob2))-> weights mob1 + weights mob2
in
  match mob with
  |(SimpleBranch(l1,w1) ,SimpleBranch(l2,w2)) -> if l1*w1 = l2*w2 then true else false
  |(SimpleBranch(l1,w1) ,CompoundBranch(l2, mob2)) -> if l1*w1 = l2*(weights mob2) then true else false
  |(CompoundBranch(l1,mob1) ,SimpleBranch(l2,w2)) ->if l2*w2 = l1*(weights mob1) then true else false
  |(CompoundBranch(l1,mob1) ,CompoundBranch(l2, mob2))-> if l1*(weights mob1) = l2*(weights mob2) then true else false
