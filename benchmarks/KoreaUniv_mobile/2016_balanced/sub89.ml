
  type mobile = branch * branch
  and branch = 
      | SimpleBranch of length * weight
      | CompoundBranch of length * mobile
  and length = int
  and weight = int



  let balanced : mobile -> bool
    = fun mob -> 
      let rec mfilter b = (*make mobile has two simple branch -- return sum of below branches' weight*)
        match b with
          |(SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> w1+w2
          |(SimpleBranch(l1,w1), CompoundBranch(l2,m)) -> w1 + (mfilter m)
          |(CompoundBranch(l1,m), SimpleBranch(l2,w2)) -> (mfilter m) + w2
          |(CompoundBranch(l1,m1), CompoundBranch(l2,m2)) -> (mfilter m1)+ (mfilter m2)

      in match mob with(*top level of mobile*)
        |(SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> (l1*w1) = (l2*w2)
        |(SimpleBranch(l1,w1), CompoundBranch(l2,m)) -> (l1*w1) = (l2*(mfilter m))
        |(CompoundBranch(l1,m), SimpleBranch(l2,w2)) -> (l1*(mfilter m)) = (l2*w2)
        |(CompoundBranch(l1,m1), CompoundBranch(l2,m2)) -> (l1*(mfilter m1)) = (l2*(mfilter m2))
