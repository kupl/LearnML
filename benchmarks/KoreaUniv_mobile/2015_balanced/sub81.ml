  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec helper (b1,b2) =
match b1,b2 with
SimpleBranch(l1,w1),SimpleBranch(l2,w2) -> if (l1*w1=l2*w2) then w1+w2 else -10000
| CompoundBranch(l1,(sub1,sub2)),SimpleBranch(l2,w2) -> helper(sub1,sub2) + w2
| SimpleBranch(l1,w1),CompoundBranch(l2,(sub1,sub2)) -> w1 + helper(sub1,sub2)
| CompoundBranch(l1,(sub1,sub2)),CompoundBranch(l2,(sub3,sub4)) ->
                                                        helper(sub1,sub2) + helper(sub3,sub4);;

let rec balanced (b1,b2)=
match b1,b2 with
SimpleBranch(l1,w1),SimpleBranch(l2,w2) -> if(l1*w1) = (l2*w2) then true else false
| SimpleBranch(l1,w1),CompoundBranch(l2,(s1,s2)) ->
                if(l2*(helper(s1,s2))) = (l1*w1) then true else false
| CompoundBranch(l1,(s1,s2)),SimpleBranch(l2,w2) ->
                if(l2*w2) = (l1*(helper(s1,s2))) then true else false
| CompoundBranch(l1,(s1,s2)),CompoundBranch(l2,(s3,s4)) ->
                if(l2*(helper(s3,s4))) = (l1*(helper(s1,s2))) then true else false;;
