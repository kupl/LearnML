  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let balanced : mobile -> bool
  =fun (x1,x2) -> 





let rec helper (b1,b2) =
match b1,b2 with
SimpleBranch(l1,w1), SimpleBranch(l2,w2)-> w1 + w2

|CompoundBranch(l1,(subbran1,subbran2)),SimpleBranch(l2,w2)->
helper((subbran1,subbran2)) + w2

|SimpleBranch(l2,w2),CompoundBranch(l1,(subbran1,subbran2))->
helper((subbran1,subbran2)) + w2

|CompoundBranch(l1,(subbran1,subbran2)),CompoundBranch(l2,(subbran3,subbran4))->
helper((subbran1,subbran2)) + helper((subbran3,subbran4))in

match x1,x2 with
SimpleBranch(l1,w1), SimpleBranch(l2,w2) -> if((l1*w1)==(l2*w2)) then true else false
|SimpleBranch(l1,w1), CompoundBranch(l2,(a,b))->if((l1*w1)==(l2*(helper(a,b)))) then true else false
|CompoundBranch(l1,(a,b)),SimpleBranch(l2,w2)->if((l1*(helper(a,b)))==l2*w2) then true else false
|CompoundBranch(l1,(a1,b1)),CompoundBranch(l2,(a2,b2))->if((l1*(helper(a1,b1)))==(l2*(helper(a2,b2)))) then true else false;;


