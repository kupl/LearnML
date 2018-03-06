(*6*)
type mobile = branch*branch (*left and right branches *)
and branch = SimpleBranch of length * weight
            |CompoundBranch of length * mobile
            and length = int
            and weight = int

let rec lengwei : branch->int = fun lw->
  match lw with
 |SimpleBranch(length,weight) -> length*weight
 |CompoundBranch (length,(SimpleBranch(length2,weight),SimpleBranch(length3,weight2))) -> (weight+weight2)
 |CompoundBranch (length,(branch1,branch2))-> length * (lengwei (branch1)+ lengwei(branch2));;
let balanced : mobile -> bool = fun m->
 match m with
 |(SimpleBranch(l1,w1),SimpleBranch(l2,w2))->
  if (lengwei (SimpleBranch(l1,w1))) = (lengwei (SimpleBranch(l2,w2))) then true else false
 |(CompoundBranch(l1,(br1,br2)),SimpleBranch(l2,w2))->
  if (lengwei (CompoundBranch(l1,(br1,br2)))) = (lengwei (SimpleBranch(l2,w2))) then true else false
 |(SimpleBranch(l1,w1),CompoundBranch(l2,(br1,br2)))->
  if (lengwei (SimpleBranch(l1,w1))) = (lengwei (CompoundBranch(l2,(br1,br2)))) then true else false
 |(CompoundBranch(l1,w1),CompoundBranch(l2,(br1,br2)))->
  if (lengwei (CompoundBranch(l1,(br1,br2)))) = (lengwei (CompoundBranch(l2, (br1,br2)))) then true else false;;
