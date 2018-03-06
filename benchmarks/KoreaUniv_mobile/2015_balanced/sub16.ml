  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec getWeight : mobile -> int
=fun (lb,rb) ->
(match lb with
SimpleBranch(l,w) -> w |
CompoundBranch(l,m) -> getWeight(m)) +
(match rb with
SimpleBranch(l,w) -> w |
CompoundBranch(l,m) -> getWeight(m));;

let rec cal : branch -> int
=fun br -> (match br with
SimpleBranch(l,w) -> l*w|
CompoundBranch(l,m) -> if balanced(m) then l*getWeight(m) else 0)

and balanced : mobile -> bool
=fun (lb,rb) -> if cal(lb)=cal(rb) && cal(lb)>0 && cal(rb)>0 then true else false;;
