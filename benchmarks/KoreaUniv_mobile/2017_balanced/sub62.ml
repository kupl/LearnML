(*problem 6*)

type mobile = branch * branch
and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
and length = int
and weight = int

let rec weightsum : mobile -> int
= fun n1-> 
match n1 with
SimpleBranch(a, b) , CompoundBranch(c, d) -> (b + weightsum d)
|CompoundBranch(a, b) , CompoundBranch(c, d) -> (weightsum b + weightsum d)
|CompoundBranch(a, b) , SimpleBranch(c, d) -> (weightsum b + d)
|SimpleBranch(a, b) , SimpleBranch(c, d) -> (b + d);;

let rec balanced : mobile -> bool
= fun c ->
match c with
SimpleBranch(a,b) , SimpleBranch(c,d) -> 
if a*b = c*d then true
else false
|CompoundBranch(a,b) , SimpleBranch(c,d) ->
if balanced b = true then
if a * weightsum b = c * d then true
else false
else false
|SimpleBranch(a,b) , CompoundBranch(c,d) ->
if balanced d = true then
if a*b = c*weightsum d then true
else false
else false
|CompoundBranch(a,b) , CompoundBranch(c,d) ->
if balanced b = true && balanced d = true then
if a*weightsum b = c*weightsum d then true
else false
else false;;
