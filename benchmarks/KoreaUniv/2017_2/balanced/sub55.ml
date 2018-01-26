(*Problem 6*)
type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec fw: mobile -> int 
= fun mb ->
  match mb with
  |(SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
  |(SimpleBranch (l1, w1), CompoundBranch (l2, mb2)) -> w1 + (fw mb2)
  |(CompoundBranch (l1, mb1), SimpleBranch (l2, w2)) -> (fw mb1) + w2
  |(CompoundBranch (l1, mb1), CompoundBranch (l2, mb2)) -> (fw mb1) + (fw mb2)

let rec balanced: mobile -> bool 
= fun mb ->
  match mb with
  |(SimpleBranch (l1, w1), SimpleBranch (l2, w2)) 
    -> if l1*w1 = l2*w2 then true else false
  |(SimpleBranch (l1, w1), CompoundBranch (l2, mb2)) 
    -> if l1*w1 = l2*(fw mb2) then true else false
  |(CompoundBranch (l1, mb1), SimpleBranch (l2, w2)) 
    -> if l1*(fw mb1)=  l2*w2 then true else false
  |(CompoundBranch (l1, mb1), CompoundBranch (l2, mb2)) 
    -> if l1*(fw mb1) = l2*(fw mb2) then true else false
