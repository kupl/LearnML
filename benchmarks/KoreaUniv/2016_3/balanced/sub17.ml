
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec tweight brch = 
 match brch with
 | SimpleBranch(l,w) -> w
 | CompoundBranch(l,(b1, b2)) -> (tweight b1) + (tweight b2)

let rec tqcal brch = 
 match brch with
 | SimpleBranch(l,w) -> l*w
 | CompoundBranch(l,(b1, b2)) -> l*((tweight b1) + (tweight b2));;

let balanced mob = 
 match mob with
 | (b1,b2) -> 
   if tqcal(b1)==tqcal(b2) then true
   else false;;