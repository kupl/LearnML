
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec weightSum : mobile -> int
  = fun mob ->
     match mob with
     | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
     | (CompoundBranch (l1, sub_mob1), SimpleBranch (l2, w2)) -> weightSum(sub_mob1) + w2
     | (SimpleBranch (l1, w1), CompoundBranch (l2, sub_mob2)) -> w1 + weightSum(sub_mob2)
     | (CompoundBranch (l1, sub_mob1), CompoundBranch (l2, sub_mob2)) -> weightSum(sub_mob1) + weightSum(sub_mob2);;

  let rec balanced : mobile -> bool
  = fun mob ->
     match mob with
     | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> if l1 * w1 == l2 * w2 then true else false
     | (CompoundBranch (l1, sub_mob1), SimpleBranch (l2, w2)) -> if balanced sub_mob1 && l1 * weightSum(sub_mob1) == l2 *w2 then true else false
     | (SimpleBranch (l1, w1), CompoundBranch (l2, sub_mob2)) -> if balanced sub_mob2 && l1 * w1 == l2 * weightSum(sub_mob2) then true else false
     | (CompoundBranch (l1, sub_mob1), CompoundBranch (l2, sub_mob2)) -> if balanced sub_mob1 && balanced sub_mob2 && l1 * weightSum(sub_mob1) == l2 * weightSum(sub_mob2) then true else false;;
