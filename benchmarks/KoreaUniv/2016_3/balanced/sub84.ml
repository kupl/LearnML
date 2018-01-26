
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec balanced : mobile -> bool
  = fun (left, right) -> 
let rec cal_Weight : branch -> weight
= fun calweight -> match calweight with
  SimpleBranch (l,w) -> w
| CompoundBranch (l,m) -> match m with (left,right) -> (cal_Weight left)+(cal_Weight right)
in let cal_Torque : branch -> int
=fun caltorque -> match caltorque with 
  SimpleBranch (l,w) -> l*w
| CompoundBranch (l,m) -> match m with (left,right) -> l*(cal_Weight left) + l*(cal_Weight right)
in
 match left,right with
  SimpleBranch (l1,w1), SimpleBranch (l2,w2) -> if (cal_Torque left)=(cal_Torque right) then true else false
| SimpleBranch (l1,w1), CompoundBranch (l2,m2) -> if (balanced m2 = true) && (cal_Torque left = cal_Torque right) then true else false
| CompoundBranch (l1,m1), SimpleBranch (l2,w2) -> if (balanced m1 = true) && (cal_Torque left = cal_Torque right) then true else false
| CompoundBranch (l1,m1), CompoundBranch (l2,m2) -> if (balanced m1 = true) && (balanced m2 = true) && (cal_Torque left = cal_Torque right) then true else false;;
