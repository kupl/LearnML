
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec calc_weight : mobile -> int
  = fun mob -> match mob with
	(SimpleBranch(l11,w11),SimpleBranch(l12,w12)) -> w11 + w12
	|(SimpleBranch(l21,w21),CompoundBranch(l22,m22)) -> w21 + (calc_weight m22)
	|(CompoundBranch(l31,m31),SimpleBranch(l32,w32)) -> (calc_weight m31) + w32
	|(CompoundBranch(l41,m41),CompoundBranch(l42,m42)) -> (calc_weight m41) + (calc_weight m42)

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
	(SimpleBranch(l11,w11),SimpleBranch(l12,w12)) -> if((l11*w11) = (l12*w12)) then true else false
	|(SimpleBranch(l21,w21),CompoundBranch(l22,m22)) -> if((balanced m22) && ((l21*w21) = (l22*(calc_weight m22)))) then true else false
	|(CompoundBranch(l31,m31),SimpleBranch(l32,w32)) -> if((balanced m31) && ((l32*w32) = (l31*(calc_weight m31)))) then true else false
	|(CompoundBranch(l41,m41),CompoundBranch(l42,m42)) -> if((balanced m41) && (balanced m42) && ((l41*(calc_weight m41)) = (l42*(calc_weight m42)))) then true else false (* TODO *)
