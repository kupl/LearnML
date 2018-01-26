
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec cal_weight : mobile -> weight
	= fun mobile ->
		match mobile with 
			|(SimpleBranch(length1, weight1), SimpleBranch(length2, weight2))
				-> weight1 + weight2
			|(SimpleBranch(length1, weight), CompoundBranch(length2, mobile))
				-> weight + cal_weight mobile
			|(CompoundBranch(length1, mobile), SimpleBranch(length2, weight))
				-> cal_weight mobile + weight
			|(CompoundBranch(length1, mobile1), CompoundBranch(length2, mobile2))
				-> cal_weight mobile1 + cal_weight mobile2;;

  let balanced : mobile -> bool
  = fun mob -> 
		match mob with
			|(SimpleBranch(length1, weight1), SimpleBranch(length2, weight2))
				-> if length1 * weight1 = length2 * weight2 then true else false
			|(SimpleBranch(length1, weight), CompoundBranch(length2, mobile))
				-> if length1 * weight = length2 * cal_weight mobile then true else false
			|(CompoundBranch(length1, mobile), SimpleBranch(length2, weight))
				-> if length1 * cal_weight mobile = length2 * weight then true else false
			|(CompoundBranch(length1, mobile1), CompoundBranch(length2, mobile2))
				-> if length1 * cal_weight mobile1 = length2 * cal_weight mobile2 then true else false;;			
