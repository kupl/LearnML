  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec get_weight : branch -> weight
  =fun b -> 
	match b with
		  SimpleBranch (l,w) -> w
		| CompoundBranch (l,(b1,b2)) -> (get_weight b1) + (get_weight b2)

  let get_length : branch -> int
  =fun b -> match b with 
		  SimpleBranch (l,w) -> l
		| CompoundBranch (l,m) -> l

  let cal_torque : branch -> int
  =fun b -> (get_length b) * (get_weight b)
(*
  let rec check_balanced : branch -> bool
  =fun b -> 
	match b with
		  SimpleBranch (l,w) -> true
		| CompoundBranch (l, (b1,b2)) -> (check_balanced b1) && (check_balanced b2) 
	
  let balanced : mobile -> bool
  =fun (lb,rb) -> 
	let tlb = cal_torque lb in
	let trb = cal_torque rb in
		if tlb = trb then (check_balanced lb && check_balanced rb) else false
*)
  let check_torque : mobile -> bool
  =fun (lb,rb) -> cal_torque lb = cal_torque rb 

  let rec sub_balanced : branch -> bool
  =fun b -> 
	match b with
		| SimpleBranch (_,_) -> true
		| CompoundBranch (l,m) -> check_balanced m 	
	
  and check_balanced : mobile -> bool
  =fun (lb,rb) -> 
	let tq = check_torque (lb,rb) in
	let islb = sub_balanced lb in
	let isrb = sub_balanced rb in
		tq && islb && isrb

  let balanced : mobile -> bool
  =fun (lb,rb) -> check_balanced (lb,rb)
 
