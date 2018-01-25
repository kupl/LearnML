  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec weight: int -> bool = fun(a) -> match a with
    | 1 -> true
    | _ -> false

let rec total_weight : mobile -> int =fun (lb,rb) -> match lb, rb with 
		| SimpleBranch(a1, b1), SimpleBranch(a2, b2) -> b1 + b2
		| SimpleBranch(a1, b1), CompoundBranch(c1, (d1, e1)) -> b1 + total_weight(d1, e1)
		| CompoundBranch(a1, (b1, c1)), SimpleBranch(d1, e1) -> total_weight(b1, c1) + e1
		| CompoundBranch(a1, (b1, c1)), CompoundBranch(d1, (e1, f1)) -> total_weight(b1, c1) + total_weight(e1, f1)

let rec balanced: mobile -> bool =fun(lb,rb) -> match lb, rb with
		| SimpleBranch(a1, b1), SimpleBranch(a2, b2) -> if(a1*b1 = a2*b2) then true else false
    | SimpleBranch(a1, b1), CompoundBranch(c1, (d1, e1)) -> let x = a1*b1 in
																														let y = c1*total_weight(d1, e1) in
																														if(balanced(d1, e1)) then 
																														(if(x=y) then true else false) else false
    | CompoundBranch(a1, (b1, c1)), SimpleBranch(d1, e1) -> let x = a1*total_weight(b1, c1) in 
																														let y = d1*e1 in
																														if(balanced(b1, c1)) then 
																														(if(x = y) then true else false) else false
    | CompoundBranch(a1, (b1, c1)), CompoundBranch(d1, (e1, f1)) -> let x = a1*total_weight(b1, c1) in 
																																		let y = d1*total_weight(e1, f1) in
																																		if(balanced(b1, c1) && balanced(e1, f1)) then 
																																		(if(x=y) then true else false) else false
