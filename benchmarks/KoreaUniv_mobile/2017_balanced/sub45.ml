(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> let rec cal_weight b =
	match b with
	| SimpleBranch(l, w) -> if ((l > 0) && (w > 0)) then w else raise (Failure "myfail")
	| CompoundBranch(l, (leftb, rightb)) -> if l > 0 then ((cal_weight leftb) + (cal_weight rightb)) else raise (Failure "myfail")
	| _ -> raise (Failure "NotPositiveError: length and weight should be positive") in

	let rec cal_balanced m = match m with 
	| (SimpleBranch(leftx, leftw), SimpleBranch(rightx, rightw)) -> ((leftx * leftw) = (rightx * rightw))
	| ((CompoundBranch(leftx, leftm)), right) -> 
		if (cal_balanced leftm) = true 
			then cal_balanced (SimpleBranch(leftx, (cal_weight (CompoundBranch(leftx,leftm)))), right)
		else false
	| (left, ((CompoundBranch(rightx, rightm)))) -> 
		if (cal_balanced rightm) = true 
			then cal_balanced (left, SimpleBranch(rightx, (cal_weight (CompoundBranch(rightx,rightm)))))
		else false	 
	
	in cal_balanced m;;
