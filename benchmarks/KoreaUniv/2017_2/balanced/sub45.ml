(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int;;

let balanced : mobile -> bool
= fun m -> let rec cal_weight b =
	match b with
	| SimpleBranch(l, w) when ((l > 0) && (w > 0)) -> w
	| CompoundBranch(l, (leftb, rightb)) when l > 0 -> ((cal_weight leftb) + (cal_weight rightb)) 
	| _ -> raise (Failure "NotPositiveError: length and weight should be positive") in

	let rec cal_balanced m = match m with 
	| (SimpleBranch(leftx, leftw), SimpleBranch(rightx, rightw)) -> ((leftx * leftw) = (rightx * rightw))
	| (((CompoundBranch(leftx, leftm)) as left), right) -> 
		if (cal_balanced leftm) = true 
			then cal_balanced (SimpleBranch(leftx, (cal_weight left)), right)
		else false
	| (left, ((CompoundBranch(rightx, rightm)) as right)) -> 
		if (cal_balanced rightm) = true 
			then cal_balanced (left, SimpleBranch(rightx, (cal_weight right)))
		else false	 
	
	in cal_balanced m;;