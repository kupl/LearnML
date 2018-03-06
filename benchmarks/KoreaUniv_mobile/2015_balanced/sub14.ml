
type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec weight: mobile -> int
= fun (lb, rb) ->
match lb with
SimpleBranch (l, w) -> (match rb with
	SimpleBranch (l1, w1) -> (w + w1)
	|CompoundBranch (l1, m) -> (w + (weight m)))
|CompoundBranch (l, m) -> (match rb with
	SimpleBranch (l1, w1) -> ((weight m) + w1)
	|CompoundBranch (l1, m1) -> ((weight m) + (weight m1)))

let rec balanced : mobile -> bool
=fun (lb,rb) ->
match lb with
SimpleBranch (l, w) -> (match rb with
	SimpleBranch (l1, w1) -> if (l * w)=(l1 * w1) then true else false
	|CompoundBranch (l, m) -> if (balanced m)=false then false
		else (match rb with
		SimpleBranch (l2, w2) -> if (l2 * w2)=(l * (weight m)) then true else false
		|CompoundBranch(l3, m1) -> if (balanced m1)=false then false
		else if (l * (weight m)) = (l3 * (weight m1)) then true else false
	)
)
|CompoundBranch (l, m) -> if (balanced m)=false then false
else (match rb with
	SimpleBranch (l1, w1) -> if (l * (weight m))=(l1 * w1) then true else false
	|CompoundBranch(l1, m1) -> if (balanced m1)=false then false
		else if (l * (weight m))=(l1 * (weight m1)) then true else false)
