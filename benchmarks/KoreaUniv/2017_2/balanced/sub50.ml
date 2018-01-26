(*problem 6*)
type mobile = branch * branch (*left and right and branches*)
and branch = SimpleBranch of length * weight | CompoundBranch of length * mobile
and length = int
and weight = int

let rec bweight
= fun b -> match b with
			|SimpleBranch(l,w) -> w
			|CompoundBranch(l, (b1,b2)) -> ((bweight b1) + (bweight b2))

let omega
= fun m -> match m with
			|SimpleBranch(l,w) -> l*w
			|CompoundBranch(l,(b1,b2)) -> l * ((bweight b1)+(bweight b2))

let rec balanced : mobile -> bool
= fun m -> match m with
			|(SimpleBranch (l1,w1), SimpleBranch(l2,w2))
				-> if l1 * w1 = l2 * w2 then true else false
			|(CompoundBranch(l1, (b1,b2)),SimpleBranch(l2,w1))
				-> let wl = omega (CompoundBranch(l1,(b1,b2))) in
					let wr = omega (SimpleBranch(l2,w1)) in
					if ((balanced (b1,b2) = true) && wl = wr)
					then true else false
			|(SimpleBranch(l1,w1),CompoundBranch (l2,(b1,b2)))
				-> let wl = omega (SimpleBranch(l1,w1)) in
					let wr = omega (CompoundBranch(l2,(b1,b2))) in
					if ((balanced (b1,b2) = true) && (wl = wr))
					then true else false
			|(CompoundBranch(l1,(b1,b2)),CompoundBranch(l2,(b3,b4)))
				-> let wl = omega (CompoundBranch(l1,(b1,b2))) in
					let wr = omega (CompoundBranch(l2,(b3,b4))) in
					if balanced (b1,b2) = true
					&& balanced (b3,b4) = true
					&& wl = wr
					then true else false
