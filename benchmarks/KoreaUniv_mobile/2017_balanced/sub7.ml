(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec getWeight : mobile -> int
= fun m -> match m with
		| (l, r) -> (match l with
				| SimpleBranch(l, w) -> let left=w in let lw=l*left in
					(match r with
						| SimpleBranch(l, w) -> let rw = l*w in
							if lw=rw then left+w else -1
						| CompoundBranch(l, m) -> let right=(getWeight m) in let rw = l*right in if rw<0 then -1 else if lw=rw then left+right else -1)
				| CompoundBranch(l, m) -> let left=(getWeight m) in let lw = l*left in if lw<0 then -1 else
					(match r with
						| SimpleBranch(l, w) -> let rw = l*w in
							if lw=rw then left+w else -1
						| CompoundBranch(l, m) -> let right=(getWeight m) in let rw = l*right in if rw<0 then -1 else if lw=rw then left+right else -1))

let balanced : mobile -> bool
= fun m -> if (getWeight m)>0 then true else false
