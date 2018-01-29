(*problem6*)

type mobile = branch * branch (*left and right branches *)
and branch = SimpleBranch of length * weight
			| CompuondBranch of length * mobile
and length = int
and weight = int
let rec weight_mb : mobile -> int
= fun m ->
match m with 
	|SimpleBranch(ll,lw), SimpleBranch(rl,rw) -> lw+rw 
	|SimpleBranch(ll,lw), CompuondBranch(rl,cm) -> lw+weight_mb cm 
	|CompuondBranch(ll,cm), SimpleBranch(rl,rw) -> rw+weight_mb cm
	|CompuondBranch(ll,lm),CompuondBranch(rl,rm) ->weight_mb lm+weight_mb rm



let rec balanced : mobile -> bool
= fun m -> 
match m with 
	|SimpleBranch (ll, lw) , SimpleBranch(rl, rw) -> (ll*lw=rl*rw)
	|SimpleBranch (ll, lw) , CompuondBranch (rl, cm) -> (ll*lw=rl*(weight_mb cm)&& balanced cm)
	|CompuondBranch(ll,cm) , SimpleBranch(rl,rw) -> (ll*(weight_mb cm)=rl*rw&&balanced cm)
	|CompuondBranch(ll,lcw), CompuondBranch(rl,rcw)->(ll*(weight_mb lcw)=rl*(weight_mb rcw)&&balanced lcw &&balanced rcw)
