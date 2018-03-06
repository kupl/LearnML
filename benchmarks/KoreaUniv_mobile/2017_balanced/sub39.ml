(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int
 

let rec getw m =
	match m with (b1,b2) -> ((match b1 with 
									| SimpleBranch (l,w) -> w
									| CompoundBranch (l,sm) -> getw sm)
								+(match b2 with 
									| SimpleBranch (l,w) -> w
									| CompoundBranch (l,sm) -> getw sm))

let rec torq a = 
    match a with 
    | SimpleBranch (l,w) -> l*w
    | CompoundBranch (l,m) -> (match m with (lb, rb) -> l*(getw m))

let rec balanced : mobile -> bool
= fun m -> (* TODO *)
    match m with
    | (b1, b2) -> if torq b1 = torq b2 then 
    								((match b1 with 
									| SimpleBranch (l,w) -> true
									| CompoundBranch (l,sm) -> (balanced sm)) 
    								&&
    								(match b2 with 
									| SimpleBranch (l,w) -> true
									| CompoundBranch (l,sm) -> (balanced sm)))
                else false

