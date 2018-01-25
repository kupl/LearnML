type mobile 
= branch * branch 
and branch = SimpleBranch of length * weight 
					| CompoundBranch of length * mobile 
and length = int and weight = int;; 
type env = mobile -> value;;
type value = Int of int | Bool of bool;;

let balanced : mobile -> bool =fun (lb,rb) -> false 
