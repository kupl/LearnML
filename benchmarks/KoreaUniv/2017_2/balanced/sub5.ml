(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> let rec funl k = match k with (x,_) -> x in let rec funr k = match k with (_,x) -> x in 
			let sum1 = 0 in let sum2 = 0 in
			let rec funa k = match k with | (SimpleBranch(a,b), SimpleBranch(c,d)) -> let sum1 = sum1 + b + d in sum1
						      | (SimpleBranch(a,b), CompoundBranch(c,d)) -> let sum1 = sum1 + b + (funa d) in sum1
						      | (CompoundBranch(a,b), SimpleBranch(c,d)) -> let sum1 = sum1 + (funa b) + d in sum1
                                                      | (CompoundBranch(a,b), CompoundBranch(c,d)) -> let sum1 = sum1 + (funa b) + (funa d) in sum1 in
			let rec funb k = match k with | (SimpleBranch(a,b), SimpleBranch(c,d)) -> let sum2 = sum2 + b + d in sum2
						      | (SimpleBranch(a,b), CompoundBranch(c,d)) -> let sum2 = sum2 + b + (funb d) in sum2
						      | (CompoundBranch(a,b), SimpleBranch(c,d)) -> let sum2 = sum2 + (funb b) + d in sum2
                                                      | (CompoundBranch(a,b), CompoundBranch(c,d)) -> let sum2 = sum2 + (funb b) + (funb d) in sum2 in
				let func k = match k with | SimpleBranch(a,b) -> let sum1 = sum1 + b in sum1
							  | CompoundBranch(a,b) -> let sum1 = sum1 + (funa b) in sum1 in
				let fund k = match k with | SimpleBranch(a,b) -> let sum2 = sum2 + b in sum2
							  | CompoundBranch(a,b) -> let sum2 = sum2 + (funb b) in sum2 in
					let fune k = match k with | SimpleBranch(a,b) -> a
							          | CompoundBranch(a,b) -> a in 
					if ((func (funl m)) * (fune (funl m))) = ((fund (funr m)) * (fune (funr m))) then true else false;;

