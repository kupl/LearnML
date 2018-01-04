type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) = match (n1, n2) with
						  | (ZERO, ZERO) -> ZERO
						  | (ZERO, SUCC m) -> SUCC (natadd (ZERO, m))
						  | (SUCC m, ZERO) -> SUCC (natadd (m, ZERO))
						  | (SUCC m1, SUCC m2) -> SUCC (SUCC (natadd (m1, m2)))

let rec natmul (n1, n2) = match (n1, n2) with
						  | (ZERO, _ ) -> ZERO
						  | ( _ , ZERO) -> ZERO
						  | (SUCC m1, SUCC m2) -> (match (m1, m2) with
										  		  | (ZERO, ZERO) -> SUCC ZERO
												  | (ZERO, _ ) -> n2
												  | ( _ , ZERO) -> n1
												  | _ -> natadd (n1, (natmul (n1, m2))))

