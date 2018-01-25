  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec balanced : mobile -> bool
=fun (lb,rb) ->
match lb with
| SimpleBranch(l,w) -> (
			let a = l in
			let b= w in
			(
			match rb with
			| SimpleBranch(l,w) -> (let c = l in
						let d = w in
						if ((a*b) = (c*d)) then true else false
						)
			| CompoundBranch(l,m) -> (let c = l in
						let d = (eval m) in
							(
							match d with
							| (k, n) -> if k then (if ((a*b) = (c*n)) then true else false) else false
							)
								
						)
			)
			)
| CompoundBranch(l,m) ->(
			let a = l in
			let b = (eval m) in
			(match b with
			|(x,y) -> if x then 
				(
				match rb with
				| SimpleBranch(l,w) -> (let c = l in
							let d = w in
							if ((a*y) = (c*d)) then true else false
							)
				| CompoundBranch(l,m) -> (let c = l in
							let d = (eval m) in
								(
								match d with
								| (k, n) -> if k then (if ((a*y) = (c*n)) then true else false) else false
								)
								
							)
				)
				else false
			)
			)

and eval: mobile -> (bool * int)
= fun (lb, rb) ->
match lb with
| SimpleBranch(l,w) -> (
			let a =l in
			let b= w in
			(
			match rb with
			| SimpleBranch(l,w) -> (let c = l in
						let d = w in
						if ((a*b) = (c*d)) then (true, (b+d)) else (false, 0)
						)
			| CompoundBranch(l,m) -> (let c = l in
						let d = (eval m) in
							(
							match d with
							| (k, n) -> if k then (if ((a*b) = (c*n)) then (true, b+n) else (false,0)) 
									else (false,0)
							)
								
						)
			)
			)
| CompoundBranch(l,m) ->(
			let a = l in
			(let b = (eval m) in
				(match b with
				| (x,y) -> if x then (match rb with
								| SimpleBranch(l,w) -> (let c = l in
											let d = w in
											if ((a*y) = (c*d)) then (true, (y+d)) else (false, 0)
											)
								| CompoundBranch(l,m) -> (let c = l in
											let d = (eval m) in
												(
												match d with
												| (k, n) -> if k then (if ((a*y) = (c*n)) then (true, (y+n)) else (false,0)) 
														else (false,0)
												)
								
											)
								)
					 else (false,0))
				)
			
				)

