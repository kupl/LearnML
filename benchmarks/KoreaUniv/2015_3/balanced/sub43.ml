  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
	let rec calweight : branch -> int
	= fun br -> match br with
						| SimpleBranch (x,y) -> y
						| CompoundBranch (x,y) -> match y with
																		| (a,b) -> calweight a + calweight b
	let callength : branch -> int
	= fun br -> match br with
						| SimpleBranch (x,y) -> x
						| CompoundBranch (x,y) -> x

  let rec balanced : mobile -> bool
  =fun (lb,rb) -> if callength lb * calweight lb = callength rb * calweight rb then (match lb with
				| CompoundBranch (x,y)-> (match rb with
										| CompoundBranch (z,w) -> balanced y && balanced w
										| _ -> balanced y)
				| _ -> (match rb with
								| CompoundBranch (z,w) -> balanced w
								| _ -> true))
									else false
