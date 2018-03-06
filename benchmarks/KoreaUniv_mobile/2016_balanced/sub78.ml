
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec getWeight
	= fun bran -> match bran with
	SimpleBranch(length, weight) -> weight
|	CompoundBranch(length, mob) -> match mob with
	(left, right) -> getWeight(left) + getWeight(right)

let getLength
	= fun bran -> match bran with
	SimpleBranch(length, weight) -> length
|	CompoundBranch(length, mob) -> length

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
		(left, right) ->
		if(getWeight(left) * getLength(left) == getWeight(right) * getLength(right))
		 then match (left, right) with

			(SimpleBranch(len1, wei1), SimpleBranch(len2, wei2)) ->
			true
		| (SimpleBranch(len1, wei1), CompoundBranch(len2, mob2)) ->
			if((balanced mob2) == true) then true else false
		| (CompoundBranch(len1, mob1), SimpleBranch(len2, wei2)) ->
			if((balanced mob1) == true) then true else false
		| (CompoundBranch(len1, mob1), CompoundBranch(len2, mob2)) ->
			if((balanced mob1) == true && (balanced mob2) == true) 
			then true else false
	 else false

		