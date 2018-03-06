
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

	let fst p = match p with (x,_) -> x;;
	let snd p = match p with (_,x) -> x;;

	
let rec exp : mobile -> int
	= fun mob ->
	match fst mob with
	SimpleBranch (l, w) ->
		( let left =   w in
	  match snd mob with
			SimpleBranch (l, w) -> left + w
			|CompoundBranch (l, m) -> left + exp m
		)
  | CompoundBranch (l, m) ->
		( let left = exp m
		in match snd mob with
			SimpleBranch (l, w) -> left + w
			|CompoundBranch (l, m) -> left + exp m );;
	 

  let rec balanced : mobile -> bool
  = fun mob ->
	match fst mob with
	SimpleBranch(l, w) -> 
		(let left = l*w in
			match snd mob with
			SimpleBranch(l, w) -> if left = l*w then true else false
			|CompoundBranch(l, m) ->
												if balanced m = true then 
														if left = l*exp m then true else false 
												else false)
	|CompoundBranch(l, m) ->
		(if balanced m = true then
				(let left = l*exp m in
				match snd mob with
				SimpleBranch(l, w) -> if left = l*w then true else false
				|CompoundBranch(l, m) ->
												if balanced m = true then
													if left = l*exp m then true else false
												else false )
		else false);; 
