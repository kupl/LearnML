
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec tortal_weight_br : branch -> int 
  = fun br ->
  match br with
  | SimpleBranch (_, w) ->	w
  | CompoundBranch (_, (lbr, rbr)) ->	(tortal_weight_br lbr) + (tortal_weight_br rbr)

  let rec tortal_weight : mobile -> int
  = fun mo ->
  match mo with
  | (lbr, rbr) ->	(tortal_weight_br lbr) + (tortal_weight_br rbr)


  let rec balanced : mobile -> bool
  = fun mob -> 
  match mob with
  | (SimpleBranch (ll, lw),	SimpleBranch (rl, rw)) ->
  	if (ll * lw) = (rl * rw) then true else false
  | (CompoundBranch (ll, lm),	SimpleBranch (rl, rw)) ->
  	if (ll * tortal_weight lm) = (rl * rw) then balanced lm else false
  | (SimpleBranch (ll, lw),	CompoundBranch (rl, rm)) ->
  	if (ll * lw) = (rl * tortal_weight rm) then balanced rm else false
  | (CompoundBranch (ll, lm),	CompoundBranch (rl, rm)) ->
  	if (ll * tortal_weight lm) = (rl * tortal_weight rm) then (balanced lm) && (balanced rm) else false
