
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec balanced : mobile -> bool
  = fun mob -> (* TODO *)
  
  let rec cal = fun mob ->
  match mob with
  |(left,right) ->
    (
    match left with
    |SimpleBranch (ll,lw) ->		
      (
      match right with
      |SimpleBranch (rl,rw) -> (lw+rw) 
      |CompoundBranch (rcl,rcm) -> (lw  + (cal rcm))
      )
    |CompoundBranch (lcl,lcm) ->
      (
      match right with
      |SimpleBranch (rl,rw) -> ((cal lcm) + rw)
      |CompoundBranch (rcl,rcm) -> ((cal lcm) + (cal rcm))
      )
    )
  in
  match mob with
  |(left,right)->
  (
    match left with
    |SimpleBranch (ll,lw) ->
    (
      match right with
      |SimpleBranch(rl,rw)->
        if rl*rw= ll*lw then true else false
      |CompoundBranch(rcl,rcm)->
        if balanced(rcm) then
          (if rcl*cal(rcm)= ll*lw then true else false)
        else false
    )
    |CompoundBranch(lcl,lcm)->
    if balanced(lcm) then
      (
      match right with
      |SimpleBranch(rl,rw)->
        if ( lcl*cal(lcm) = rl*rw ) then true else false
      |CompoundBranch(rcl,rcm)->
        if ( lcl*cal(lcm) = rcl*cal(rcm) ) then true else false
      )
    else false
  )