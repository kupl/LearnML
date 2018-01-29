
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec brWeight
  = fun (branch) ->
      match branch with 
        SimpleBranch(len, wgh) -> wgh
        | CompoundBranch(len, mob) -> 
            match mob with (lb, rb) -> brWeight(lb) + brWeight(rb)
  let rec brTorq
  = fun (branch) ->
      match branch with 
        SimpleBranch(len, wgh) -> len*wgh
        | CompoundBranch(len, mob) -> 
            match mob with (lb, rb) -> len*(brWeight(lb) + brWeight(rb))

  let rec brBal
  = fun (branch) ->
      match branch with 
        SimpleBranch(len, wgh) -> true
        | CompoundBranch(len, mob) ->
            match mob with (lb, rb) -> 
              if(brBal lb && brBal rb && (brTorq lb = brTorq rb)) then true else false
  let rec balanced : mobile -> bool
  = fun mob ->
      match mob with
          (lb, rb) -> 
            if (brBal lb && brBal rb && (brTorq lb = brTorq rb) ) then true else false