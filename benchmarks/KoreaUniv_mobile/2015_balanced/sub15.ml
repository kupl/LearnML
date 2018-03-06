  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int


  let rec getWeight : branch -> weight
    =fun br ->
      match br with
          SimpleBranch(l, w) -> w
        | CompoundBranch(l, m) -> 
            match m with
                (br1, br2) -> getWeight(br1) + getWeight(br2)

  let rec calBranch : branch -> int
    =fun br ->
      match br with
          SimpleBranch (l, w) -> l * w
        | CompoundBranch (l, m) ->
            match m with
                (br1, br2) ->
                  if calBranch(br1) = calBranch(br2)
                  then l * (getWeight(br1) + getWeight(br2))
                  else -1

  let balanced : mobile -> bool
    =fun (lb,rb) ->
      if calBranch(lb) < 0 || calBranch(rb) < 0 || calBranch(lb) != calBranch(rb)
      then false
      else true
