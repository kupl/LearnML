  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int

  
  let rec sumWeight
    =fun (lb, rb)->
      match lb, rb with
        |SimpleBranch (a,b), SimpleBranch (c,d) -> b + d
        |SimpleBranch (a,b), CompoundBranch (c,d) -> b + sumWeight d
        |CompoundBranch (a,b), SimpleBranch (c,d) -> sumWeight b + d
        |CompoundBranch (a,b), CompoundBranch (c,d) -> sumWeight b + sumWeight d

  
  
  let rec balanced  : mobile -> bool
    =fun (lb,rb) ->

      
      match lb, rb with
        |SimpleBranch (a,b), SimpleBranch (c,d)
          	-> if a * b = c * d then true else false
        |SimpleBranch (a,b), CompoundBranch (c,d)
          	-> if (a * b = c * sumWeight d) && (balanced d) then true else false
        |CompoundBranch (a,b), SimpleBranch (c,d)
          	-> if (a * sumWeight b = c * d) && (balanced b) then true else false
        |CompoundBranch (a,b), CompoundBranch (c,d)
          	-> if (a * sumWeight b = c * sumWeight d) && (balanced b) && (balanced d) then true else false
