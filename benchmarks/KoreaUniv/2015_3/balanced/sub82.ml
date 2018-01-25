  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
let rec bal : mobile -> int
    =fun (lb,rb) ->
    match (lb,rb) with 
    |(SimpleBranch (a,b),SimpleBranch (c,d)) -> b+d
    |(SimpleBranch (a,b),CompoundBranch (c,d)) -> (b+(bal d))
    |(CompoundBranch (a,b),CompoundBranch (c,d)) -> ((bal b)+(bal d))
    |(CompoundBranch (a,b),SimpleBranch (c,d)) -> ((bal b)+d)

let rec balanced : mobile -> bool
  =fun (lb,rb) -> 
    match (lb,rb) with 
    |(SimpleBranch (a,b), SimpleBranch (c,d)) -> if (a*b = c*d) then true else false
    |(SimpleBranch (a,b), CompoundBranch (c,d)) -> if ((((balanced d) == true)&&(a*b == c*(bal d)))) then true else false
    |(CompoundBranch (a,b), CompoundBranch (c,d)) -> if (((balanced b) == true)&&((balanced d) == true)&&(a*(bal b) == c*(bal d))) then true else false
    |(CompoundBranch (a,b), SimpleBranch (c,d)) -> if (((balanced b) == true)&&((a*(bal b)) == (c*d))) then true else false
