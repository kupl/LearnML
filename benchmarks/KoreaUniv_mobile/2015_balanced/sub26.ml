type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec sumweight : branch -> int
 =fun b ->
 match b with
 SimpleBranch (l,w) -> w
 |CompoundBranch (l,m) -> 
 ( match m with
   (lb,rb) -> sumweight(lb)+sumweight(rb)
 )

let rec helper : branch -> int
 =fun b ->
 match b with 
 SimpleBranch (l,w) -> l*w
 | CompoundBranch (l,m)-> 
 ( match m with 
   |(lb,rb) -> 
   if(helper lb=(-1000001)) then -1000001
   else if(helper rb=(-1000001)) then -1000001
   else if(helper lb!= helper rb) then -1000001
   else l*(sumweight lb +sumweight rb)
 )

let balanced : mobile -> bool
 =fun (lb,rb) -> (* TODO *)
 if(helper lb=(-1000001)) then false
 else if(helper rb=(-1000001)) then false
 else if(helper lb=helper rb) then true else false
