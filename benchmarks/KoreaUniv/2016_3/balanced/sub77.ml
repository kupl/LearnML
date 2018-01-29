
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec weightB =
    fun exp->
     match exp with
     |(SimpleBranch (a,b), SimpleBranch (c,d))-> b+d 
     |(SimpleBranch (a,b), CompoundBranch (c,d))-> b+(weightB (d))
     |(CompoundBranch (a,b),  SimpleBranch (c,d))-> (weightB (b))+d 
     |(CompoundBranch (a,b), CompoundBranch (c,d))-> (weightB (b)) + (weightB (d))

  let balanced : mobile -> bool
  = fun mob->  (* TODO *)
    match mob with
    |(SimpleBranch(a,b),SimpleBranch(c,d))-> if (a*b) = (c*d) then true else false 
    |(SimpleBranch(a,b),CompoundBranch(c,d))-> if(a*b) =(c*(weightB (d))) then true else false
    |(CompoundBranch(a,b), SimpleBranch (c,d))-> if((a*(weightB (b))) = (c*d)) then true else false
    |(CompoundBranch(a,b), CompoundBranch(c,d))-> if((a*(weightB (b))) = (c*(weightB (d)))) then true else false
