(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec findint:mobile->int
   =fun m->
   match m with
 |(SimpleBranch (x,y), SimpleBranch (z,r))-> y+r
 |(SimpleBranch(x,y),CompoundBranch(z,r))->y+findint r
 |(CompoundBranch(x,y),CompoundBranch(z,r))-> findint y+findint r
 |(CompoundBranch(x,y),SimpleBranch(z,r))->findint y+r;;
        
 let rec balanced : mobile -> bool
  = fun m -> (* TODO *)
   match m with
 |(SimpleBranch(x,y),SimpleBranch(z,r))->
          if y*x=r*z then true else false
 |(SimpleBranch(x,y),CompoundBranch(z,r))->
          if y=z*findint r && balanced r then true else false
 |(CompoundBranch(x,y),CompoundBranch(z,r))->
          if x*findint y=z*findint r && balanced y &&balanced r then true else false
 |(CompoundBranch(x,y),SimpleBranch(z,r))->
          if x*findint y=z*r &&balanced y then true else false;;