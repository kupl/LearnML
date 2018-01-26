
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int
  

let rec evalWeight : branch -> weight
    = fun bran ->
     match bran with
    |  SimpleBranch (l,w) -> w
    | CompoundBranch (l,m) ->
        (match m with
          (lb,rb) -> (evalWeight lb) + (evalWeight rb));;
  
    let rec evalTorque : branch -> int
    = fun bran ->
      match bran with
     | SimpleBranch (l,w) -> l * w
    | CompoundBranch (l,m) ->
        (match m with
          (lb,rb) -> l * (evalWeight lb + evalWeight rb))

        let rec balanced : mobile -> bool
            = fun (leftB,rightB) ->
              match leftB,rightB with
            |  SimpleBranch (l1,w1),SimpleBranch (l2,w2) ->
                if evalTorque leftB = evalTorque rightB then true else false
            | SimpleBranch (l1,w),CompoundBranch (l2,m) ->
                if (balanced m = true) && (evalTorque leftB = evalTorque rightB)
                  then true else false
            | CompoundBranch (l1,m),SimpleBranch (l2,w) ->
                if (balanced m = true) && (evalTorque leftB = evalTorque rightB)
                  then true else false
            | CompoundBranch (l1,m1),CompoundBranch (l2,m2) ->
                if (balanced m1 = true) && (balanced m2 = true)
                && (evalTorque leftB = evalTorque rightB)
                  then true else false

            |_ ->raise NotImplemented 