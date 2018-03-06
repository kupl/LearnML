type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec getWeight : mobile -> int
  = fun(lb, rb) ->
    (match lb with
        SimpleBranch(l, w) -> w
      | CompoundBranch(l, m) -> getWeight(m))
    +(match rb with
         SimpleBranch(l, w) -> w
       | CompoundBranch(l, m) -> getWeight(m))
;;
let rec balanced : mobile -> bool
  = fun(lb,rb) -> 
    match lb with
        SimpleBranch(l, w) ->
          (match rb with 
              SimpleBranch(l2, w2) ->
                (if(l*w = l2*w2) then true else false)
            | CompoundBranch(l2, m2) -> 
                if(balanced m2)
                then(if(l*w = l2*getWeight(m2)) then true else false)
                else false
          )
      | CompoundBranch(l, m) -> 
          (if(balanced m)
           then (match rb with 
                    SimpleBranch(l2, w2) ->
                      (if(l*getWeight(m) = l2*w2) then true else false)
                  | CompoundBranch(l2, m2) -> 
                      if(balanced m2)
                      then(if(l*getWeight(m) = l2*getWeight(m2)) then true else false)
                      else false
                )
           else false)
;;

