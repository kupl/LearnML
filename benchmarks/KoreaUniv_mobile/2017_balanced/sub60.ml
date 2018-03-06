(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec sumOfWeight : branch -> int
= fun bra ->
  match bra with
    | SimpleBranch (len, wei) -> wei
    | CompoundBranch (len, mob) -> 
      match mob with 
        | (branch1, branch2) -> (sumOfWeight branch1 + sumOfWeight branch2)
  
let rec calAll : branch -> int
= fun bra ->
  match bra with
    | SimpleBranch (len, wei) -> (len * wei)
    | CompoundBranch (len, mob) -> 
      match mob with
        | (branch1, branch2) -> (len * (sumOfWeight branch1 + sumOfWeight branch2))

let rec balanced : mobile -> bool
= fun m -> (* TODO *)
  match m with
    | (branch1, branch2) ->
      match branch1 with
        | SimpleBranch (len1, wei1) ->
          (match branch2 with
            | SimpleBranch (len2, wei2) -> 
              if (calAll branch1 = calAll branch2) then true
              else false
            | CompoundBranch (len2, mob) ->
              if (balanced mob) then
                (if (calAll branch1 = calAll branch2) then true
                else false)
              else false)
        | CompoundBranch (len1, mob1) ->
          if (balanced mob1) then
            (match branch2 with
              | SimpleBranch (len2, wei2) ->
                if (calAll branch1 = calAll branch2) then true
                else false
              | CompoundBranch (len2, mob2) ->
                if (balanced mob2) then 
                  (if (calAll branch1 = calAll branch2) then true
                  else false)
                else false)
          else false