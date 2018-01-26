(* problem 6*)
type mobile = branch * branch (*left and right branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec weight : branch -> int
= fun m -> match m with 
            | SimpleBranch(x,y) -> y
            | CompoundBranch(x,y) -> match y with
                                      | (n1, n2) -> (weight n1) + (weight n2)               

let help_bal : branch -> int
= fun m -> match m with 
            | CompoundBranch(x,y) -> (match y with
                                      | (n1, n2) -> x * ((weight n1) + (weight n2)))
            | SimpleBranch(x,y) -> x * y

let rec balanced : mobile -> bool
= fun m -> (match m with
            | (x) -> (match x with
                      | (y,z) -> (match y,z with 
                                  | CompoundBranch(n1,n2), CompoundBranch(n3,n4) ->
                                     if ((help_bal y = help_bal z) && balanced n2 && balanced n4)
                                         then true
                                     else false
                                  | CompoundBranch(a1,a2), SimpleBranch(a3,a4) ->
                                     if ((help_bal y = help_bal z) && balanced a2) then true
                                     else false
                                  | SimpleBranch(a5,a6), CompoundBranch(a7,a8) ->
                                     if ((help_bal y = help_bal z) && balanced a8) then true
                                     else false
                                  | SimpleBranch(n5,n6), SimpleBranch(n7,n8) ->
                                     if (help_bal y = help_bal z) then true
                                     else false)))
