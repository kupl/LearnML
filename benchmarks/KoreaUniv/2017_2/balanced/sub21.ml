(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m ->     
    let rec w_bal: branch -> int -> int
    = fun b weight ->
        match b with
        | SimpleBranch (l1, w1) -> weight+w1
        | CompoundBranch (l1, (b1, b2)) -> (w_bal b1 weight) + (w_bal b2 weight)
    in let l_bal: branch -> int
       = fun b ->
           match b with
           | SimpleBranch (l1, w1) -> l1
           | CompoundBranch (l1, m1) -> l1   
        in let rec sub_bal: mobile -> bool
        = fun sub_mob ->
            match sub_mob with
            | (sub_b1, sub_b2) ->
                    (match (sub_b1, sub_b2) with
                    | ((SimpleBranch (sub_l1, sub_w1)), (SimpleBranch (sub_l2, sub_w2))) -> 
                            if ((w_bal sub_b1 0) * (l_bal sub_b1)) = ((w_bal sub_b2 0) * (l_bal sub_b2)) then true
                            else false
                    | ((SimpleBranch (sub_l1, sub_w1)), (CompoundBranch (sub_l2, sub_m1))) ->
                            if ((w_bal sub_b1 0) * (l_bal sub_b1)) = ((w_bal sub_b2 0) * (l_bal sub_b2)) then (sub_bal sub_m1)
                            else false
                    | ((CompoundBranch (sub_l1, sub_m1)), SimpleBranch (sub_l2, sub_w1)) ->
                            if ((w_bal sub_b1 0) * (l_bal sub_b1)) = ((w_bal sub_b2 0) * (l_bal sub_b2)) then (sub_bal sub_m1)
                            else false
                    | (CompoundBranch (sub_l1, sub_m1), CompoundBranch (sub_l2, sub_m2)) ->
                            if ((w_bal sub_b1 0) * (l_bal sub_b1)) = ((w_bal sub_b2 0) * (l_bal sub_b2)) then (sub_bal sub_m1) && (sub_bal sub_m2)
                            else false)
            in sub_bal m;;
