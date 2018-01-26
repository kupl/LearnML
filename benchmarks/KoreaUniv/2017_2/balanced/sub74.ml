(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let left : mobile -> branch
= fun m -> match m with (x,_) -> x;;

let rigth : mobile -> branch
= fun m -> match m with (_,x) -> x;;


let rec calc_weight : branch -> int
= fun b -> (match b with
           | CompoundBranch (length, mobile) -> ((calc_weight (left mobile))+(calc_weight (rigth mobile)))
           | SimpleBranch (length, weight) -> weight
           )


let calc_torgue : branch -> int
= fun b -> match b with
           | SimpleBranch (length, weight) -> (length*weight)
           | CompoundBranch (length, mobile) -> (length*((calc_weight (left mobile))+(calc_weight (rigth mobile))))


let rec balanced : mobile -> bool
= fun m -> (match m with
           | (x,y) -> let v1 = (calc_torgue x) in 
                      let v2 = (calc_torgue y) in
                      (if (v1!=v2) then false else 
                        (match x with 
                         | CompoundBranch (x_length, x_mobile) -> ((balanced x_mobile)&&(match y with 
                                 | CompoundBranch (y_length, y_mobile) -> (balanced y_mobile)
                                 | _ -> true))
                         | _ -> (match y with 
                                 | CompoundBranch (y_length, y_mobile) -> (balanced y_mobile)
                                 | _ -> true)
                        )
                      )
                      
           )
