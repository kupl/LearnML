(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec mobile_weight : mobile -> int = fun m ->
    match m with
    |b1,b2 -> (match b1 with
                |SimpleBranch(l1,w1) -> (match b2 with
                                        |SimpleBranch(l2,w2) -> w1 + w2
                                        |CompoundBranch(l2,m) -> w1 + mobile_weight m
                                        |_->0
                                        )
                |CompoundBranch(l1,m1)-> (match b2 with
                                        |SimpleBranch(12,w2) -> mobile_weight m1 + w2
                                        |CompoundBranch(12,m2) -> mobile_weight m1 + mobile_weight m2
                                        |_->0
                                        ))

let rec balanced : mobile -> bool
= fun m ->
    match m with
    |b1, b2 -> (match b1 with
            |SimpleBranch(l1,w1) -> (match b2 with |SimpleBranch(l2,w2) -> l1*w1 = l2*w2
                                                    |CompoundBranch(l2,m) -> (l1*w1 = (mobile_weight m)*l2) && balanced m
                                                    )
            |CompoundBranch(l1,m1) -> (match b2 with |SimpleBranch(l2, w2) -> (l1*(mobile_weight m1) = l2*w2) && balanced m1
                                                    |CompoundBranch(l2,m2) -> (l1*(mobile_weight m1) = l2*(mobile_weight m2) ) && balanced m2 && balanced m1
                                                    ))