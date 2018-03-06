(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec mobile_weight : mobile -> int
= fun m ->
  match m with
  | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
  | (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> let w_m2 = mobile_weight m2 in
                                                        w1 + w_m2
  | (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> let w_m1 = mobile_weight m1 in
                                                        w_m1 + w2
  | (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> let w_m1 = mobile_weight m1 in
                                                          let w_m2 = mobile_weight m2 in
                                                          w_m1 + w_m2


let rec balance_helper : mobile -> bool
= fun m ->
  match m with
  | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> if (l1 * w1) = (l2 * w2) then true else false
  | (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> let m2_balance = balance_helper m2 in
                                                        let w_m2 = mobile_weight m2 in
                                                        if m2_balance = false then false
                                                        else balance_helper ((SimpleBranch (l1, w1), SimpleBranch (l2, w_m2)))
  | (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> let m1_balance = balance_helper m1 in
                                                        let w_m1 = mobile_weight m1 in
                                                        if m1_balance = false then false
                                                        else balance_helper ((SimpleBranch (l1, w_m1), SimpleBranch (l2, w2)))
  | (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> let m1_balance = balance_helper m1 in
                                                          let m2_balance = balance_helper m2 in
                                                          let w_m1 = mobile_weight m1 in
                                                          let w_m2 = mobile_weight m2 in
                                                          if (m1_balance && m2_balance) = true then balance_helper ((SimpleBranch (l1, w_m1), SimpleBranch (l2, w_m2)))
                                                          else false

let balanced = balance_helper
