(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> let rec f m = let rec weightf = fun (branchl, branchr) -> let rec weightget x = match x with 
                                                             |SimpleBranch(l, w) -> w
                                                             |CompoundBranch(l, m) -> weightf m in
                                                   weightget branchl + weightget branchr in 
           match m with 
           |(SimpleBranch(ll, wl), SimpleBranch(lr, wr)) -> if ll * wl == lr * wr then true else false
           |(CompoundBranch(ll, ml), CompoundBranch(lr, mr)) -> if f ml == true && f mr == true && ll * weightf ml == lr * weightf mr then true else false
           |(SimpleBranch(ll, wl), CompoundBranch(lr, mr)) -> if f mr == true && ll * wl == lr * weightf mr then true else false
           |(CompoundBranch(ll, ml), SimpleBranch(lr, wr)) -> if f ml == true && ll * weightf ml == lr * wr then true else false in
           f m 