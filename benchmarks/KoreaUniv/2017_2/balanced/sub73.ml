(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m ->
 let rec wb : mobile -> int
  = fun n ->
    match n with
    |(SimpleBranch(_,a_w), SimpleBranch(_,b_w)) -> a_w + b_w
    |(SimpleBranch(_,a_w), CompoundBranch(_,cm)) -> a_w + (wb cm)
    |(CompoundBranch(_,cm), SimpleBranch(_,b_w)) -> (wb cm) + b_w
    |(CompoundBranch(_,l_m), CompoundBranch(_,r_m)) -> (wb l_m) + (wb r_m)

in match m with
|(SimpleBranch(a_l,a_w), SimpleBranch(b_l,b_w)) ->
   if a_l * a_w = b_l * b_w then true else false
|(SimpleBranch(a_l,a_w), CompoundBranch(b_l,cm)) ->
   if a_l * a_w = b_l * (wb cm) then (balanced cm) else false
|(CompoundBranch(a_l,cm), SimpleBranch(b_l,b_w)) ->
   if a_l * (wb cm) = b_l * b_w then (balanced cm) else false
|(CompoundBranch(a_l,l_m), CompoundBranch(b_l,r_m)) ->
   if a_l * (wb l_m) = b_l * (wb r_m) then (balanced l_m) && (balanced r_m) else false
;;
