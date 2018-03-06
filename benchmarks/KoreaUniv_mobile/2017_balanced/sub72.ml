
    (* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m -> 
  let rec totalweight : mobile -> int
  = fun m ->
  match m with 
  | (SimpleBranch (a1, b1), SimpleBranch (a2, b2)) -> b1+b2
  | (CompoundBranch (a1, b1), SimpleBranch (a2, b2)) -> (totalweight b1)+ b2
  | (SimpleBranch (a1, b1), CompoundBranch(a2, b2)) -> b1+(totalweight b2)
  | (CompoundBranch (a1, b1), CompoundBranch(a2,b2)) -> (totalweight b1) + (totalweight b2)
  in let rec balance : mobile->bool->bool
  = fun m b -> 
  if b=false then false else 
    match m with
    | (SimpleBranch (a1, b1), SimpleBranch (a2, b2)) -> if (a1*b1) = (a2*b2) then true else false
    | (CompoundBranch (a1, b1), SimpleBranch (a2, b2)) -> if (balance b1 true) = false then false 
                                                          else if ((totalweight b1)*a1) = (a2*b2) then true
                                                          else false 
    | (SimpleBranch (a1, b1), CompoundBranch(a2, b2)) -> if (balance b2 true) = false then false 
                                                          else if ((totalweight b2)*a2) = (a1*b1) then true
                                                          else false 
    | (CompoundBranch (a1, b1), CompoundBranch(a2,b2)) -> if (balance b2 true)&&(balance b1 true) = false then false 
                                                          else if ((totalweight b2)*a2) = ((totalweight b1)*a1) then true
                                                          else false 
  in balance m true
