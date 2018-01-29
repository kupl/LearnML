(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec calWeight : branch -> int = fun bl -> 
  match bl with
  | SimpleBranch(a,b) -> b
  | CompoundBranch(n,m) ->
    match m with
    | (a,b) -> (calWeight a) + (calWeight b);;

let rec balanced : mobile -> bool =
  fun m -> 
  match m with
  | (SimpleBranch(a,b),SimpleBranch(c,d)) -> if a * b = c * d then true else false
  | (SimpleBranch(a,b),CompoundBranch(c,d)) -> if (a * b = (c * (calWeight (CompoundBranch(c,d))))) then true && (balanced d) else false
  | (CompoundBranch(c,d),SimpleBranch(a,b)) -> if (a * b = (c * (calWeight (CompoundBranch(c,d))))) then true && (balanced d) else false
  | (CompoundBranch(a,b),CompoundBranch(c,d)) -> if ((a * (calWeight (CompoundBranch(a,b)))) = (c * (calWeight (CompoundBranch(c,d))))) then (true && (balanced b) && (balanced d)) else false;;

