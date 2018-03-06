
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec help1 temp =
    match temp with
    SimpleBranch (l,w) -> w |
    CompoundBranch (l,m) ->
        (match m with
        (a,b) -> (help1 a) + (help1 b));;

let rec help2 temp =
    match temp with
    SimpleBranch (l,w) -> l*w |
    CompoundBranch (l,m) ->
        (match m with
        (a,b) -> if (help2 a)=(help2 b) then (((help1 a)+(help1 b))*l) else (-1));;

let balanced : mobile -> bool
=fun (lb,rb) ->
     if ((help2 lb)=(help2 rb)) then true else false
