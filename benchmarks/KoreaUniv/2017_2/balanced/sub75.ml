(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec one b
= match b with
| SimpleBranch(l,w) -> w
| CompoundBranch(l,m) ->
(match m with
 | (b1,b2) -> (one b1)+(one b2));;

let rec two b
 = match b with
 | SimpleBranch(l,w) -> l
 | CompoundBranch(l,m) ->
 (match m with
  | (b1,b2) -> (two b1)+(two b2));;

let balanced : mobile -> bool
= fun m ->
match m with
| (b1,b2) -> 
if ((one b1)*(two b1)) = ((one b2)*(two b2)) then true else false