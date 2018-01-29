(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let front t = match t with|(x,_)->x;;
let rear t = match t with|(_,y)->y;;

let rec wei :branch->int = fun branch->match branch with
|SimpleBranch (l,w)-> w
|CompoundBranch (l,m)->(match m with
	|(x,y)->(wei x)+(wei y)
);;

let rec eval : branch->int*bool = fun branch->
match branch with
|SimpleBranch (l,w)->(l*w,true)
|CompoundBranch (l,m)->(match m with
|(a,b)->let x= (front (eval a)) in let y = (front (eval b)) in
if (rear (eval a)) = false then (0,false) else if (rear (eval b)) = false then (0,false)
else if (x=y) then ((l*(wei a+wei b)),true) else (0,false));;

let rec balanced : mobile -> bool
= fun m -> match m with|(a,b)->if (rear (eval a))=false then false
else if (rear (eval b))=false then false
else if (front (eval a))=(front (eval b)) then true else false;;
