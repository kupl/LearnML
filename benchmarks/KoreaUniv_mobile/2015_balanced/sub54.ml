type mobile = branch * branch
and branch = SimpleBranch of length * weight
| CompoundBranch of length * mobile
and length = int
and weight = int;;

let rec getbranch : branch -> int
= fun b -> match b with
|SimpleBranch (_, weight) -> weight
|CompoundBranch (_, (bl,br)) -> (getbranch(bl) + getbranch(br));;

let getlength : branch -> int
= fun b -> match b with
|SimpleBranch (length, _) -> length
|CompoundBranch (length, _) -> length;;

let balanced : mobile -> bool
= fun (lb, rb) ->
let lweight = match lb with
|SimpleBranch (length, weight) -> (getlength(lb) * getbranch(lb))
|CompoundBranch (length, (b1, b2)) -> (getlength(lb) * (getbranch(b1) + getbranch(b2))) in 
let rweight = match rb with
|SimpleBranch (length, weight) -> (getlength(lb) * (getbranch(lb)))
|CompoundBranch (length, (b1, b2)) -> (getlength(lb) * (getbranch(b1) + getbranch(b2))) in
lweight = rweight;;
