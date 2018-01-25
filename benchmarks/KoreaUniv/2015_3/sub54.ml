module Problem1 = struct
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
end

module Problem2 = struct
type exp = 
V of var
|P of var * exp
|C of exp * exp
and var = string;;

let rec isinlist : ((string list) * string) -> bool
= fun (f, s) ->
match f with
|[] -> false;
|hd::tl -> if hd = s then true else isinlist(tl, s);;

let rec check2 : (exp * (string list)) -> bool
= fun (e, state) -> match e with
|V str -> isinlist (state, str)
|P (str, e) -> check2 (e, (str::state))
|C (e1, e2) -> (check2 (e1,state) && check2 (e2, state));;

let rec check : exp -> bool
= fun e -> check2 (e, []);;
end

