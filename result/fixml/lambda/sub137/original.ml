type lambda = V of var | P of var * lambda | C of lambda * lambda

and var = string

let t1 = P ("a", V "a")

let t2 = P ("a", P ("a", V "a"))

let t3 = P ("a", P ("b", C (V "a", V "b")))

let t4 = P ("a", C (V "a", P ("b", V "a")))

let t5 = P ("a", V "b")

let t6 = P ("a", C (V "a", P ("b", V "c")))

let t7 = P ("a", P ("b", C (V "a", V "c")))

let rec make_Plst lam =
  match lam with
  | P (v, lam') -> v :: make_Plst lam'
  | C (lam1, lam2) -> make_Plst lam1 @ make_Plst lam2
  | _ -> []


let rec make_Vlst lam =
  match lam with
  | V v -> [ v ]
  | P (v, lam') -> make_Vlst lam'
  | C (lam1, lam2) -> make_Vlst lam1 @ make_Vlst lam2


let rec check_in_lst lst v =
  match lst with
  | [] -> false
  | head :: tail -> if head = v then true else check_in_lst tail v


let rec check_VP lstP lstV =
  match lstV with
  | [] -> true
  | head :: tail ->
      if check_in_lst lstP head = true then check_VP lstP tail else false


let check : lambda -> bool =
 fun lam ->
  let lstP = make_Plst lam in
  let lstV = make_Vlst lam in
  check_VP lstP lstV
