type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let t1 : lambda = P ("a", V "a")

let t2 : lambda = P ("a", P ("a", V "a"))

let t3 : lambda = P ("a", P ("b", C (V "a", V "b")))

let t4 : lambda = P ("a", C (V "a", P ("b", V "a")))

let t5 : lambda = P ("a", V "b")

let t6 : lambda = P ("a", C (V "a", P ("b", V "c")))

let t7 : lambda = P ("a", P ("b", C (V "a", V "c")))

let rec make_Plst (lam : lambda) : string list =
  match lam with
  | P (v, lam') -> v :: make_Plst lam'
  | C (lam1, lam2) -> make_Plst lam1 @ make_Plst lam2
  | _ -> []


let rec make_Vlst (lam : lambda) : string list =
  match lam with
  | V v -> [ v ]
  | P (v, lam') ->
      List.filter (fun (__s9 : string) -> __s9 != v) (make_Vlst lam')
  | C (lam1, lam2) -> make_Vlst lam1 @ make_Vlst lam2


let rec check_in_lst (lst : 'a list) v : bool =
  match lst with
  | [] -> false
  | head :: tail -> if head = v then true else check_in_lst tail v


let rec check_VP (lstP : 'b list) (lstV : 'b list) : bool =
  match lstV with
  | [] -> true
  | head :: tail ->
      if check_in_lst lstP head = true then check_VP lstP tail else false


let check (lam : lambda) : bool =
  let lstP : string list = make_Plst lam in

  let lstV : string list = make_Vlst lam in
  List.length (make_Vlst lam) = 0
