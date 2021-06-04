type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

type v_set = var list

let rec union1 ((s1 : string list), (s2 : string list)) : v_set =
  match s1 with
  | [] -> s2
  | hd :: tl -> if List.mem hd s2 then union1 (tl, s2) else union1 (tl, hd :: s2)


let rec find_erase (v : string) (s : string list) (sc : string list) : v_set =
  match s with
  | hd :: tl -> if hd = v then tl @ sc else find_erase v tl (hd :: sc)
  | [] -> sc


let rec union2 ((s1 : string list), (s2 : string list)) : v_set =
  match s1 with
  | [] -> s1
  | hd :: tl -> if List.mem hd s2 then hd :: union2 (tl, s2) else union2 (tl, s2)


let rec save (lam : lambda) (s : string list) : v_set =
  match lam with
  | P (_, l) -> save l s
  | C (l1, l2) -> union1 (save l1 s, save l2 s)
  | V v -> if List.mem v s then s else v :: s


let rec erase (lam : lambda) (s : string list) : v_set =
  match lam with
  | P (v, l) -> erase l (find_erase v s [])
  | C (l1, l2) -> union2 (erase l1 s, erase l2 s)
  | V v -> s


let check (lam : lambda) : bool =
  match erase lam (save lam []) with [] -> true | hd :: tl -> false


let (_ : bool) = check (P ("a", V "a"))

let (_ : bool) = check (P ("a", P ("a", V "a")))

let (_ : bool) = check (P ("a", P ("b", C (V "a", V "b"))))

let (_ : bool) = check (P ("a", C (V "a", P ("b", V "a"))))

let (_ : bool) = check (P ("a", V "b"))

let (_ : bool) = check (P ("a", C (V "a", P ("b", V "c"))))

let (_ : bool) = check (P ("a", P ("b", C (V "a", V "c"))))
