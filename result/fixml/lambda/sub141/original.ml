type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

type v_set = var list

let rec union1 : v_set * v_set -> v_set =
 fun (s1, s2) ->
  match s1 with
  | [] -> s2
  | hd :: tl -> if List.mem hd s2 then union1 (tl, s2) else union1 (tl, hd :: s2)


let rec find_erase : var -> v_set -> v_set -> v_set =
 fun v s sc ->
  match s with
  | hd :: tl -> if hd = v then tl @ sc else find_erase v tl (hd :: sc)
  | [] -> sc


let rec union2 : v_set * v_set -> v_set =
 fun (s1, s2) ->
  match s1 with
  | [] -> s1
  | hd :: tl -> if List.mem hd s2 then hd :: union2 (tl, s2) else union2 (tl, s2)


let rec save : lambda -> v_set -> v_set =
 fun lam s ->
  match lam with
  | P (_, l) -> save l s
  | C (l1, l2) -> union1 (save l1 s, save l2 s)
  | V v -> if List.mem v s then s else v :: s


let rec erase : lambda -> v_set -> v_set =
 fun lam s ->
  match lam with
  | P (v, l) -> erase l (find_erase v s [])
  | C (l1, l2) -> union2 (erase l1 s, erase l2 s)
  | V v -> s


let check : lambda -> bool =
 fun lam -> match erase lam (save lam []) with [] -> true | hd :: tl -> false


let _ = check (P ("a", V "a"))

let _ = check (P ("a", P ("a", V "a")))

let _ = check (P ("a", P ("b", C (V "a", V "b"))))

let _ = check (P ("a", C (V "a", P ("b", V "a"))))

let _ = check (P ("a", V "b"))

let _ = check (P ("a", C (V "a", P ("b", V "c"))))

let _ = check (P ("a", P ("b", C (V "a", V "c"))))
