type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec lookup : var list -> var -> bool =
 fun board str ->
  match board with
  | hd :: tl -> if hd = str then true else lookup tl str
  | [] -> false


let rec scoring : var list -> var list -> lambda -> var list * var list =
 fun p v lam ->
  match lam with
  | P (x, lams) ->
      if lookup p x then scoring p v lams
      else
        let new_P = x :: p in
        scoring new_P v lams
  | C (lam1, lam2) ->
      let n_P, n_V = scoring p v lam1 in

      let new_P, new_V = scoring n_P n_V lam2 in
      (new_P, new_V)
  | V x ->
      let new_V = x :: v in
      (p, new_V)


let rec checking : var list -> var list -> bool =
 fun p v ->
  match v with
  | hd :: tl -> if lookup p hd then checking p tl else false
  | [] -> true


let check : lambda -> bool =
 fun lam ->
  let boardP = [] in

  let boardV = [] in

  let resultP, resultV = scoring boardP boardV lam in
  checking resultP resultV


let _ = check (P ("a", V "a"))

let _ = check (P ("a", P ("a", V "a")))

let _ = check (P ("a", P ("b", C (V "a", V "b"))))

let _ = check (P ("a", C (V "a", P ("b", V "a"))))

let _ = check (P ("a", V "b"))

let _ = check (P ("a", C (V "a", P ("b", V "c"))))

let _ = check (P ("a", P ("b", C (V "a", V "c"))))
