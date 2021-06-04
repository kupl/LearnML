type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check : lambda -> bool =
 fun lam ->
  let rec get lam lst1 lst2 =
    match lam with
    | V x -> (lst1, x :: lst2)
    | P (x, e) ->
        let lst' = x :: lst1 in
        get e lst' lst2
    | C (e1, e2) ->
        let t = get e1 lst1 lst2 in
        get e2 (fst t) (snd t)
  in

  let rec search lst1 lst2 =
    match lst2 with
    | [] -> true
    | hd :: tl ->
        let rec find p lst =
          match lst with
          | [] -> false
          | hd :: tl -> if hd = p then true else find p tl
        in
        if find hd lst1 then search lst1 tl else false
  in

  match lam with
  | V x -> true
  | _ ->
      let t = get lam [] [] in
      search (fst t) (snd t)
