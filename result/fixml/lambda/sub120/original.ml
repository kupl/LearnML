type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec mklist l lam =
  match lam with
  | P (var, lda) -> mklist (var :: l) lda
  | V var -> l
  | C (lda1, lda2) ->
      let l1 = mklist l lda1 in

      let l2 = mklist l lda2 in
      l1 @ l2


let rec checker l var =
  match l with
  | [] -> false
  | hd :: tl -> if var = hd then true else checker tl var


let rec free_check l lam =
  match lam with
  | V var -> checker l var
  | P (var, lda) -> free_check l lda
  | C (lda1, lda2) ->
      if free_check l lda1 = true then free_check l lda2 else false


let rec check : lambda -> bool =
 fun lam ->
  match lam with
  | V var -> false
  | P (v, lam) ->
      let bound_list = mklist [] (P (v, lam)) in
      free_check bound_list lam
  | C (lda1, lda2) ->
      let b_list1 = mklist [] lda1 in

      let b_list2 = mklist [] lda2 in
      free_check b_list1 lda1 && free_check b_list2 lda2
