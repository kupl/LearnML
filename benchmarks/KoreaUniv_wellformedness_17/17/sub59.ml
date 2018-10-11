(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
(*and var = string*)


let rec is_there
= fun x arr -> match arr with
  | [] -> false
  | hd::tl -> if x = hd then true else (is_there x tl)

let rec find
= fun lam arr -> match lam with
  | V x -> (is_there x arr, arr)
  (*(match arr with
      | [] -> (false, [])
      | hd::tl -> if x = hd then (true, arr) else (find (V x) tl))*)
  | P (x, l) -> let arr1 = x::arr in 
                (match l with
                | V x1 -> find (V x1) arr1
                | P (x1, l1) -> find (P (x1, l1)) arr1
                | C (l1, l2) -> (match l1 with
                    | V x2 -> let (t1, a1) = (find (V x2) arr1) in
                              let (t2, a2) = (find l2 a1) in
                              (t1&&t2, a2)
                    | _ -> find l arr1))
  | C (l1, l2) -> let (t1, a1) = (find l1 arr) in
                  let (t2, a2) = (find l2 a1) in
                  (t1&&t2, a2)
(*(find l1 arr)&&(find l2 arr)*)

let rec check : lambda -> bool
= fun lam -> match (find lam []) with
  | (true, _) -> true
  | (false, _) -> false
