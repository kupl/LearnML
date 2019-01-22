(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
| Const c -> Const 0
| Var v -> if v = x then Const 1 else Var v
| Power (str, po) -> begin match str with
      | x -> Times [Const po ; Power(str, po -1)]
     (* | _ -> Power (str, po) *)
  end
| Times l -> begin match l with
      | [] -> raise (Failure "wrong format")
      | hd::tl -> let inner_hd = match tl with
                  | hd2 :: tl2 -> hd2
                  | [] -> Const 0

      in Sum[Times[diff (hd, x); inner_hd]; Times[hd ; diff(inner_hd, x)]]
  end
| Sum l -> match l with
      | [] -> raise (Failure "wrong format")
      | hd::tl -> let inner_hd = match tl with
        | hd2 :: tl2 -> hd2
        | [] -> Const 0 in Sum[diff(hd, x); diff(inner_hd, x)]