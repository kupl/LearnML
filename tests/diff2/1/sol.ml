type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> 
  match aexp with
  | Const n -> Const 0
  | Var st -> if st = x then Const 1 else Const 0
  | Power (st, n) -> if st = x then Times [Const n; Power (st , (n - 1))]
                               else Const 0
  | Sum lst -> (match lst with
    | [] -> Const 0
    | hd::tl -> (match tl with
      | [] -> Sum [(diff (hd , x)) ; Const 0]
      | _ -> Sum [(diff (hd , x)) ; diff (Sum tl, x)]))
  | Times lst -> (match lst with
    | [] -> Const 0
    | hd::tl -> (match tl with
      | [] -> Times [ diff (hd , x) ; Const 1]
      | _ -> let hdd = diff (hd , x)         in
                        let ttl = Times tl              in
                        let hdp = Times [hdd ; ttl]     in
                        let tlp = Times [hd; diff (ttl , x)]  in
                        Sum [hdp ; tlp]
    ))
