(* problem 8*)

let rec kind_of_coins : int list -> int
= fun lst -> match lst with
          | [] -> 0
          | hd::t1 -> 1 + (kind_of_coins t1)

(* have to got biggest *)
let rec first_domination : int list -> int -> int
= fun lst kind_of_coins -> match lst with
                        | [] -> raise (Failure "list is too short")
                        | hd::t1 -> if kind_of_coins = 1 then hd
                                    else first_domination t1 (kind_of_coins-1)

let rec change : int list -> int -> int
= fun coins amount -> let rec cc  
                      = fun coins coins_kind_num amount -> if amount = 0 then 1
                                                         else if ((amount) < 0) || (coins_kind_num = 0) then 0
                                                         else cc coins (coins_kind_num-1) amount + cc coins coins_kind_num (amount- (first_domination coins coins_kind_num))
                      in cc coins (kind_of_coins coins) (amount)
