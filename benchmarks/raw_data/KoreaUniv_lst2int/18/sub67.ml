let rec lst2int : int list -> int
= fun lst -> let rec check_digit : int -> int
  = fun n -> if n / 10 = 0 then 1 else 10 * check_digit (n / 10) in
  let rec check_list_digit : int list -> int
    = fun l -> match l with
      | [] -> 1
      | [a] -> check_digit a
      | hd :: tl -> check_digit hd * 10 * check_list_digit tl in
      match lst with
        | [] -> raise (Failure "List Too Short")
        | [a] -> a
        | hd :: tl -> hd * check_list_digit tl * 10 + lst2int tl;;