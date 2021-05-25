let rec lst2int : int list -> int
= fun lst -> (*TODO*)
  let rec pow = fun n l ->
    match l with
      [] -> n
      |hd::tl -> (pow (10*n) tl)
  in
  match lst with
    [] -> 0
    | hd::tl -> pow hd tl + lst2int tl
    ;;
    
lst2int [2;3;4;5;7;4;2;3;4;6];;