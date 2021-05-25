let lst2int : int list -> int = fun lst ->
  let rec func lst result =
    match lst with
      |[] -> result
      |hd::tl -> func tl ((result * 10) + hd)
  in func lst 0;;
  
lst2int [2;3;4;5];;