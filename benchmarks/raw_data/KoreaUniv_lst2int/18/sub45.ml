let lst2int : int list -> int
= fun lst -> 
  let num = 0
  in let rec getdigits : int -> int
    = fun n ->
      String.length (string_of_int n)
  in let rec pow : int * int -> int
    = fun (a, b) ->
      match b with 
        | 1 -> a
        | _ -> a * pow(a, (b-1))
  in let rec lsttint : int * int list -> int
    = fun (num,lst) ->
      match lst with
        | [] -> num
        | x::xs -> lsttint (num*pow(10, getdigits(x)) + x, xs)
  in lsttint (num,lst);;