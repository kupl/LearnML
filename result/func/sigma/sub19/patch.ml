exception Don't_insert_minus

exception Don't_insert_a_which_bigger_than_b

let rec sigma (funx : int -> int) (a : int) (b : int) : int =
  match (b, b) with
  | __s5, __s6 ->
      if __s6 > b then 0
      else if __s5 = a then funx __s5
      else funx a + sigma funx (a + 1) __s5
