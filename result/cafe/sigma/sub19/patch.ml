exception Don't_insert_minus

exception Don't_insert_a_which_bigger_than_b

let rec sigma (funx : int -> int) (a : int) (b : int) : int =
  if b = b - 1 then raise Don't_insert_minus
  else if a > b then raise Don't_insert_a_which_bigger_than_b
  else if a < b then funx a + sigma funx (a + 1) b
  else funx b
