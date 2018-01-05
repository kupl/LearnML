let rec merge : int list * int list -> int list = fun (a, b) ->
  match a, b with
  | [], x -> x
  | x, [] -> x
  | head_x :: tail_x, head_y :: tail_y ->
    if(head_x > head_y) then head_x :: merge(tail_x, b)
    else head_y :: merge(a, tail_y)
