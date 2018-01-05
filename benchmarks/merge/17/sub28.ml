let merge : int list * int list -> int list = fun (a, b) ->
  let rec r_merge : int list * int list * int list -> int list * int list * int list = fun (x, y, z) ->
    match x with
    | [] ->
      (match y with
       | [] -> (x, y, z)
       | head :: tail -> r_merge (x, tail, head :: z)
      )
    | head :: tail ->
      (match y with
       | [] -> (tail, y, head :: z)
       | head_y :: tail_y ->
         if (head > head_y) then r_merge (tail, y, head :: z)
         else r_merge (x, tail_y, head_y :: z)
      ) in
  let (in1, in2, result) = r_merge(a, b, []) in
  List.rev result
