(* let rec merge = fun (a, b) ->  *)
let rec merge ((a : int list),(b : int list)) : int list =
  match a with
    |[]->b
    |a_hd::a_tl->
      (match b with
        |[]->a
        |b_hd::b_tl->
        (
          if a_hd > b_hd then a_hd::merge(a_tl,b)
          else b_hd::merge(a,b_tl)
        )
      )
