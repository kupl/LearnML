let rec merge:int list*int list->int list=fun(a,b)->
  match a with
  |[]->b
  |ahd::atl->(
    match b with
    |[]->a
    |bhd::btl->(
       if(ahd>bhd)
       then(ahd::merge(atl,b))
       else(bhd::merge(a,btl))
    )
  )
