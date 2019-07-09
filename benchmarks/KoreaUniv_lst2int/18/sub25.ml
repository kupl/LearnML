let rec reverse x =
  match x with 
    |[]->[]
    |h::t->(reverse t) @ [h]
    let lst2int : int list ->int
    =fun lst->
      let rec sum x=
        match x with
          |[]->0
          |h::t ->h+ 10*(sum t)in
          sum(reverse lst);;
          lst2int[1;2;3];;
        