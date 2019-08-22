type lambda=V of var
          |P of var*lambda
          |C of lambda*lambda
and var=string

let rec check:lambda->bool=fun(a)->
  let rec checkList:(var list*lambda)->bool=fun(l,m)->
    let rec checkNameinList:(var*(var list))->bool=fun(n,nl)->
      match nl with
        |[]->false
        |nlhd::nltl->(n=nlhd)||(checkNameinList(n,nltl))
    in
    match m with
      |V n->checkNameinList(n,l)
      |P (n,mt)->checkList(n::l,mt)
      |C (ma,mb)->checkList(l,ma)&&checkList(l,mb)
  in
  checkList([],a)
