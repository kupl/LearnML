type metro=STATION of name
          |AREA of name*metro
          |CONNECT of metro*metro
and name=string

let rec checkMetro:metro->bool=fun(a)->
  let rec checkMetroList:(name list*metro)->bool=fun(l,m)->
    let rec checkNameinList:(name*(name list))->bool=fun(n,nl)->
      match nl with
        |[]->false
        |nlhd::nltl->(n=nlhd)||(checkNameinList(n,nltl))
    in
    match m with
      |STATION n->checkNameinList(n,l)
      |AREA (n,mt)->checkMetroList(n::l,mt)
      |CONNECT (ma,mb)->checkMetroList(l,ma)&&checkMetroList(l,mb)
  in
  checkMetroList([],a)
