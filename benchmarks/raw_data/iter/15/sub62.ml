(*컴공 2014-10618 이세영 1-3*)
let iter (n, f)=
    let rec com (nn, ff) y=
        if nn>0 then ff (com (nn-1, ff) y) else y
    in fun x-> (com (n, f) x);;
