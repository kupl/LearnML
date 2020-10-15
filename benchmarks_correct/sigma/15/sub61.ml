(*컴공 2014-10618 이세영 1-2*)
let rec sigma f a b  =
    if b<a then 0 else (sigma f (a+1) b+f a);;
