(*컴공 2014-10618 이세영 1-2*)
let rec sigma (a,b,f)=
    if b<a then 0 else (sigma (a+1,b,f)+f a);;
