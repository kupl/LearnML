(* ��ǻ�Ͱ��к� / 2005-11721 / ����� / ����1-1 *)
let rec sigma f a b =
    if a>b then 0
    else f(a) + sigma f (a+1) b