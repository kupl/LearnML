(* ��ǻ�Ͱ��к� / 2005-11721 / ����� / ����1-1 *)
let rec sigma (a,b,f) =
    if a>b then 0
    else f(a) + sigma (a+1,b,f)