(* ��ǻ�Ͱ��к� / 2005-11721 / ����� / ����1-1 *)
exception Error of string
let rec sigma (a,b,f) =
    if a > b then raise(Error "b is less than a")
    else if a = b then f(a)
    else f(a) + sigma (a+1,b,f)