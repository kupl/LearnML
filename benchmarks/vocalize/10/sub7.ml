(* ��ǻ�Ͱ��к� / 2005-11721 / ����� / ����1-3 *)
let vocal2 (int) =
    match int with
      1 -> []
    | 2 -> ["��"]
    | 3 -> ["��"]
    | 0 -> ["õ"]
    | _ -> []
let vocal1 (char,int) =
    match char with
      '0' -> []
    | '1' -> if int=1 then ["��"] else vocal2(int)
    | '2' -> ["��"] @ vocal2(int)
    | '3' -> ["��"] @ vocal2(int)
    | '4' -> ["��"] @ vocal2(int)
    | '5' -> ["��"] @ vocal2(int)
    | '6' -> ["��"] @ vocal2(int)
    | '7' -> ["ĥ"] @ vocal2(int)
    | '8' -> ["��"] @ vocal2(int)
    | '9' -> ["��"] @ vocal2(int)
    |  _  -> []
let rec vocal (list,num,string) =
    let int = (String.length(string) - num) mod 4 in
    let new_list = list @ vocal1(string.[num], int) in
    match int with
      1 -> let new_list1 = 
             if new_list = [] then ["��"] else new_list in
           if (num+1) = String.length string then [new_list1]
           else [new_list1] @ vocal([], num+1, string)
    | _ -> vocal(new_list, num+1, string)
let vocalize string = vocal([],0,string)