let vocal2 (int) =
    match int with
      1 -> []
    | 2 -> ["��"]
    | 3 -> ["��"]
    | 0 -> ["õ"]
let vocal1 (list,char,int) =
    match char with
      '0' -> if list = [] then ["��"] else []
    | '1' -> if int=1 then ["��"] else vocal2(int)
    | '2' -> ["��"] @ vocal2(int)
    | '3' -> ["��"] @ vocal2(int)
    | '4' -> ["��"] @ vocal2(int)
    | '5' -> ["��"] @ vocal2(int)
    | '6' -> ["��"] @ vocal2(int)
    | '7' -> ["ĥ"] @ vocal2(int)
    | '8' -> ["��"] @ vocal2(int)
    | '9' -> ["��"] @ vocal2(int)
let rec vocal (list,num,string) =
    let int = (String.length(string) - num) mod 4 in
    match int with
      1 -> if (num+1) = String.length string then [list @ vocal1(list,string.[num], int)]
           else [list @ vocal1(list,string.[num], int)] @ vocal([], num+1, string)
    | _ -> vocal(list @ vocal1(["a"], string.[num], int), num+1, string)
let vocalize string = vocal([],0,string)