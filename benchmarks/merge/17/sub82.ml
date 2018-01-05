(* 컴퓨터공학부 2013-11425 이창영 hw1_1 *)

let rec merge ((a: int list), (b: int list)) : int list =
    match (a, b) with
    | ([], _) -> b
    | (_, []) -> a
    | (h1::t1, h2::t2) -> if h1>h2 then h1:: merge (t1, b)
                            else h2:: merge (a, t2)
