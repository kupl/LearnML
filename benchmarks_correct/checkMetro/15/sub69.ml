type lambda =
    |V of var
    |P of var * lambda
    |C of lambda * lambda
and var = string;;

let rec check met =
    let rec inList lst a =
        match lst with
        |[] -> false
        |b::sublist -> if a = b then true
                        else inList sublist a
    in
    let rec aux listArea met =
        match met with
        |V var -> inList listArea var
        |P (var, submet) -> aux (var::listArea) submet
        |C (submet1, submet2) -> (aux listArea submet1) && (aux listArea submet2)
    in
    aux [] met;;
