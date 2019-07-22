type metro =
    |STATION of name
    |AREA of name * metro
    |CONNECT of metro * metro
and name = string;;

let rec checkMetro met =
    let rec inList lst a =
        match lst with
        |[] -> false
        |b::sublist -> if a = b then true
                        else inList sublist a
    in
    let rec aux listArea met =
        match met with
        |STATION name -> inList listArea name
        |AREA (name, submet) -> aux (name::listArea) submet
        |CONNECT (submet1, submet2) -> (aux listArea submet1) && (aux listArea submet2)
    in
    aux [] met;;
