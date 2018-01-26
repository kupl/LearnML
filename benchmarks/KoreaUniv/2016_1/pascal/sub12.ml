let rec factorial a =
    match a with
    1 -> 1
    | 0 -> 1
    |_ -> a* factorial (a-1) ;;

let permutation x y = (factorial x) / (factorial y) ;;

let pascal (x,y) = (permutation x y) / (factorial y ) ;;
