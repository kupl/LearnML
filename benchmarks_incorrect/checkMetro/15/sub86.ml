type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string


let rec checkMetro : metro -> bool = fun a -> 
    match a with
    | STATION q -> false
    | AREA(q,p) -> (match p with
                    | STATION x -> if q = x then true
                                   else false
                    | AREA(x,y) -> (match y with
                                    |AREA(a,b) -> checkMetro(AREA(a,b)) || checkMetro(AREA(x,b)) || checkMetro(AREA(q,b))
                                    |STATION a -> (a=x) || (a=q)
                                    |CONNECT(a,b) -> (match a, b with
                                       |STATION a, STATION b -> (a=q ||a=x)&&(b=q||b=x)
                                       |STATION a, AREA(c,d) -> (a=q||a=x)&&( checkMetro(AREA(c,d))||checkMetro(AREA(x,d))||checkMetro(AREA(q,d)) )
                                       |AREA(a,b), STATION c -> ( checkMetro(AREA(a,b)) || checkMetro(AREA(x,b)) || checkMetro(AREA(q,b)) )&&(c=q||c=x)
                                       |AREA(a,b), AREA(c,d) -> ( checkMetro(AREA(a,b)) || checkMetro(AREA(x,b))||checkMetro(AREA(q,b)) )&&( checkMetro(AREA(c,d))||checkMetro(AREA(x,d))||checkMetro(AREA(q,d)) )
                                       |_,_ -> checkMetro(a) && checkMetro(b)
                                       
                                       )   

                                    )
                    | CONNECT(x,y) -> (match x, y with
                                       |STATION a, STATION b -> (a=q)&&(b=q)
                                       |STATION a, AREA(c,d) -> (a=q)&&( checkMetro(AREA(c,d))||checkMetro(AREA(q,d)) )
                                       |AREA(a,b), STATION c -> (checkMetro(AREA(a,b))||checkMetro(AREA(q,b)))&&(c=q)
                                       |AREA(a,b), AREA(c,d) -> ( checkMetro(AREA(a,b))||checkMetro(AREA(q,b)) )&&( checkMetro(AREA(c,d))||checkMetro(AREA(q,d)) )
                                       |_,_ -> checkMetro(x) && checkMetro(y)
                                       
                                       )                   
                    )
    | CONNECT(q,p) -> checkMetro(q) && checkMetro(p)
 
           
(*
let _= 
let print_bool x = print_endline (string_of_bool x) in 

let a81 = checkMetro (AREA("a", STATION "a")) in 
let a82 = checkMetro (AREA("a", AREA("a", STATION "a"))) in 
let a83 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))) in 
let a84 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))) in 
let a85 = checkMetro (AREA("a", STATION "b")) in 
let a86 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))) in 
let a87 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))) in 

print_bool(false = checkMetro ( STATION "a")); 
print_bool(true = checkMetro ( CONNECT (AREA ("a", STATION "a"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "a")))))); 
print_bool(false = checkMetro ( CONNECT (AREA ("c", STATION "c"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "c")))))); 
print_bool(true = a81); 
print_bool(true = a82); 
print_bool(true = a83); 
print_bool(true = a84); 
print_bool(false = a85); 
print_bool(false = a86); 
print_bool(false = a87);;




*)
