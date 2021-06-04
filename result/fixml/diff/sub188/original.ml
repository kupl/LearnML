type aexp =
|Const of int
|Var of string
|Power of (string * int)
|Times of aexp list
|Sum of aexp list

let rec map_2  = 
fun f a lst -> 
 (match lst with 
|[] -> a
|hd::tl -> 
let r  = f (a) (hd)
 in 
map_2 (f) (r) (tl))

let get_type  = 
fun exp -> 
 (match exp with 
|Const n -> n
|Power (str, n) -> n
|Sum lst -> List.length (lst)
|_ -> 0)

let get_list  = 
fun exp -> 
 (match exp with 
|Sum lst -> lst
|Times lst -> lst
|_ -> [])

let rec map_lst  : (((aexp * string) -> aexp) -> (aexp -> (string -> aexp list))) = 
fun f exp str -> 
let a  = get_list (exp)
 in 

 (match a with 
|[] -> []
|hd::tl -> 
let b  = f ((hd, str))
 in 

let c  = map_lst (f) (Sum (tl)) (str)
 in 
(b :: c))

let rec eval time_times  : ((aexp * string) -> aexp) = 
fun (exp, x) -> 
 (match exp with 
|Const n -> exp
|Power (str, n) -> exp
|Var str -> exp
|Times lst -> 
let a  = get_list (exp)
 in 

 (match a with 
|hd::tl -> time_times (Times ([Const (1); Power (("x", 0))])) (hd) (Times (tl))
|[] -> Times ([]))
|Sum lst -> 
let c  = map_lst (eval (time_times)) (exp) (x)
 in 
Sum (c))

let rec sum_time time_sum  = 
fun exp lst -> 
let a  = get_list (lst)
 in 

 (match exp with 
||Const _ |Var _ |Power (_, _) |Times _  -> 
 (match a with 
|hd::tl -> (Times ([exp; hd]) :: sum_time (time_sum) (exp) (Sum (tl)))
|[] -> [])
|Sum lst2 -> time_sum (exp) (lst))

let rec time_sum  = 
fun lst1 lst2 -> 
let b  = get_list (lst2)
 in 

 (match b with 
|hd::tl -> 
let a  = sum_time (time_sum) (hd) (lst1)
 in 
List.rev_append (a) (time_sum (lst1) (Sum (tl)))
|[] -> [])

let rec time_times  : (aexp -> (aexp -> (aexp -> aexp))) = 
fun exp1 x exp2 -> 
let a  = get_list (exp1)
 in 

let b  = get_list (exp2)
 in 

 (match x with 
|Const n -> 
let c  = n
 in 

let d  = get_type (List.nth (a) (0))
 in 

 (match b with 
|hd::tl -> time_times (Times ([Const ((d * c)); List.nth (a) (1)])) (hd) (Times (tl))
|[] -> Times ([Const ((d * c)); List.nth (a) (1)]))
|Var str -> 
 (match List.nth (a) (1) with 
|Power (str, n) -> 
 (match b with 
|hd::tl -> time_times (Times ([List.nth (a) (0); Power ((str, (n + 1)))])) (hd) (Times (tl))
|[] -> Times ([List.nth (a) (0); Power ((str, (n + 1)))]))
|_ -> raise Failure ("type error in time_times->Var()"))
|Power (str, n) -> 
let c  = n
 in 

 (match List.nth (a) (1) with 
|Power (str, n) -> 
 (match b with 
|hd::tl -> time_times (Times ([List.nth (a) (0); Power ((str, (n + c)))])) (hd) (Times (tl))
|[] -> Times ([List.nth (a) (0); Power ((str, (n + c)))]))
|_ -> raise Failure ("type error in time_times->Var()"))
|Times lst -> 
let p  = get_list (x)
 in 

 (match p with 
|[Const int1; Power (str, int2)] -> 
let e  = get_type (List.nth (p) (0))
 in 

let g  = get_type (List.nth (p) (1))
 in 

let h  = str
 in 

 (match b with 
|[] -> Times ([Const ((get_type (List.nth (a) (0)) * e)); Power ((h, (get_type (List.nth (a) (1)) + g)))])
|hd::tl -> time_times (Times ([Const ((get_type (List.nth (a) (0)) * e)); Power ((h, (get_type (List.nth (a) (1)) + g)))])) (hd) (Times (tl)))
|hd::tl -> 
let y  = time_times (Times ([Const (1); Power (("x", 0))])) (hd) (Times (tl))
 in 
time_times (exp1) (y) (exp2)
|[] -> 
 (match b with 
|hd::tl -> time_times (exp1) (hd) (Times (tl))
|[] -> exp1))
|Sum lst -> 
 (match List.nth (a) (1) with 
|Power (str, n) -> 
let h  = str
 in 

let e  = sum_time (time_sum) (exp1) (x)
 in 

let q  = eval (time_times) ((Sum (e), h))
 in 

 (match b with 
|[] -> q
|hd::tl -> 
let j  = time_times (Times ([Const (1); Power (("x", 0))])) (hd) (Times (tl))
 in 

 (match j with 
|Times lst -> 
let k  = sum_time (time_sum) (j) (q)
 in 
eval (time_times) ((Sum (k), h))
|Sum lst -> 
let p  = time_sum (j) (q)
 in 
eval (time_times) ((Sum (p), h))
|_ -> raise Failure ("Type error: j must be Times or Sum")))))

let rec differ  : (aexp -> aexp) = 
fun exp -> 
 (match exp with 
|Const n -> Const (0)
|Var str -> Const (1)
|Power (str, n) -> if (n != 0) then Times ([Const (n); Power ((str, (n - 1)))])
 else Times ([Const (n)])
|Times lst -> 
 (match lst with 
|[Const int1; Power (str, int2)] -> if (int2 = 0) then Const (0)
 else Times ([Const ((int1 * int2)); Power ((str, (int2 - 1)))])
|_ -> raise Failure ("type error in differ"))
|Sum lst -> 
let a  = List.map (differ) (lst)
 in 
Sum (a))

let diff  : ((aexp * string) -> aexp) = 
fun (exp, x) -> 
let a  = eval (time_times) ((exp, x))
 in 
differ (a)