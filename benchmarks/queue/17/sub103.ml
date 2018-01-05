(* Dept. of Computer Science and Engineering, 2015-12055, An Dantae, 2-6 *)
module type Queue =
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
  end

module IntListQ =
  struct
    type element = int list
    type queue = int list list * int list list
    exception EMPTY_Q
    let emptyQ : queue = ([],[])
    let enQ : queue * element -> queue = fun (q, e) ->
        match q with
        | (a, b) -> (e::a, b)
    let deQ : queue -> element * queue = fun q ->
        match q with
        | (a, []) -> 
            (let q' : queue = ([], List.rev a) in
             (match q' with
              | ([], []) -> raise (EMPTY_Q)
              | ([], h::t) -> (h, ([], t))
              | (_::_, _) -> raise (EMPTY_Q)))
        | (a, h::t) -> (h, (a, t))
  end

(* Test Code
module ValidIntListQ = (IntListQ : Queue)
let myQ = IntListQ.emptyQ
let yourQ = IntListQ.enQ(myQ, [1])
let (x,restQ) = IntListQ.deQ yourQ
let hisQ = IntListQ.enQ(yourQ, [2])

let rec helper1 : int list -> string = fun il -> match il with
    | [] -> ";"
    | h::t -> string_of_int h ^ "," ^ helper1(t)
let rec helper2 : int list list -> string = fun q -> match q with
    | [] -> "|"
    | h::t -> (helper1(h) ^ helper2(t))
let test : IntListQ.queue -> unit = fun q -> match q with
    | (a, b) -> print_endline(helper2(a) ^ helper2(b))

let _ = test(myQ)
let _ = test(yourQ)
let _ = test(restQ)
let _ = print_endline(helper1(x))
let _ = test(hisQ)
let (y, rQ) = IntListQ.deQ hisQ
let _ = test(rQ)
let _ = print_endline(helper1(y))
let sQ = IntListQ.enQ(rQ, [1;2;3])
let _ = test(sQ)
let (z, tQ) = IntListQ.deQ sQ
let _ = test(tQ)
let _ = print_endline(helper1(z))
let uQ = IntListQ.enQ(sQ, [4;4])
let _ = test(uQ)
let (w, vQ) = IntListQ.deQ uQ
let _ = test(vQ)
let _ = print_endline(helper1(w))
let (p, qQ) = IntListQ.deQ vQ
let _ = test(qQ)
let _ = print_endline(helper1(p))
let (o, eQ) = IntListQ.deQ myQ
*)
