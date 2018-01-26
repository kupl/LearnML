(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let fst p = match p with (x,_) -> x
let snd p = match p with (_,x) -> x

exception Problem
let hd l = match l with [] -> raise Problem | hd::tl -> hd
let tl l = match l with [] -> raise Problem | hd::tl -> tl

let full_adder 
= fun m ->
  if m = (ZERO,ZERO,ZERO) then (ZERO,ZERO)
  else if m = (ZERO,ZERO,ONE) then (ONE,ZERO)
  else if m = (ZERO,ONE,ZERO) then (ONE,ZERO)
  else if m = (ZERO,ONE,ONE) then (ZERO,ONE)
  else if m = (ONE,ZERO,ZERO) then (ONE,ZERO)
  else if m = (ONE,ZERO,ONE) then (ZERO,ONE)
  else if m = (ONE,ONE,ZERO) then (ZERO,ONE)
  else if m = (ONE,ONE,ONE) then (ONE,ONE)
  else (ZERO,ZERO)

let rec sum
= fun b1 b2 c -> 
  if b1 = [] then 
    if b2 = [] then
      if c = ZERO then []
      else [ONE]
    else 
      let h2 = hd b2 in
      let t2 = tl b2 in
      let res = full_adder(ZERO,h2,c) in
      (fst res)::(sum b1 t2 (snd res))
  else
    let h1 = hd b1 in
    let t1 = tl b1 in
    if b2 = [] then
      let res = full_adder(h1,ZERO,c) in
      (fst res)::(sum t1 b2 (snd res))
    else
      let h2 = hd b2 in
      let t2 = tl b2 in
      let res = full_adder(h1,h2,c) in
      (fst res)::(sum t1 t2 (snd res))

let rec reverse
= fun l -> match l with
  [] -> []
| hd::tl -> (reverse tl)@[hd]

let rec process
= fun a b c -> match b with
  [] -> c
| hd::tl -> ( match hd with
    ZERO -> process a tl c
  | ONE -> process a tl (ZERO::(sum a c ZERO))
  )

let bmul : bin -> bin -> bin
= fun b1 b2 ->
  let sol = reverse (process (reverse b1) b2 []) in
  if sol = [] then [ZERO]
  else sol