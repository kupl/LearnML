(* hw2ex4.ml *)


exception NOMOVE of string

type item = string
type tree = LEAF of item
          | NODE of tree list

type zipper = TOP
            | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper


let goLeft loc = match loc with
  | LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
  | LOC(t, TOP) -> raise (NOMOVE "right of top")
  | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  | LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of last")


let _ = goLeft (LOC (LEAF "*",
                     HAND([LEAF "c"],
                          HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]],
                               TOP,
                               []),
                          [LEAF "d"])))


let goUp loc = match loc with
  | LOC(t, TOP) -> raise (NOMOVE "top of top")
  | LOC(t, HAND(left, up, right)) -> LOC(NODE ((List.rev left)@t::right), up)

let goDown loc = match loc with
  | LOC(LEAF t, _) -> raise (NOMOVE "down of bottom")
  | LOC(NODE [], _) -> raise (NOMOVE "down of nothing")
  | LOC(NODE [left; middle; right], zip) -> LOC(left, HAND([], zip, [middle; right]))




(* testcase

  let _ = goUp (LOC (LEAF "*",
  HAND([LEAF "c"],
  HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]],
  TOP,
  []),
  [LEAF "d"])))

  let _ = goUp (LOC (NODE [LEAF "c"; LEAF "*"; LEAF "d"],
  HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, [])))



  let _ = goDown (LOC (NODE [LEAF "c"; LEAF "*"; LEAF "d"],
  HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, [])))

  let _ = goRight (LOC (LEAF "c",
  HAND ([], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []),
  [LEAF "*"; LEAF "d"]))
  )





  let loc1 = LOC (NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"]; 
  LEAF "+"; 
  NODE [LEAF "c"; LEAF "*"; LEAF "d"]], 
  TOP) 

  let (|>) g f = f g 

  let a91 = loc1 |> goDown 
  let a92 = loc1 |> goDown |> goDown 
  let a93 = loc1 |> goDown |> goUp |> goDown 
  let a94 = loc1 |> goDown |> goDown |> goRight 
  let a95 = loc1 |> goDown |> goDown |> goRight |> goLeft |> goRight |> goRight 
  let a96 = loc1 |> goDown |> goRight |> goRight |> goDown |> goRight 
  let a97 = 
  try (loc1 |> goUp |> ignore); false with NOMOVE _ -> true 
  ;;


*)







(*goRight: location -> location
  goUp: location -> location
  goDown: location -> location*)
