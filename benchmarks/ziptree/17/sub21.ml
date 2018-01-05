type item = string
type tree = LEAF of item 
          | NODE of tree list
type zipper = Top
            | HAND of tree list * zipper * tree list



