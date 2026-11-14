open! Src.Cs17_setup_hw09;;

type ('key, 'value) bst =
    Empty
  | Node of ('key * 'value) * ('key, 'value) children

and ('key, 'value) children = {
  left: ('key, 'value) bst;
  right: ('key, 'value) bst;
}

(* input: an ('a, 'b) bst and a key of type 'a to search for
   output: a 'b option representing the key associated with the 'a value, None 
   the key is not in the bst *)
let rec search = function
  | Empty, _ -> None
  | Node ((k, v), ch), x ->
    if x = k then Some v
    else if x < k then search (ch.left, x)
    else search (ch.right, x);;