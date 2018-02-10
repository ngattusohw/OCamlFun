(* Nick Gattuso
I pledge my honor that I have abided by the Stevens Honor System *)


(* add : int -> int -> int *)
let add x y = x+y;;

type calcExp =
	| Const of int
	| Add of (calcExp*calcExp)
	| Sub of (calcExp*calcExp) 
	| Mult of (calcExp*calcExp)
	| Div of (calcExp*calcExp);;

let tTest = Node('a', Leaf 0, Leaf 0);;

(* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  EXERCISE 1 !!!!!!!!!!!!!!!!!!!!!!!!! *)

(* 1 ] Define dTree, an algebraic type in OCaml which encodes binary decision trees as depicted
in Fig 1. The names of the two constructors should be Leaf and Node. *)

type dTree = 
	| Node of (char*dTree*dTree)
	| Leaf of int;;

(* 2 ] Define two expressions, tLeft and tRight, of type dTree that represent each of the two
trees in Fig. 1 *)

let tLeft = Node ('w', Node ('x', Leaf 2, Leaf 5), Leaf 8);;
let tRight = Node('w', Node('x', Leaf 2, Leaf 5) , Node('y', Leaf 7, Leaf 5));;

(* 3 ] Implement the following functions indicating, for each one, its type. In the examples
below, we use tLeft to denote the example tree (Fig. 1–left) encoded as a value of type
dTree and similarly for tRight. *)

(* (a) dTree_height: that given a dTree returns its height *)

let rec dTree_height = function  
  | Leaf i -> 0
  | Node (_, left, right) -> 1 + max (dTree_height left) (dTree_height right);;

(* (b) dTree_size: that given a dTree returns its size. The size of a dTree consists of the
number of nodes and leaves. *)



