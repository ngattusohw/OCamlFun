(* Nick Gattuso
I pledge my honor that I have abided by the Stevens Honor System *)


(* let tTest = Node('a', Leaf 0, Leaf 0);; *)

(* 1 ] Define dTree, an algebraic type in OCaml which encodes binary decision trees as depicted
in Fig 1. The names of the two constructors should be Leaf and Node. *)

(* type dTree = Node of (char * dTree * dTree) | Leaf of int *)
type dTree = 
	| Node of (char*dTree*dTree)
	| Leaf of int;;

(* 2 ] Define two expressions, tLeft and tRight, of type dTree that represent each of the two
trees in Fig. 1 *)

(* val tLeft : dTree = Node ('w', Node ('x', Leaf 2, Leaf 5), Leaf 8) *)
let tLeft = Node ('w', Node ('x', Leaf 2, Leaf 5), Leaf 8);;

(* val tRight : dTree = Node ('w', Node ('x', Leaf 2, Leaf 5), Node ('y', Leaf 7, Leaf 5)) *)
let tRight = Node('w', Node('x', Leaf 2, Leaf 5) , Node('y', Leaf 7, Leaf 5));;

(* 3 ] Implement the following functions indicating, for each one, its type. In the examples
below, we use tLeft to denote the example tree (Fig. 1–left) encoded as a value of type
dTree and similarly for tRight. *)

(* (a) dTree_height: that given a dTree returns its height *)

(* val dTree_height : dTree -> int = <fun> *)
let rec dTree_height = function  
  | Leaf (i) -> 0
  | Node (_, left, right) -> 1 + max (dTree_height left) (dTree_height right);;

(* (b) dTree_size: that given a dTree returns its size. The size of a dTree consists of the
number of nodes and leaves. *)

(* val dTree_size : dTree -> int = <fun> *)
let rec dTree_size = function
  | Leaf (i) -> 1
  | Node(_, left, right) -> 1 + dTree_size(left) + dTree_size(right);;

(* (c) dTree_paths: that given a dTree returns a list with all the paths to its leaves. A
path is a list of digits in the set {0, 1} such that if we follow it on the tree, it
leads to a leaf. The order in which the paths are listed is irrelevant. *)

(* val dTree_paths : dTree -> int list list = <fun> *)
let rec dTree_paths = function
  | Leaf(i) -> [[]]
  | Node(_, left, right) -> (List.map (fun l -> 0::l) (dTree_paths(left))) @
  							(List.map (fun l -> 1::l) (dTree_paths(right)));;

(* (d) dTree_is_perfect: that determines whether a dTree is perfect. A dTree is said to be
perfect if all leaves have the same depth *)

(* val dTree_is_perfect : dTree -> bool = <fun> *)
let rec dTree_is_perfect elem = 
	match elem with
		| Leaf i -> true
		| Node (_, left, right) -> 
			if((dTree_height left) = (dTree_height right) && (dTree_is_perfect left) && 
			(dTree_is_perfect right)) then
				true
			else 
				false;;

(* dTree_map: that given the following arguments
	f: char -> char
	g: int -> int
	t: dTree
returns a new dTree resulting from t by applying f to the characters in each node
and g to the numbers in each leaf. *)

(* val dTree_map : (char -> char) -> (int -> int) -> dTree -> dTree = <fun> *)
let rec dTree_map f g t =
	match t with
	 | Leaf(i) -> Leaf(g i)
	 | Node(c,l,r) -> Node(f c, dTree_map f g l, dTree_map f g r);; 


(* 4] Define list_to_tree, a function that given a list of characters l, creates a tree in which
the symbols of an inner node at level n corresponds to the n-th element in l. The
value of all of its leaves may be set to 0. E.g. list_to_tree [’x’;’y’;’z’] produces the
dTree representing: *)

(* val list_to_tree : char list -> dTree = <fun> *)
let rec list_to_tree l = 
	match l with
	| [] -> Leaf(0)
	| x::xs -> Node(x, (list_to_tree xs), (list_to_tree xs));;


(* 5] Define replace_leaf_at, a function that given a tree t and a graph for 
a function f, replaces all the leaves in t by the value indicated in the graph of the function *)


let rec replace_helper path tree v = 
	match path,tree with
	| [], Leaf(i) -> Leaf(v)
	| 0::xs, Node(c, left, right) -> Node(c, (replace_helper xs left v), right)
	| 1::xs, Node(c, left, right) -> Node(c, left, (replace_helper xs right v))
	| _ -> failwith "Bad input, :( ";;

(* val replace_leaf_at : dTree -> (int list * int) list -> dTree = <fun> *)
let rec replace_leaf_at t f =
	match f with
	| [] -> t
	| (f,v)::xs-> replace_leaf_at (replace_helper f t v) xs;;

(* 6] Define a function bf_to_dTree that takes a pair-encoding of a boolean function and
returns its tree-encoding. Note that, depending on the order in which the formal
parameters are processed, there may be more than one possible tree-encoding for a
given pair-encoding. You may return any of them *)

(* val bf_to_dTree : char list * (int list * int) list -> dTree = <fun> *)
let bf_to_dTree f = replace_leaf_at (list_to_tree (fst f)) (snd f);; 


