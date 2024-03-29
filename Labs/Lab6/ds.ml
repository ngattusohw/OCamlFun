
(* This file defines expressed values and environments *)


(* expressed values and environments are defined mutually recursively *)

type exp_val =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of string*Ast.expr*env
  | RefVal of int
  | UnitVal
  | PairVal of exp_val*exp_val
  | TreeVal of btree
  | ListVal of exp_val list

and
  env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env
  | ExtendEnvRec of string*string*Ast.expr*env
and
  btree =
  | EmptyNodeVal of Ast.texpr
  | NodeVal of exp_val*btree*btree


(* Operations on environments *)

let empty_env (x:unit):env = EmptyEnv

let extend_env (env:env) (id:string) (v:exp_val):env = ExtendEnv(id,v,env)

let rec apply_env (env:env) (id:string):exp_val option =
  match env with
  | EmptyEnv -> None
  | ExtendEnv (key,value,env) ->
    if id=key
    then Some value
    else apply_env env id
  | ExtendEnvRec(key,param,body,en) ->
    if id=key
    then Some (ProcVal(param,body,env))
    else apply_env en id


(* operations on expressed values *)

let numVal_to_num =  function
  |  NumVal n -> n
  | _ -> failwith "Expected a number!"

let boolVal_to_bool =  function
  |  BoolVal b -> b
  | _ -> failwith "Expected a boolean!"

let refVal_to_int =  function
  |  RefVal n -> n
  | _ -> failwith "Expected a reference!"

let unitVal_to_unit =  function
  |  UnitVal -> ()
  | _ -> failwith "Expected a Unit!"

let pairVal_to_fst =  function
  | PairVal(x,y) -> x
  | _ -> failwith "Expected a PairVal!"

let pairVal_to_snd =  function
  | PairVal(x,y) -> y
  | _ -> failwith "Expected a PairVal!"

let listVal_to_list =  function
  | ListVal l -> l
  | _ -> failwith "Expected a ListVal!"

let treeVal_to_tree =  function
  | TreeVal t -> t
  | _ -> failwith "Expected a TreeVal!"

let rec string_of_list = function
  | x::[] -> (string_of_expval x)
  | x::xs -> (string_of_expval x) ^ "," ^ string_of_list xs

 and string_of_expval = function
  | NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b
  | ProcVal (id,body,env) -> "ProcVal ("^id^","^Ast.string_of_expr body^","^ string_of_env env^")"
  | RefVal i -> "RefVal (" ^ string_of_int i ^ ")"
  | PairVal(e1,e2) -> "PairVal (" ^ string_of_expval e1 ^ "," ^ string_of_expval e2 ^ ")"
  | ListVal l -> "ListVal ([" ^ string_of_list l ^ "])" 
  | TreeVal t -> "TreeVal (" ^ string_of_btree t ^ ")"
  | UnitVal -> "UnitVal"
 and
  string_of_env  = function
  | EmptyEnv -> ""
  | ExtendEnv(id,v,env) -> "("^id^","^string_of_expval v^")"^string_of_env env
  | ExtendEnvRec(id,param,body,env) -> "("^id^","^param^","^Ast.string_of_expr body^")"^string_of_env env
 and
  string_of_btree = function
  | EmptyNodeVal t -> "EmptyNode"
  | NodeVal(data, lst, rst) -> "NodeVal(" ^ (string_of_expval data) ^ "," ^ (string_of_btree lst) ^ "," ^ (string_of_btree rst) ^ ")"
