open Ast
open Ds


let from_some = function
  | None -> failwith "from_some: None"
  | Some v -> v

let g_store = Store.empty_store 20 (NumVal 0)

let init_env =
  ExtendEnv("i", RefVal (Store.new_ref g_store (NumVal 1)),
            ExtendEnv("v",RefVal (Store.new_ref g_store (NumVal 5)),
                      ExtendEnv("x",RefVal (Store.new_ref g_store (NumVal 10)),
                                EmptyEnv)))

let rec apply_proc f l =
  match f with 
  | ProcVal (xs,b,env) ->  
    (match xs,l with
    | x::xs,y::ys -> apply_proc (ProcVal(xs,b,(extend_env env x y))) ys
    | x::xs,[] -> ProcVal(x::xs,b,env)
    | [],y::ys -> failwith "You passed too many arguments!"
    | _ -> eval_expr env b)
  | _ -> failwith "apply_proc: Not a procVal"
and
  eval_expr (en:env) (e:expr):exp_val =
  match e with
  | Int n -> NumVal n
  | Var id ->
    (match apply_env en id with
     | None -> failwith @@ "Variable "^id^" undefined"
     | Some ev ->  ev)
  | ITE(e1, e2, e3) ->
    let v1 = eval_expr en e1 in
    if boolVal_to_bool v1
    then eval_expr en e2
    else eval_expr en e3
  | Add(e1, e2) ->
    let v1 = eval_expr en e1 in
    let v2 = eval_expr en e2  in
    NumVal ((numVal_to_num v1) + (numVal_to_num v2))
  | Mul(e1, e2) ->
    let v1 = eval_expr en e1 in
    let v2 = eval_expr en e2  in
    NumVal ((numVal_to_num v1) * (numVal_to_num v2))
  | Sub(e1, e2) ->
    let v1 = eval_expr en e1 in
    let v2 = eval_expr en e2  in
    NumVal ((numVal_to_num v1) - (numVal_to_num v2))
  | IsZero(e) ->
    let v1 = eval_expr en e  in
    BoolVal (numVal_to_num v1=0)
  | Let(x, e1, e2) ->
    let v1 = eval_expr en e1
    in eval_expr (extend_env en x v1) e2
  | Proc(x,e)      -> ProcVal (x,e,en)
  | App(e1,e2)     -> 
    let rec appHelper exprL evalL = 
      (match exprL with
      | x::xs -> 
        let v2 = eval_expr en x in
        appHelper xs (v2::evalL)
      | _ -> evalL) in 
    let v1 = eval_expr en e1 in
    apply_proc v1 (appHelper e2 [])
  | Letrec(id,params,body,e) ->
    eval_expr (ExtendEnvRec(id,params,body,en)) e
  | Set(id,e) ->
    let v=eval_expr en e
    in Store.set_ref g_store (refVal_to_int (from_some (apply_env en id))) v;
    UnitVal
  | BeginEnd(es) ->
    List.fold_left (fun v e -> eval_expr en e) UnitVal es
  | NewRef(e) ->
    RefVal(Store.new_ref g_store (eval_expr en e))
  | DeRef(e) ->
    let v1 = eval_expr en e
    in Store.deref g_store (refVal_to_int v1)
  | SetRef(e1,e2) ->
    let v1=eval_expr en e1
    in let v2=eval_expr en e2
    in Store.set_ref g_store (refVal_to_int v1) v2;
    UnitVal
  | Debug ->
    print_string "Environment:\n";
    print_string @@ string_of_env en;
    print_string "\nStore:\n";
    List.iteri (fun i s -> print_string (string_of_int i^"->"
                                         ^s^"\n")) @@ List.map
      string_of_expval @@ Store.store_to_list g_store;
    UnitVal
  | For(i,start,e,body) ->
    let v1 = eval_expr en start in 
    let v2 = eval_expr en e in 
    let startNum = numVal_to_num v1 in
    let endNum = numVal_to_num v2 in

    for x=startNum to endNum do
      eval_expr (extend_env en i (NumVal(x))) body
    done;
    UnitVal
  | _ -> failwith("Not implemented: "^string_of_expr e)
and
  eval_prog (Ast.AProg e) = eval_expr init_env e



(***********************************************************************)
(* Everything above this is essentially the same as we saw in lecture. *)
(***********************************************************************)




(* Parse a string into an ast *)


let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let interp (e:string) : exp_val =
  e |> parse |> eval_prog

let ex1 = "
let x = 7
in let y = 2
   in let y = let x = x-1
              in x-y
      in (x-8)- y"

let ex2 = "
   let g =
      let counter = 0
      in proc(d) {
         begin
           set counter = counter+1;
           counter
         end
         }
   in (g 11)-(g 22)"

let ex3 = "
  let g =
     let counter = newref(0)
     in proc (d) {
         begin
          setref(counter, deref(counter)+1);
          deref(counter)
         end
       }
  in (g 11) - (g 22)"

let ex4 = "
   let g =
      let counter = 0
      in proc(d) {
         begin
           set counter = counter+1;
           counter
         end
         }
   in debug"

let ex5 = "
let a = 3
in let b = 3
in let p = proc(x, y) {
    begin
      set x = 4;
      set y = 4;
      x
    end
  }
in begin
        (p a b);
        a
       end"
