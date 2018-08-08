open Syntax

exception Unbound


let empty_env = []
let extend x v env = (x, v) :: env

let rec lookup x env =
  try List.assoc x env with Not_found -> raise Unbound

exception EvalErr

(* env((name*thunk)list) -> expr -> value *)
let rec eval_expr env e =
  match e with
  | EConstInt i ->
    VInt i
  | EConstBool b ->
    VBool b
  | EVar x ->
    (try
      let Thunk(e,env') =  (lookup x env) in
      (eval_expr env' e)
     with
     | Unbound -> print_string("Not Found\n");
		raise EvalErr)
  | EAdd (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 + i2)
     | _ -> print_string "incompatible type\n";
            raise EvalErr)
  | ESub (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 - i2)
     | _ -> print_string "incompatible type\n";
		raise EvalErr)
  | EMul (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 * i2)
     | _ -> print_string "incompatible type\n";
		raise EvalErr)
  | EDiv (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 / i2)
     | _ -> print_string "incompatible type\n";
		raise EvalErr)
  | EAnd (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VBool true, VBool true -> VBool true
     | VBool _, VBool _ -> VBool false
     | _ -> print_string "incompatible type\n";
		raise EvalErr)
  | EOr (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VBool false, VBool false -> VBool false
     | VBool _, VBool _ -> VBool true
     | _ -> print_string "incompatible type\n";
		raise EvalErr)
  | EEq (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1,  VInt i2  -> VBool (i1 = i2)
     | _ -> print_string "incompatible type\n";
		raise EvalErr)
  | ELt (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1,  VInt i2  -> VBool (i1 < i2)
     | _ -> print_string "incompatible type\n";
		raise EvalErr)
  | EIf (e1,e2,e3) ->
    let v1 = eval_expr env e1 in
    (match v1 with
     | VBool b ->
       if b then eval_expr env e2 else eval_expr env e3
     | _ -> print_string "incompatible type\n";
		raise EvalErr)
  | ELet (e1,e2,e3) ->
    let th2 = Thunk(e2,env) in
    let env2 = (e1,th2)::env in
    (eval_expr env2 e3)
  | ELetRec(f,x,e1,e2) ->
	let env' = (extend f (Thunk(ELetFun(f,x,e1,env),env)) env)
	in
	(eval_expr env' e2)
  | EFun(x,e) -> VFun(x,e,env)
  | ELetFun(f,x,e1,env) -> VRecFun(f,x,e1,env)
  | EApp(e1,e2) ->
     let v1 = eval_expr env e1 in
     let newthunk = Thunk (e2,env) in
	(match v1 with
	| VFun(x,e,env')->
	  eval_expr (extend x newthunk env') e
	| VRecFun(f,x,e,oenv) ->
	let thunk2 = Thunk (ELetFun (f, x, e, oenv), oenv) in
          let env' = extend x newthunk (extend f thunk2 oenv)in
          eval_expr env' e
        | _ -> print_string "not function\n";
		raise EvalErr)
   | EPair(e1,e2) ->
        let exp1 = eval_expr env e1 in
        let exp2 = eval_expr env e2 in
        VPair(exp1,exp2)
  | ENil -> VNil
  | ECons (e1,e2) ->
        let exp1 = eval_expr env e1 in
        let exp2 = eval_expr env e2 in
        VCons(exp1,exp2)

let rec eval_command env c =
  match c with
  | CExp e -> ("-", env, eval_expr env e)
  | CDecl (e1,e2) -> 
  let v1 = eval_expr env e2 in
  let newthunk = Thunk(e2,env) in
   (e1, (e1,newthunk)::env, v1)
  | CRecDecl (f,x,e1) ->
        (f,(extend f (Thunk((ELetFun(f,x,e1,env)),env)) env),(VRecFun(f,x,e1,env)))

let rec print_value v =
  match v with
  | VInt i  -> print_int i
  | VBool b -> print_string (string_of_bool b)
  | VFun (name,expr,env) -> print_string "fun"
  | VRecFun  (name,expr,env,e1) -> print_string "fun"
  | VPair(a,b) -> print_string "(";print_value a;print_string ", ";print_value b; print_string ")"
  | VNil -> print_string "[]"
  | VCons(a,b) ->
  match b with
        |VNil -> (print_value a)
        |_ -> print_string "[";(print_value a);print_string ";";(print_value b);print_string "]"
