open Syntax

exception Unbound
exception MatchErr
 type 'a option =
  | None
  | Some of 'a;;

let rec find_match pattern value =
match (pattern,value) with
|(PInt a,VInt b)when(a=b)-> Some []
|(PBool a,VBool b)when(a=b)-> Some []
|(PVar a,b)->Some [(a,b)]
|(PPair(a,b),VPair(c,d)) |(PCons (a,b),VCons(c,d))->
(match ((find_match a c),(find_match b d)) with
        |(Some e,Some f)->
        Some (e@f@[])
        |_ -> None)
|(PNil,VNil)->Some []
|(_,_)->None

let empty_env = []
let extend x v env = (x, v) :: env

let rec lookup x env =
  try List.assoc x env with Not_found -> raise Unbound

exception EvalErr

let rec eval_expr env e =
  match e with
  | EConstInt i ->
    VInt i
  | EConstBool b ->
    VBool b
  | EVar x ->
    (try
       lookup x env
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
    let value = eval_expr env e2 in
    let env2 = (e1,value)::env in
    (eval_expr env2 e3)
  | ELetRec(f,x,e1,e2) ->
        let env' =
        extend f (VRecFun(f,x,e1,env)) env
        in
        (eval_expr env' e2)
  | EFun(x,e) -> VFun(x,e,env)
  | EApp(e1,e2) ->
  let v1 = eval_expr env e1 in
     let v2 = eval_expr env e2 in
        (match v1 with
        | VFun(x,e,oenv)->
          eval_expr (extend x v2 oenv) e
        | VRecFun(f,x,e,oenv) ->
          let env' =
          extend x v2
          (extend f (VRecFun(f,x,e,oenv)) oenv)
           in
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
  | EMatch(e1,list1) ->
    let value =  (eval_expr env e1) in
    match list1 with
	|[] -> raise MatchErr
	|(p,e2)::xs -> 
	match (find_match p value) with
		|None -> (eval_expr env (EMatch(e1,xs)))
		|Some a -> (eval_expr (a@env) e2)


let rec eval_command env c =
  match c with
  | CExp e -> ("-", env, eval_expr env e)
  | CDecl (e1,e2) ->
  let value = (eval_expr env e2) in
   (e1, (e1,value)::env, value)
  | CRecDecl (f,x,e1) ->
        (f,(extend f (VRecFun(f,x,e1,env)) env),(VRecFun(f,x,e1,env)))


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
