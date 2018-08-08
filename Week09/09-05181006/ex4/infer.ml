open Syntax
open TySyntax
open ConstraintSolver
exception Unbound
exception EvalUnbound

type tyenv = (name * ty) list
let empty_env = []
let extend x v env = (x, v) :: env

let rec lookup x env =
  try List.assoc x env with Not_found -> raise Unbound

exception InferErr

let rec infer_expr tyenv e =
  match e with
  | EConstInt i ->
    (TyInt,[])
  | EConstBool b ->
    (TyBool,[])
  | EVar x ->
    (try
       ((lookup x tyenv),[])
     with
     | Unbound -> print_string("Unbound Value\n");
		raise InferErr)
  | EAdd (e1,e2) ->
  let (x1,const1) = infer_expr tyenv e1 in
    let (x2,const2) = infer_expr tyenv e2 in
    (TyInt,[(x1,TyInt);(x2,TyInt)]@const1@const2)
  | ESub (e1,e2) ->
  let (x1,const1) = infer_expr tyenv e1 in
    let (x2,const2) = infer_expr tyenv e2 in
    (TyInt,[(x1,TyInt);(x2,TyInt)]@const1@const2)
| EMul (e1,e2) ->
  let (x1,const1) = infer_expr tyenv e1 in
    let (x2,const2) = infer_expr tyenv e2 in
    (TyInt,[(x1,TyInt);(x2,TyInt)]@const1@const2)
  | EDiv (e1,e2) ->
    let (x1,const1) = infer_expr tyenv e1 in
    let (x2,const2) = infer_expr tyenv e2 in
    (TyInt,[(x1,TyInt);(x2,TyInt)]@const1@const2) 
  | EAnd (e1,e2) ->
  let (x1,const1) = infer_expr tyenv e1 in
    let (x2,const2) = infer_expr tyenv e2 in
    (TyBool,[(x1,TyBool);(x2,TyBool)]@const1@const2)
  | EOr (e1,e2) ->
  let (x1,const1) = infer_expr tyenv e1 in
    let (x2,const2) = infer_expr tyenv e2 in
    (TyBool,[(x1,TyBool);(x2,TyBool)]@const1@const2)
  | EEq (e1,e2) ->
    let (x1,const1) = infer_expr tyenv e1 in
    let (x2,const2) = infer_expr tyenv e2 in
    (TyBool,[(x1,TyInt);(x2,TyInt)]@const1@const2)
  | ELt (e1,e2) ->
    let (x1,const1) = infer_expr tyenv e1 in
    let (x2,const2) = infer_expr tyenv e2 in
    (TyBool,[(x1,TyInt);(x2,TyInt)]@const1@const2)
  | EIf (e1,e2,e3) ->
    let v1 = infer_expr tyenv e1 in
    let v2 = infer_expr tyenv e2 in
    let v3 = infer_expr tyenv e3 in
    (match v1,v2,v3 with
     | (x1,const1),(x2,const2),(x3,const3) ->
       (x2,[(x1,TyBool);(x2,x3)]@const1@const2@const3))
  | ELet (e1,e2,e3) ->
    let (x2,const2) = infer_expr tyenv e2 in
    	let newenv = (e1,x2)::tyenv in
    	let (x3,const3) = infer_expr newenv e3 in
	(x3,const2@const3)
  | ELetRec(f,x,e1,e2) ->
	let newvarx = new_tyvar() in
	let newvare1 = new_tyvar() in
	let newenv = (f,TyFun(TyVar newvarx,TyVar newvare1))::(x,TyVar newvarx)::tyenv in
	let (v1,const1) = infer_expr newenv e1 in
	let (v2,const2) = (infer_expr ((f,TyFun (TyVar newvarx,TyVar newvare1))::tyenv) e2) in
	(v2,(v1,TyVar newvare1)::const1@const2)
  | EFun(x,e) -> 
	let newvar = (TyVar (new_tyvar())) in
	let (x1,const1) = infer_expr ((x,newvar)::tyenv) e in
	(TyFun (newvar,x1),const1)
  | EApp(e1,e2) ->
     let (v1,const1) = infer_expr tyenv e1 in
     let (v2,const2) = infer_expr tyenv e2 in
     let newvar = (TyVar (new_tyvar())) in
	(newvar,(v1,TyFun(v2,newvar))::const1@const2)
  |EPair (e1,e2) ->
        let (v1,const1) = infer_expr tyenv e1 in
        let (v2,const2) = infer_expr tyenv e1 in
        (TyPair (v1,v2),const1@const2)
  |ENil -> let a = new_tyvar() in
        (TyList (TyVar a),[])
  |ECons (e1,e2) ->
        let (v1,const1) = infer_expr tyenv e1 in
        let (v2,const2) = infer_expr tyenv e1 in
        let a = (TyVar (new_tyvar())) in
        (TyList a,[(a,v1);(TyList a,v2)]@const1@const2)

let rec infer_cmd tyenv c =
  try
  match c with
  | CExp e -> 
  let (x,const) = infer_expr tyenv e in
  let x1 = ty_subst (ty_unify const) x in
  (x1,tyenv)	
  | CDecl (e1,e2) -> 
  let (t,const) = (infer_expr tyenv e2) in
  let tx = (ty_subst (ty_unify const) t) in
   (tx,(e1,tx)::tyenv)
  | CRecDecl (f,x,e1) ->
  let newvarx = TyVar (new_tyvar()) in
        let newvare1 = (TyVar (new_tyvar())) in
        let newenv = (f,TyFun(newvarx,newvare1))::(x,newvarx)::tyenv in
        let (v1,const1)= infer_expr newenv e1 in
	let fvar = ty_subst (ty_unify((v1,newvare1)::const1)) (TyFun(newvarx,newvare1)) in
	(fvar,(f,fvar)::tyenv)
   with
   | TyError -> raise InferErr
   | Unbound -> raise InferErr

