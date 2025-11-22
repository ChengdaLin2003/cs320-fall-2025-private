open Utils
module Env = Utils.Env

exception DivByZero = Division_by_zero
exception AssertFail

(******************************************************************
 * 重新导出 ty / error / sfexpr / toplet / prog / expr / value 类型
 ******************************************************************)

type ty = Utils.ty =
  | IntTy
  | BoolTy
  | UnitTy
  | FunTy of ty * ty

type error = Utils.error =
  | ParseErr
  | UnknownVar of string
  | IfTyErr of ty * ty
  | IfCondTyErr of ty
  | OpTyErrL of bop * ty * ty
  | OpTyErrR of bop * ty * ty
  | FunArgTyErr of ty * ty
  | FunAppTyErr of ty
  | LetTyErr of ty * ty
  | LetRecErr of string
  | AssertTyErr of ty

type bop = Utils.bop =
  | Add | Sub | Mul | Div | Mod
  | Lt | Lte | Gt | Gte | Eq | Neq
  | And | Or

type sfexpr = Utils.sfexpr =
  | SUnit
  | SBool of bool
  | SNum of int
  | SVar of string
  | SFun of {
      args : (string * ty) list;
      body : sfexpr;
    }
  | SApp of sfexpr list
  | SLet of {
      is_rec : bool;
      name : string;
      args : (string * ty) list;
      ty : ty;
      binding : sfexpr;
      body : sfexpr;
    }
  | SIf of sfexpr * sfexpr * sfexpr
  | SBop of bop * sfexpr * sfexpr
  | SAssert of sfexpr

type toplet = Utils.toplet = {
  is_rec : bool;
  name : string;
  args : (string * ty) list;
  ty : ty;
  binding : sfexpr;
}

type prog = Utils.prog

type expr = Utils.expr =
  | Unit
  | Bool of bool
  | Num of int
  | Var of string
  | If of expr * expr * expr
  | Bop of bop * expr * expr
  | Fun of string * ty * expr
  | App of expr * expr
  | Let of {
      is_rec : bool;
      name : string;
      ty : ty;
      binding : expr;
      body : expr;
    }
  | Assert of expr

type value = Utils.value =
  | VUnit
  | VBool of bool
  | VNum of int
  | VClos of {
      arg  : string;
      body : expr;
      env  : dyn_env;
      name : string option;
    }

and dyn_env = value Env.t

(******************************************************************
 * Parser
 ******************************************************************)

let parse (s : string) : prog option =
  try
    let lexbuf = Lexing.from_string s in
    Some (Parser.prog Lexer.read lexbuf)
  with _ ->
    None

let string_of_value = Utils.string_of_value
let string_of_error = Utils.string_of_error

(******************************************************************
 * Helpers: 构造函数类型 / 柯里化 / 应用串联
 ******************************************************************)

let rec fun_ty_of_args (args : (string * ty) list) (ret_ty : ty) : ty =
  match args with
  | [] -> ret_ty
  | (_, t) :: tl -> FunTy (t, fun_ty_of_args tl ret_ty)

let rec curry_fun (args : (string * ty) list) (body_e : expr) : expr =
  match args with
  | [] -> body_e
  | (x, ty) :: tl ->
      Fun (x, ty, curry_fun tl body_e)

let rec chain_app (f : expr) (args : expr list) : expr =
  match args with
  | [] -> f
  | a :: tl -> chain_app (App (f, a)) tl

(******************************************************************
 * desugar sfexpr -> expr
 ******************************************************************)

let rec desugar_sf (e : sfexpr) : expr =
  match e with
  | SUnit -> Unit
  | SBool b -> Bool b
  | SNum n -> Num n
  | SVar x -> Var x

  | SFun { args; body } ->
      curry_fun args (desugar_sf body)

  | SApp lst ->
      (match lst with
       | [] -> failwith "empty SApp"
       | f :: args ->
           chain_app (desugar_sf f) (List.map desugar_sf args))

  | SIf (c, t, f) ->
      If (desugar_sf c, desugar_sf t, desugar_sf f)

  | SBop (op, e1, e2) ->
      Bop (op, desugar_sf e1, desugar_sf e2)

  | SAssert e1 ->
      Assert (desugar_sf e1)

  | SLet { is_rec; name; args; ty; binding; body } ->
      let binding_e = curry_fun args (desugar_sf binding) in
      let fun_ty = fun_ty_of_args args ty in
      Let {
        is_rec;
        name;
        ty = fun_ty;
        binding = binding_e;
        body = desugar_sf body;
      }

let desugar_toplet (t : toplet) (k : expr) : expr =
  Let {
    is_rec = t.is_rec;
    name = t.name;
    ty = fun_ty_of_args t.args t.ty;
    binding = curry_fun t.args (desugar_sf t.binding);
    body = k;
  }

let desugar (p : prog) : expr =
  match p with
  | [] -> Unit
  | _ ->
      let last = (List.hd (List.rev p)).name in
      List.fold_right desugar_toplet p (Var last)

(******************************************************************
 * Type checking
 ******************************************************************)

type ty_env = ty Env.t

let lookup (env : ty_env) (x : string) : (ty, error) result =
  match Env.find_opt x env with
  | Some t -> Ok t
  | None -> Error (UnknownVar x)

let empty_ty_env : ty_env = Env.empty

let rec type_of_with (env : ty_env) (e : expr) : (ty, error) result =
  match e with
  | Unit -> Ok UnitTy
  | Bool _ -> Ok BoolTy
  | Num _ -> Ok IntTy
  | Var x -> lookup env x

  | If (c, t, f) ->
      begin match type_of_with env c with
      | Error e -> Error e
      | Ok tc ->
          if tc <> BoolTy then Error (IfCondTyErr tc)
          else
            match type_of_with env t, type_of_with env f with
            | Ok tt, Ok tf ->
                if tt = tf then Ok tt else Error (IfTyErr (tt, tf))
            | Error e, _ | _, Error e -> Error e
      end

  | Bop (op, e1, e2) ->
      begin match type_of_with env e1, type_of_with env e2 with
      | Error e, _ | _, Error e -> Error e
      | Ok t1, Ok t2 ->
          begin match op with
          | Add | Sub | Mul | Div | Mod ->
              if t1 <> IntTy then Error (OpTyErrL (op, IntTy, t1))
              else if t2 <> IntTy then Error (OpTyErrR (op, IntTy, t2))
              else Ok IntTy
          | Lt | Lte | Gt | Gte ->
              if t1 <> IntTy then Error (OpTyErrL (op, IntTy, t1))
              else if t2 <> IntTy then Error (OpTyErrR (op, IntTy, t2))
              else Ok BoolTy
          | And | Or ->
              if t1 <> BoolTy then Error (OpTyErrL (op, BoolTy, t1))
              else if t2 <> BoolTy then Error (OpTyErrR (op, BoolTy, t2))
              else Ok BoolTy
          | Eq | Neq ->
              if t1 <> t2 then Error (OpTyErrR (op, t1, t2))
              else Ok BoolTy
          end
      end

  | Fun (x, arg_ty, body) ->
      begin match type_of_with (Env.add x arg_ty env) body with
      | Ok tbody -> Ok (FunTy (arg_ty, tbody))
      | Error e -> Error e
      end

  | App (e1, e2) ->
      begin match type_of_with env e1 with
      | Error e -> Error e
      | Ok (FunTy (arg_ty, ret_ty)) ->
          begin match type_of_with env e2 with
          | Ok targ ->
              if targ = arg_ty then Ok ret_ty
              else Error (FunArgTyErr (arg_ty, targ))
          | Error e -> Error e
          end
      | Ok t -> Error (FunAppTyErr t)
      end

  | Let { is_rec; name; ty; binding; body } ->
      if not is_rec then (
        match type_of_with env binding with
        | Ok t_binding ->
            if t_binding <> ty then Error (LetTyErr (ty, t_binding))
            else type_of_with (Env.add name ty env) body
        | Error e -> Error e
      ) else (
        (* let rec: 绑定必须是函数 *)
        match binding with
        | Fun _ ->
            let env' = Env.add name ty env in
            (match type_of_with env' binding with
             | Ok t_binding ->
                 if t_binding <> ty then Error (LetTyErr (ty, t_binding))
                 else type_of_with env' body
             | Error e -> Error e)
        | _ -> Error (LetRecErr name)
      )

  | Assert e1 ->
      begin match type_of_with env e1 with
      | Ok BoolTy -> Ok UnitTy
      | Ok t -> Error (AssertTyErr t)
      | Error e -> Error e
      end

let type_of (e : expr) : (ty, error) result =
  type_of_with empty_ty_env e

(******************************************************************
 * Evaluation（有环境的闭包 + let/let rec）
 ******************************************************************)

let int_of_value = function
  | VNum n -> n
  | _ -> failwith "expected int"

let bool_of_value = function
  | VBool b -> b
  | _ -> failwith "expected bool"

let rec eval_with (env : dyn_env) (e : expr) : value =
  match e with
  | Unit -> VUnit
  | Bool b -> VBool b
  | Num n -> VNum n
  | Var x ->
      (try Env.find x env with Not_found ->
         failwith ("unbound variable " ^ x))

  | If (c, t, f) ->
      if bool_of_value (eval_with env c)
      then eval_with env t
      else eval_with env f

  | Fun (x, _, body) ->
      (* 普通函数：闭包里不带名字 *)
      VClos { arg = x; body; env; name = None }

  | App (e1, e2) ->
      let vf = eval_with env e1 in
      let va = eval_with env e2 in
      begin match vf with
      | VClos { arg; body; env = clos_env; name } ->
          (* 递归函数：在闭包环境里加入 self 绑定 *)
          let base_env =
            match name with
            | None -> clos_env
            | Some f_name -> Env.add f_name vf clos_env
          in
          let env' = Env.add arg va base_env in
          eval_with env' body
      | _ ->
          failwith "apply non-function"
      end

  | Let { is_rec; name; ty = _; binding; body } ->
      if not is_rec then (
        (* 非递归 let：正常环境扩展 *)
        let v = eval_with env binding in
        let env' = Env.add name v env in
        eval_with env' body
      ) else (
        (* 递归 let：binding 必须是函数（由 type_of 保证） *)
        match binding with
        | Fun (arg, _arg_ty, fun_body) ->
            (* 闭包捕获当前 env，并记录名字用于递归；
               真正 self 绑定在 App 里通过 name 实现。 *)
            let clos = VClos { arg; body = fun_body; env; name = Some name } in
            let env' = Env.add name clos env in
            eval_with env' body
        | _ ->
            (* 理论上不会发生；保底回退到非递归语义。 *)
            let v = eval_with env binding in
            let env' = Env.add name v env in
            eval_with env' body
      )

  | Assert e1 ->
      if bool_of_value (eval_with env e1) then VUnit
      else raise AssertFail

  | Bop (op, e1, e2) ->
      begin match op with
      | And ->
          let v1 = eval_with env e1 in
          (match v1 with
           | VBool false -> VBool false
           | VBool true -> VBool (bool_of_value (eval_with env e2))
           | _ -> failwith "&& expects bool")
      | Or ->
          let v1 = eval_with env e1 in
          (match v1 with
           | VBool true -> VBool true
           | VBool false -> VBool (bool_of_value (eval_with env e2))
           | _ -> failwith "|| expects bool")

      | Add | Sub | Mul | Div | Mod ->
          let n1 = int_of_value (eval_with env e1) in
          let n2 = int_of_value (eval_with env e2) in
          begin match op with
          | Add -> VNum (n1 + n2)
          | Sub -> VNum (n1 - n2)
          | Mul -> VNum (n1 * n2)
          | Div ->
              if n2 = 0 then raise DivByZero else VNum (n1 / n2)
          | Mod ->
              if n2 = 0 then raise DivByZero else VNum (n1 mod n2)
          | _ -> assert false
          end

      | Lt | Lte | Gt | Gte ->
          let n1 = int_of_value (eval_with env e1) in
          let n2 = int_of_value (eval_with env e2) in
          let b =
            match op with
            | Lt  -> n1 <  n2
            | Lte -> n1 <= n2
            | Gt  -> n1 >  n2
            | Gte -> n1 >= n2
            | _ -> assert false
          in
          VBool b

      | Eq | Neq ->
          let v1 = eval_with env e1 in
          let v2 = eval_with env e2 in
          let eq =
            match v1, v2 with
            | VUnit, VUnit -> true
            | VBool b1, VBool b2 -> b1 = b2
            | VNum n1, VNum n2 -> n1 = n2
            | _ -> failwith "equality on unsupported value"
          in
          VBool (if op = Eq then eq else not eq)
      end

let eval (e : expr) : value =
  eval_with Env.empty e

(******************************************************************
 * Top-level interp
 ******************************************************************)

let interp (s : string) : (value, error) result =
  match parse s with
  | None -> Error ParseErr
  | Some p ->
      let e = desugar p in
      match type_of e with
      | Error err -> Error err
      | Ok _ -> Ok (eval e)