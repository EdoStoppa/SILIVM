open List

(* controllare type_of_exps *)
(* Next steps:
    - Fix return typecheck
    - Extensive Testing!!!!!
    - Matrices
*)

type ident = string

type exp = Var of ident | Num of int | Bool of bool | Vec of int list
         | Add of exp * exp | Sub of exp * exp | Mul of exp * exp
         | And of exp * exp | Or of exp * exp | Eq of exp * exp 

type cmd = Assign of ident * exp | Seq of cmd * cmd | Skip
         | IfC of exp * cmd * cmd | While of exp * cmd
         | Call of ident * ident * exp list | Return of exp

type value = IntVal of int | BoolVal of bool | VecVal of int list

type typ = IntTy | BoolTy | FunTy of typ * typ list | VecTy



(* State definition *)
type entry = Val of value | Fun of ident list * cmd
type state = ident -> entry option
let empty_state = fun x -> None
let lookup_state (s : state) (x : ident) : entry option = s x
let update_state (s : state) (x : ident) (e : entry) : state = fun y -> if y = x then Some e else s y



(* Context definition*) 
type context = ident -> typ option
let empty_context = fun x -> None
let lookup_context (gamma : context) (x : ident) : typ option = gamma x
let update_context (gamma : context) (x : ident) (t : typ) : context = fun y -> if y = x then Some t else gamma y



(* Vector helper functions *)

(* Get nth element from Vec *)
let val_at_idx (Vec v) (idx : int) : int option = nth_opt v idx

(* Mixed operations between integers and vectors *)

(* Addition integer vector *)
let rec add_int_vec_aux (i: int) (v : int list) (ret : int list): int list = 
    match v with
    | [] -> rev ret
    | h :: rest -> add_int_vec_aux i rest ((h+i) :: ret)

let add_int_vec (i: int) (v : int list) : int list = 
    add_int_vec_aux i v []

(* Subtraction integer vector *)
let rec sub_int_vec_aux (i: int) (v : int list) (ret : int list): int list = 
    match v with
    | [] -> rev ret
    | h :: rest -> sub_int_vec_aux i rest ((h-i) :: ret)

let sub_int_vec (i: int) (v : int list) : int list = 
    sub_int_vec_aux i v []

(* Multiplication integer vector *)
let rec mul_int_vec_aux (i: int) (v : int list) (ret : int list): int list = 
    match v with
    | [] -> rev ret
    | h :: rest -> mul_int_vec_aux i rest ((h*i) :: ret)

let mul_int_vec (i: int) (v : int list) : int list = 
    mul_int_vec_aux i v []

(* Operations between vectors *)

(* Add two vectors *)
let rec add_vecs_aux (v1: int list) (v2 : int list) (ret : int list): int list option = 
    match v1, v2 with
    | [], [] -> Some (rev ret)
    | h1 :: rest1, h2 :: rest2 -> add_vecs_aux rest1 rest2 ((h1+h2) :: ret)
    | _, _ -> None

let add_vecs (v1: int list) (v2 : int list) : int list option = 
    add_vecs_aux v1 v2 []

(* Subtract two vectors *)
let rec sub_vecs_aux (v1: int list) (v2 : int list) (ret : int list): int list option = 
    match v1, v2 with
    | [], [] -> Some (rev ret)
    | h1 :: rest1, h2 :: rest2 -> sub_vecs_aux rest1 rest2 ((h1-h2) :: ret)
    | _, _ -> None

let sub_vecs (v1: int list) (v2 : int list) : int list option = 
    sub_vecs_aux v1 v2 []

(* Multiply two vectors *)
let rec mul_vecs_aux (v1: int list) (v2 : int list) (ret : int list): int list option = 
    match v1, v2 with
    | [], [] -> Some (rev ret)
    | h1 :: rest1, h2 :: rest2 -> mul_vecs_aux rest1 rest2 ((h1*h2) :: ret)
    | _, _ -> None

let mul_vecs (v1: int list) (v2 : int list) : int list option = 
    mul_vecs_aux v1 v2 []


(* Semantics *)
let rec eval_exp (e : exp) (s : state) : value option =
    match e with
    | Var x -> (match lookup_state s x with Some (Val v) -> Some v | _ -> None)
    | Num i -> Some (IntVal i)
    | Add (e1, e2) ->
        (match eval_exp e1 s, eval_exp e2 s with
        | Some (IntVal i1), Some (IntVal i2) -> Some (IntVal (i1 + i2))
        | Some (IntVal i), Some (VecVal v) -> Some (VecVal (add_int_vec i v))
        | Some (VecVal v), Some (IntVal i) -> Some (VecVal (add_int_vec i v))
        | Some (VecVal v1), Some (VecVal v2) -> 
            (match add_vecs v1 v2 with
            | Some v -> Some (VecVal v)
            | _ -> None)
        | _, _ -> None)
    | Sub (e1, e2) ->
        (match eval_exp e1 s, eval_exp e2 s with
        | Some (IntVal i1), Some (IntVal i2) -> Some (IntVal (i1 - i2))
        | Some (IntVal i), Some (VecVal v) -> Some (VecVal (sub_int_vec i v))
        | Some (VecVal v), Some (IntVal i) -> Some (VecVal (sub_int_vec i v))
        | _, _ -> None)
    | Mul (e1, e2) ->
        (match eval_exp e1 s, eval_exp e2 s with
        | Some (IntVal i1), Some (IntVal i2) -> Some (IntVal (i1 * i2))
        | Some (IntVal i), Some (VecVal v) -> Some (VecVal (mul_int_vec i v))
        | Some (VecVal v), Some (IntVal i) -> Some (VecVal (mul_int_vec i v))
        | _, _ -> None)
    | Bool b -> Some (BoolVal b)
    | And (e1, e2) -> 
        (match eval_exp e1 s, eval_exp e2 s with
        | Some (BoolVal b1), Some (BoolVal b2) -> Some (BoolVal (b1 && b2))
        | _, _ -> None)
    | Or (e1, e2) ->
        (match eval_exp e1 s, eval_exp e2 s with
        | Some (BoolVal b1), Some (BoolVal b2) -> Some (BoolVal (b1 || b2))
        | _, _ -> None)
    | Eq (e1, e2) ->
        (match eval_exp e1 s, eval_exp e2 s with
        | Some v1, Some v2 -> Some (BoolVal (v1 = v2))
        | _, _ -> None)
    | Vec v -> Some (VecVal v)

let rec eval_exps (es : exp list) (s : state) : value list option =
    match es with
    | [] -> Some []
    | e :: rest -> 
        (match eval_exp e s, eval_exps rest s with
        | Some v, Some vs -> Some (v :: vs)
        | _, _ -> None)

let rec add_args (s : state) (li : ident list) (lv : value list) : state =
    match li, lv with
    | i :: irest, v :: vrest -> add_args (update_state s i (Val v)) irest vrest
    | _, _ -> s

type stack = (state * ident) list

type config = cmd * stack * state


(* Function to execute a program in SILIVM *)
let rec step_cmd (c : cmd) (k : stack) (s : state) : config option =
    match c with
    | Assign (x, e) ->
        (match eval_exp e s with
        | Some v -> Some (Skip, k, update_state s x (Val v))
        | None -> None)
    | Seq (Skip, c2) -> Some (c2, k, s)
    | Seq (c1, c2) ->
        (match step_cmd c1 k s with
        | Some (c1', k', s') -> Some (Seq (c1', c2), k', s')
        | None -> None)
    | Skip -> None
    | IfC (e, c1, c2) ->
        (match eval_exp e s with
        | Some (BoolVal true) -> Some (c1, k, s)
        | Some (BoolVal false) -> Some (c2, k, s)
        | _ -> None)
    | While (e, c) -> Some (IfC (e, Seq (c, While (e, c)), Skip), k, s)
    | Return (e) ->
        (match k with 
        | [] -> None
        |(s0, x) :: r -> 
            (match eval_exp e s with 
            | Some v -> Some (Skip, r, update_state s0 x (Val v))
            | None -> None))
    | Call (x, f, es) -> (match eval_exps es s with 
        | Some vs ->
            (match lookup_state s f with 
            | Some Fun (xs, c) -> Some (c, (s, x)::k, (add_args s xs vs))
            | _ -> None)
        | None -> None)

let rec run_config (con : config) : config =
  let (c, k, s) = con in
  match step_cmd c k s with
  | Some con' -> run_config con'
  | None -> con

let run_prog (c : cmd) s =
  run_config (c, [], s)



(* Function that returns the type of an expression *)
let rec type_of (gamma : context) (e : exp) : typ option =
    match e with
    | Num i -> Some IntTy
    | Bool b -> Some BoolTy
    | Vec v -> Some VecTy
    | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) ->
        (match type_of gamma e1, type_of gamma e2 with
        | Some IntTy, Some IntTy -> Some IntTy
        | Some IntTy, Some VecTy -> Some VecTy
        | Some VecTy, Some IntTy -> Some VecTy
        | Some VecTy, Some VecTy -> Some VecTy
        | _, _ -> None)
    | And (e1, e2) | Or (e1, e2) -> 
        (match type_of gamma e1, type_of gamma e2 with
        | Some BoolTy, Some BoolTy -> Some BoolTy
        | _, _ -> None)
    | Eq (e1, e2) -> 
        (match type_of gamma e1, type_of gamma e2 with
        | Some IntTy, Some IntTy -> Some BoolTy
        | Some BoolTy, Some BoolTy -> Some BoolTy
        | Some VecTy, Some VecTy -> Some BoolTy
        | _, _ -> None)
    | Var x -> lookup_context gamma x

let rec type_of_exps (gamma : context) (es : exp list) : typ list option =
    match es with
    | [] -> Some []
    | e :: rest ->
        (match type_of gamma e, type_of_exps gamma rest with
        | Some t, Some ts -> Some (t :: ts)
        | _, _ -> None)



(* TExpressions typechecker *)
let typecheck_exp (gamma : context) (e : exp) : bool =
    match type_of gamma e with
    | None -> false
    | _ -> true



(* Simple helper function to compare two type lists *)
let rec compare_ty_lists (gamma : context) (l1: typ list) (l2 : typ list) : bool =
    match l1, l2 with
    | [], [] -> true
    | h_l1 :: rest_l1, h_l2 :: rest_l2 ->
        (if h_l1 = h_l2 then (compare_ty_lists gamma rest_l1 rest_l2)
        else false)
    | _, _ -> false


(* Commands Typechecker*)
let rec typecheck_cmd (gamma : context) (c : cmd) : bool =
    match c with
    | Assign (i, e) ->
        (match gamma i, type_of gamma e with
        | Some t1, Some t2 -> 
            (match t2 with
            | FunTy (ret_ty, args_ty) -> false
            |_ -> t1 = t2)
        | _, _ -> false)
    | Seq (c1, c2) -> typecheck_cmd gamma c1 && typecheck_cmd gamma c2
    | Skip -> true
    | Return e -> 
        (match type_of gamma e with
        | Some t -> true
        | _ -> false)
    |IfC (cond, c1, c2) ->
        (match type_of gamma cond with
        | Some (BoolTy) -> (typecheck_cmd gamma c1) && (typecheck_cmd gamma c2)
        | _ -> false)
    | While (cond, c) ->
        (match type_of gamma cond with
        | Some (BoolTy) -> typecheck_cmd gamma c
        | _ -> false)
    | Call (ret_name, call_f_name, call_args)-> 
        (match type_of gamma (Var ret_name) with
        | Some ret_call_t -> 
            (match gamma call_f_name with
            | Some FunTy (ret_ty, f_args_ty) -> 
                if ret_call_t != ret_ty then false else 
                (match type_of_exps gamma call_args with
                | Some call_args_ts -> (compare_ty_lists gamma call_args_ts f_args_ty)
                | _ -> false)
            | _ -> false)
        | _ -> false)
