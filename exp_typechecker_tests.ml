(* Expression typechecking tests*)
(* Tests function type_of that does all the work in the typechecking of expressions*)

(* Tests for Add, Sub, and Mul *)
let add_0 = type_of empty_context (Add (Num 1, Num 2)) (* Shoul return Some IntTy *)
let add_1 = type_of empty_context (Add (Num 2, Vec (1::[]))) (* Shoul return Some VecTy *)
let add_2 = type_of empty_context (Add (Vec (1::[]), Vec (2::[]))) (* Shoul return Some VecTy *)
let add_3 = type_of empty_context (Add (Num 1, Add (Num 2, Vec (1::[])))) (* Shoul return Some VecTy *)
let add_4 = type_of empty_context (Add (Bool true, Num 1)) (* Should return None *)
let add_5 = type_of empty_context (Add(Add(Num 1, Num 0), Add(Vec (1::[]), Bool false))) (* Should return None *)

(* Tests for Or and And *)
let and_0 = type_of empty_context (And(Bool true, Eq(Bool true, Bool false))) (* Shoul return Some BoolTy *)
let and_1 = type_of empty_context (And (Vec (1::[]), Vec (2::[]))) (* Shoul return None *)

(* Tests for Eq *)
let eq_0 = type_of empty_context (Eq (Num 1, Num 2)) (* Shoul return Some BoolTy *)
let eq_1 = type_of empty_context (Eq (Bool true, Eq(Bool true, Bool false))) (* Shoul return Some BoolTy *)
let eq_2 = type_of empty_context (Eq (Add(Vec (1::[]), Vec (0::[])), Vec(1::[]))) (* Shoul return Some BoolTy *)
let eq_3 = type_of empty_context (Eq (Num 1, Bool true)) (* Shoul return None *)
let eq_4 = type_of empty_context (Eq (Num 1, Vec (1::[]))) (* Should return None *)
let eq_5 = type_of empty_context (Eq (Vec (1::[]), Bool true)) (* Should return None *)

(* Tests for Var *)
let ctx = update_context empty_context "x" IntTy
let var_0 = type_of ctx (Var "x") (* Shoul return Some IntTy *)
let var_1 = type_of ctx (Var "y") (* Shoul return None *)

(* Simple test for type_of_exps *)
let ctx = update_context empty_context "x" IntTy
let type_of_exps_test = type_of_exps ctx ((Var "x")::(Bool true)::(Vec (1::[]))::[]) (* Shoul return Some [IntTy; BoolTy; VecTy] *)