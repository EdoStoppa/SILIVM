(* This is our simple demo with mixed type operations between IntTy and VecTy! *)
(* Before running the program, please execute "prj.ml", otherwise nothing will work :) *)


(* Typecheck exp*)

let gamma0 : context = empty_context
let gamma1 : context = update_context gamma0 "a" IntTy
let gamma2 : context = update_context gamma1 "b" VecTy
let gamma3 : context = update_context gamma2 "x" IntTy
let gamma4 : context = update_context gamma3 "y" VecTy
let gamma5 : context = update_context gamma2 "x" VecTy
let gamma6 : context = update_context gamma5 "y" IntTy

let add = Add(Var "x", Var "y")
let sub = Sub(Var "x", Var "y")
let mul = Mul(Var "x", Var "y")

let test_add = typecheck_exp gamma4 add (* Should return true*)
let test_sub = typecheck_exp gamma6 sub (* Should return true*)
let test_mul = typecheck_exp gamma6 mul (* Should return true*)


(* Typecheck cmd*)

let a = Assign("a", Num 17)
let b = Assign("b", Vec(3::[]))

let test_cmd_a = typecheck_cmd gamma2 a (* Should return true*)
let test_cmd_b = typecheck_cmd gamma2 b (* Should return true*)

let gamma7 : context = update_context gamma4 "__ret" VecTy (* Should return true*)
let return_add = Return(add)
let test_return_add = typecheck_cmd gamma7 return_add (* Should return true*)

let gamma8 : context = update_context gamma6 "__ret" VecTy (* Should return true*)
let return_sub = Return(sub)
let test_return_sub = typecheck_cmd gamma8 return_sub (* Should return true*)

let gamma9 : context = update_context gamma6 "__ret" VecTy (* Should return true*)
let return_mul = Return(mul)
let test_return_mul = typecheck_cmd gamma9 return_mul (* Should return true*)

let gamma_pre_c1 : context = update_context empty_context "a" IntTy
let gamma_pre_c2 : context = update_context gamma_pre_c1 "b" VecTy
let gammac : context = update_context (update_context gamma_pre_c2 "f" (FunTy (VecTy, [IntTy; VecTy]))) "var1" VecTy
let gammacc : context = update_context (update_context gammac "g" (FunTy (VecTy, [VecTy; IntTy]))) "var2" VecTy
let gammaccc : context = update_context (update_context gammacc "h" (FunTy (VecTy, [VecTy; IntTy]))) "var3" VecTy

let c = Seq(Call ("var1", "f", [Var "a"; Var "b"]), Seq(Call ("var2", "g", [Var "var1";Var "a"]), Call ("var3", "h", [Var "var2"; Var "a"])))
let test_c = typecheck_cmd gammaccc c (* Should return true*)

(* Run of the typechecked program*)
let state0 = update_state empty_state "f" (Fun (["x"; "y"], Return (add)))
let state1 = update_state state0 "g" (Fun (["x"; "y"], Return (sub)))
let state2 = update_state state1 "h" (Fun (["x"; "y"], Return (mul)))

let prog1 = Seq(a, Seq(b, c))

let (res_c, res_k, res_s) = run_prog prog1 state2;;

lookup_state res_s "var1";; (* Should return Some (Val (VecVal [20])) *)
lookup_state res_s "var2";; (* Should return Some (Val(VecVal [3])) *)
lookup_state res_s "var3";; (* Should return Some (Val (VecVal [51])) *)

(* #use "demo1.ml";; *)