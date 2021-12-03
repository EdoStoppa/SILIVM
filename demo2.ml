(* This is our simple demo with operations between only VecTy! *)
(* Before running the program, please execute "prj.ml", otherwise nothing will work :) *)


(* Typecheck exp*)

let gamma0 : context = empty_context
let gamma1 : context = update_context gamma0 "a" VecTy
let gamma2 : context = update_context gamma1 "b" VecTy
let gamma3 : context = update_context gamma2 "x" VecTy
let gamma4 : context = update_context gamma3 "y" VecTy

let op1 = Add(Var "x", Add(Var "x", Var "y"))
let op2 = Mul(Var "x", Sub(Var "x", Var "y"))
let op3 = Mul(Sub(Var "x", Var "y"), Add(Var "x",Var "y")) (* Mul  ( Sub ((28,60,104), (3,4,5) ), Add ( ))     *)
                                                                            (*(25,56,99)*)        (*(31,64,109)*)
let test_op1 = typecheck_exp gamma4 op1 (* Should return true*)
let test_op2 = typecheck_exp gamma4 op2 (* Should return true*)
let test_op3 = typecheck_exp gamma4 op3 (* Should return true*)


(* Typecheck cmd*)

let a = Assign("a", Vec(3::4::5::[]))
let b = Assign("b", Vec(1::2::3::[]))

let test_cmd_a = typecheck_cmd gamma4 a (* Should return true*)
let test_cmd_b = typecheck_cmd gamma4 b (* Should return true*)

let gamma5 : context = update_context gamma4 "__ret" VecTy (* Should return true*)
let return_op1 = Return(op1)
let test_return_op1 = typecheck_cmd gamma5 return_op1 (* Should return true*)
let return_op2 = Return(op2)
let test_return_op2 = typecheck_cmd gamma5 return_op2 (* Should return true*)

let return_op3 = Return(op3)
let test_return_op3 = typecheck_cmd gamma5 return_op3 (* Should return true*)

let gamma_pre_c1 : context = update_context empty_context "a" VecTy
let gamma_pre_c2 : context = update_context gamma_pre_c1 "b" VecTy
let gammac : context = update_context (update_context gamma_pre_c2 "f" (FunTy (VecTy, [VecTy; VecTy]))) "var1" VecTy
let gammacc : context = update_context (update_context gammac "g" (FunTy (VecTy, [VecTy; VecTy]))) "var2" VecTy
let gammaccc : context = update_context (update_context gammacc "h" (FunTy (VecTy, [VecTy; VecTy]))) "var3" VecTy

let c = Seq(Call ("var1", "f", [Var "a"; Var "b"]), Seq(Call ("var2", "g", [Var "var1";Var "a"]), Call ("var3", "h", [Var "var2"; Var "a"])))
let test_c = typecheck_cmd gammaccc c (* Should return true*)

(* Run of the typechecked program*)
let state0 = update_state empty_state "f" (Fun (["x"; "y"], Return (op1)))
let state1 = update_state state0 "g" (Fun (["x"; "y"], Return (op2)))
let state2 = update_state state1 "h" (Fun (["x"; "y"], Return (op3)))

let prog1 = Seq(a, Seq(b, c))

let (res_c, res_k, res_s) = run_prog prog1 state2;;

lookup_state res_s "var1";; (* Should return Some (Val (VecVal [7; 10; 13])) *)
lookup_state res_s "var2";; (* Should return Some (Val (VecVal [28; 60; 104])) *)
lookup_state res_s "var3";; (* Should return Some (Val (VecVal [775; 3584; 10791])) *)

(* #use "demo2.ml";; *)



