(* Tests function eval_exp that evaluates all the expressions *)

(* Tests for Add *)
let st = update_state empty_state "x" (Val(IntVal(1)))

let add_0 = eval_exp (Add(Num 1, Num 2)) empty_state (* Should return Some IntVal 3 *)
let add_1 = eval_exp (Add(Num 1, Vec (1::2::[]))) empty_state (* Should return Some VecVal [2;3] *)
let add_2 = eval_exp (Add(Vec (1::2::[]), Num 1)) empty_state (* Should return Some VecVal [2;3] *)
let add_3 = eval_exp (Add(Vec (2::1::[]), Vec (1::2::[]))) empty_state (* Should return Some VecVal [3;3] *)
let add_4 = eval_exp (Add(Add (Num 1, Num 1), Vec (1::2::[]))) empty_state (* Should return Some VecVal [3;4] *)
let add_5 = eval_exp (Add(Bool true, Vec (1::2::[]))) empty_state (* Should return None *)
let add_6 = eval_exp (Add(Num 1, Var "x")) st (* Should return Some IntVal 2 *)
let add_7 = eval_exp (Add(Vec (1::2::[]), Vec (1::[]))) empty_state (* Should return None *)



(* Tests for Sub *)
let st = update_state empty_state "x" (Val(IntVal(1)))

let sub_0 = eval_exp (Sub(Num 2, Num 2)) empty_state (* Should return Some IntVal 0 *)
let sub_1 = eval_exp (Sub(Num 1, Vec (2::2::[]))) empty_state (* Should return Some VecVal [1;1] *)
let sub_2 = eval_exp (Sub(Vec (1::2::[]), Num 1)) empty_state (* Should return Some VecVal [0;1] *)
let sub_3 = eval_exp (Sub(Vec (2::1::[]), Vec (1::2::[]))) empty_state (* Should return Some VecVal [1;-1] *)
let sub_4 = eval_exp (Sub(Add (Num 1, Num 1), Vec (1::2::[]))) empty_state (* Should return Some VecVal [-1;0] *)
let sub_5 = eval_exp (Sub(Bool true, Vec (1::2::[]))) empty_state (* Should return None *)
let sub_6 = eval_exp (Sub(Num 1, Var "x")) st (* Should return Some IntVal 0 *)
let sub_7 = eval_exp (Sub(Vec (1::2::[]), Vec (1::[]))) empty_state (* Should return None *)


(* Tests for Mul *)
let st = update_state empty_state "x" (Val(IntVal(1)))
let mul_0 = eval_exp (Mul(Num 1, Num 2)) empty_state (* Should return Some IntVal 2 *)
let mul_1 = eval_exp (Mul(Num 1, Vec (1::2::[]))) empty_state (* Should return Some VecVal [1;2] *)
let mul_2 = eval_exp (Mul(Vec (1::2::[]), Num 1)) empty_state (* Should return Some VecVal [1;2] *)
let mul_3 = eval_exp (Mul(Vec (2::1::[]), Vec (1::2::[]))) empty_state (* Should return Some VecVal [2;2] *)
let mul_4 = eval_exp (Mul(Add (Num 1, Num 1), Vec (1::2::[]))) empty_state (* Should return Some VecVal [2;4] *)
let mul_5 = eval_exp (Mul(Bool true, Vec (1::2::[]))) empty_state (* Should return None *)
let mul_6 = eval_exp (Mul(Num 1, Var "x")) st (* Should return Some IntVal 1 *)
let mul_7 = eval_exp (Mul(Vec (1::2::[]), Vec (1::[]))) empty_state (* Should return None *)


(* Tests for Eq *)
let st = update_state empty_state "x" (Val(IntVal(1)))
let eq_0 = eval_exp (Eq(Num 1, Num 2)) empty_state (* Should return Some BoolVal false *)
let eq_1 = eval_exp (Eq(Num 2, Num 2)) empty_state (* Should return Some BoolVal true *)
let eq_2 = eval_exp (Eq(Vec(2::[]), Num 2)) empty_state (* Should return Some BoolVal false *)
let eq_3 = eval_exp (Eq(Num 1, Var "y")) st (* Should return None *)
let eq_4 = eval_exp (Eq(Num 1, Var "x")) st (* Should return Some BoolVal true *)


(* Tests for And *)
let st = update_state empty_state "x" (Val(BoolVal(true)))

let and_0 = eval_exp (And(Bool true, Bool false)) empty_state (* Should return Some BoolVal false *)
let and_1 = eval_exp (And(Bool true, Bool true)) empty_state (* Should return Some BoolVal true *)
let and_2 = eval_exp (And(Bool true, Eq (Num 1, Num 2))) empty_state (* Should return Some BoolVal false *)
let and_3 = eval_exp (And(Num 1, Bool false)) st (* Should return None *)
let and_4 = eval_exp (And(Bool true, Var "x")) st (* Should return Some BoolVal true *)


(* Tests for Or *)
let st = update_state empty_state "x" (Val(BoolVal(true)))
let or_0 = eval_exp (Or(Bool false, Bool false)) empty_state (* Should return Some BoolVal false *)
let or_1 = eval_exp (Or(Bool true, Bool false)) empty_state (* Should return Some BoolVal true *)
let or_2 = eval_exp (Or(Bool false, Eq (Num 1, Num 2))) empty_state (* Should return Some BoolVal false *)
let or_3 = eval_exp (Or(Num 1, Bool false)) st (* Should return None *)
let or_4 = eval_exp (Or(Bool false, Var "x")) st (* Should return Some BoolVal true *)
