open Core

type sExpr =
  | Atom of string
  | List of sExpr list

type expr =
  | Num of float
  | Var of string
  | Op1 of string*expr
  | Op2 of string*expr*expr
  | Fct of string * expr list

type statement =
  | Assign of string*expr
  | Expr of expr
  | If of expr*statement list * statement list
  | While of expr*statement list
  | For of statement*expr*statement*statement list
  | FctDef of string * string list * statement list 
  | Return of expr
  | Break
  | Continue

(*type stateTypes =*)
  (*| Normal of statement*)
  (*| Break*)
  (*| Continue*)
  (*| Return of expr*)

type block =
  statement list

let rec print_block = function
  | [] -> printf "\n"
  | Assign(_,_)::l -> printf "Assign "; print_block l
  | Expr(_)::l -> printf "Expression "; print_block l
  | If(_,_,_)::l -> printf "If "; print_block l
  | While(_,_)::l -> printf "While "; print_block l
  | For(_,_,_,_)::l -> printf "For"; print_block l
  | FctDef(_,_,_)::l -> printf "FctDef"; print_block l
  | Return(_)::l -> printf "Return"; print_block l
  | _::l -> printf "Special"; print_block l

type env =
  | N of float
  | V of string * float
  | F of string * string list * statement list

type scope =
  env list

let rec print_scope = function 
  [] -> print_endline ""
  | N(ret)::l -> printf "RETURN = %F " ret ; print_scope l
  | V(id, value)::l -> printf "var %S = %F " id value ; print_scope l
  | F(id, _, _)::l -> printf "fun %S " id ; print_scope l
  

type envQueue =
  scope list

let print_prgrm (q:envQueue) = 
  let rec loop i = function
    [] -> ()
    | crntScope::l -> (printf "Stack: %i " i); (print_scope crntScope); (loop (i+1) l)
  in 
  loop 0 q

let rec varEval (_v: string) (_q:envQueue): float =
  match _q with
  | [] -> 0.0 (*Doesn't exist NEEDS TESTS*)
  | crntScope::prgrm -> 
      let var = 
        List.find crntScope ~f:(fun s -> match s with V(v, _) when v = _v -> true | _->false ) 
      in
      match var with
      | Some V (_, value) -> value
      | _ -> varEval(_v) (prgrm)

let rec fctEval (_id: string) (_q: envQueue): (string list)*(statement list) =
  match _q with
  | [] -> ([],[]) (*Doesn't exist NEEDS TESTS*)
  | crntScope::prgrm -> 
      let var = 
        List.find (crntScope) 
          ~f:(fun s -> match s with F(id, _, _) when id = _id -> true | _->false ) 
      in
      match var with
      | Some F (_, param, def) -> (param, def)
      | _ -> fctEval(_id) (prgrm)

let bind (e:env) (s:scope): scope = 
  match e with
  | V(id, _) as var ->
    (match
      List.findi (s) 
        ~f:(fun _ a -> 
          match a with 
          | V(_id,_) when _id = id -> true 
          | _ -> false) 
      with
      | Some(index, _) -> (*printf "Mutating %S = %F @ %i" id value index*)
          List.mapi (s) 
            ~f:(fun i a -> if i = index then var else a)
      | None -> var::s)
  | F(id, _, _) as fn -> 
      (match
        List.findi (s) 
        ~f:(fun _ a -> 
          match a with
          | F(_id, _params, _def) when _id = id -> true 
          | _ -> false)
      with 
      | Some(index, _) -> 
          List.mapi (s) 
            ~f:(fun i a -> if i = index then fn else a)
      | None -> fn::s)
  | _ -> s
let rec evalExpr (_e: expr) (_q:envQueue): float  =
  match _e with
  | Num n -> n

  (*Retrieve value associated with id in local env*)
  | Var id -> (varEval id _q)

  (* See evalStatement Expr pattern for op1 *)
  (*| Op1(op, a) when op = "++a"  ->  (evalExpr a _q) +. 1.*)
  (*| Op1(op, a) when op = "--a"  ->  (evalExpr a _q) -. 1.*)

  (*Binary ops*)
  | Op2(op, a, b) when op = "*"-> (evalExpr a _q ) *. (evalExpr b _q)
  | Op2(op, a, b) when op = "/"-> (evalExpr a _q ) /. (evalExpr b _q)
  | Op2(op, a, b) when op = "+"-> (evalExpr a _q ) +. (evalExpr b _q)
  | Op2(op, a, b) when op = "-"-> (evalExpr a _q ) -. (evalExpr b _q)

  (* Relational *)
  | Op2(op, a, b) when op = ">"->  if(evalExpr a _q > evalExpr b _q)  then 1.0 else 0.0
  | Op2(op, a, b) when op = "<"->  if(evalExpr a _q < evalExpr b _q)  then 1.0 else 0.0
  | Op2(op, a, b) when op = ">="-> if(evalExpr a _q >= evalExpr b _q) then 1.0 else 0.0
  | Op2(op, a, b) when op = "<="-> if(evalExpr a _q <= evalExpr b _q) then 1.0 else 0.0
  | Op2(op, a, b) when op = "=="-> if(evalExpr a _q = evalExpr b _q)  then 1.0 else 0.0
  (*Function call*)
  | Fct(id, args) -> (
      let (params, def) = 
        fctEval(id) (_q) 
      in
      let argAssign = 
        List.map2 ~f:(fun param arg -> 
          Assign(param,arg))(params)(args) 
      in 
      let fnScope = match argAssign with
        | Ok(a)-> (List.fold_left ~f:(evalStatement) ~init:([]) (a))
        | Unequal_lengths -> [] (*raise error!*)
      in
      let rec fctCall (queue: envQueue) (stmntList: statement list): float=
        match stmntList with
        | [] -> 0.0
        | Return(e)::_ -> evalExpr (e) (queue)
        | stmnt::tl -> fctCall (evalStatement (queue)(stmnt)) (tl)
      in
      fctCall (fnScope @ _q) (def))
  | _ -> 0.0

and evalStatement (q: envQueue) (s: statement) : envQueue = 
  match s with 
  | Expr(e) -> (
      match e with
      | Op1(op, Var(id)) when op = "++a"  -> 
          let incremented = 
            (evalExpr (Var(id))  q) +. 1.
          in
          evalStatement (q) (Assign (id, Num(incremented)))
      | Op1(op, Var(id)) when op = "--a"  -> 
          let decremented = 
            (evalExpr (Var(id))  q) +. 1.
          in
          evalStatement (q) (Assign (id, Num(decremented)))
      | expr -> printf "%F\n" (evalExpr (expr) (q)); q
  )
  | Assign(v, e) ->
      let value = 
        evalExpr e q 
      in
      (match q with
      | local::prgm -> bind(V(v, value)) (local)::prgm
      | [] -> bind(V(v, value)) ([])::[])
  | FctDef(id, params, def) -> 
      (match q with
      | local::prgm -> bind(F(id, params, def)) (local)::prgm
      | [] -> bind(F(id, params, def)) ([])::[])
  | If(cond, codeT, codeF) ->
      if(evalExpr(cond) (q) > 0.0) 
        then (List.fold_left ~f:(evalStatement) ~init:(q) (codeT))
        else (List.fold_left ~f:(evalStatement) ~init:(q) (codeF))
  | While(cond, block) ->
      let rec loop (queue:envQueue) (cond:expr) =
        if(evalExpr(cond) (queue) > 0.0)
          then let newQueue = 
            (List.fold_left ~f:(evalStatement) ~init:(queue) (block))
          in 
          loop newQueue cond
        else 
          queue
      in 
      loop (q) (cond)
  | For(init, cond, update, block) ->
      let initScope =  
        evalStatement (q) (init) 
      in
      let rec loop (queue: envQueue) (cond: expr) =
        if(evalExpr(cond) (queue) > 0.0)
          then let newQueue = 
            (List.fold_left ~f:(evalStatement) ~init:(queue) (block))
          in 
          let evalUpdate =
            evalStatement (newQueue) (update)
          in
          loop (evalUpdate) (cond)
        else 
          queue
      in
      loop (initScope) (cond)
  | _ -> q

let evalCode (stmntList: block) (q: envQueue): unit = 
  let s: scope = 
    [] 
  in let _ = 
    List.fold_left ~f:(evalStatement) ~init:(s::q) (stmntList)
  in ()

(* EXPRESSION TESTS *)
let%expect_test "evalNum" = 
  evalExpr (Num 10.0) ([])
  |> printf "%F";
  [%expect {| 10. |}]
let%expect_test "eval var" = 
  evalExpr (Var "var0") [[V("var0", 10.0)]; [V("var1", -10.0); F("dontcall", [],[])]]
  |> printf "%F"; 
  [%expect {| 10. |}]
let%expect_test "eval binary" = 
  evalExpr (Op2 ("+", Num(9.0), Num(1.0))) ([])
  |> printf "%F";
  [%expect {| 10. |}]
let%expect_test "eval fct" = 
  evalExpr 
    (Fct ("callme", [])) 
    [[V("var0", 10.0)]; [V("var1", -10.0); F("callme",[],[Return(Num(10.))])]]
  |> printf "%F"; 
  [%expect {| 10. |}]
let%expect_test "eval fct outside of scope" = 
  evalExpr 
    (Fct ("callme", [])) 
    [[]; [];[V("var0", 10.0)]; [V("var1", -10.0); F("callme",[],[Return(Num(10.))])]]
  |> printf "%F"; 
  [%expect {| 10. |}]

(*Statement tests*)
let%expect_test "eval var" = 
  let fnStored = evalStatement
    ([])
    (FctDef("callme", [], [Return (Num 10.) ]))
  in
  evalExpr (Fct ("callme", [])) fnStored
  |> printf "%F"; 
  [%expect {| 10. |}]

(* END EXPRESSION TESTS *)
let a0: block = [
  Assign("v", Num(1.0));
  Expr(Var("v")) 
]

let%expect_test "a0" =
  evalCode a0 []; 
  [%expect {| 1. |}]

let a1: block = [
  Assign("v", Num(1.0));
  Assign("a", Num(2.0));
  Expr(Var("v"));
  Expr(Var("a"))
]

let%expect_test "a1" =
  evalCode a1 []; 
  [%expect {| 
            1. 
            2.|}]
(*
    v = 1.0;
    if (v>10.0) then
        v = v + 1.0
    else
        for(i=2.0; i<10.0; i++) {
            v = v * i
        }
    v   // display v
*)

let p2: block = [
    Assign("v", Num(1.0));
    If(
      Op2(">", Var("v"), Num(10.0)), 
        [Assign("v", Op2("+", Var("v"), Num(1.0)))], 
        [For(
            Assign("i", Num(2.0)),
            Op2("<", Var("i"), Num(10.0)),
            Expr(Op1("++a", Var("i"))),
            [
              Assign("v", Op2("*", Var("v"), Var("i")));
            ])]
    );
    Expr(Var("v"))
]

let%expect_test "p2" =
    evalCode p2 []; 
    [%expect {| 362880. |}]

(* Fibonacci sequence
define f(x) {
  if (x<2.0) then
    return (1.0)
  else
    return (f(x-1)+f(x-2))
}
f(3)
f(5)

*)


let p3: block =
 [
   FctDef("f", ["x"], [
     If( Op2("<", Var("x"), Num(2.0)), 
      (*then*) 
      [Return(Num(1.0))],
      (*else*) 
      [Return(Op2("+",Fct("f", [Op2("-", Var("x"), Num(1.0))]), Fct("f", [Op2("-", Var("x"), Num(2.0))])))]
    )
   ]);
 Expr(Fct("f", [Num(3.0)]));
 Expr(Fct("f", [Num(5.0)]));
 ]

let%expect_test "p3" =
  evalCode p3 [];
  [%expect {|
  3.
  8.
  |}]

let p4: block =
 [
   FctDef("f", ["x"], [ 
     Return(Num(1.0))
   ]);
   Expr(Op2("+", Fct("f", []), Fct("f", [])));
]

let%expect_test "p4" =
  evalCode p4 [];
  [%expect {|
  2.
  |}]


