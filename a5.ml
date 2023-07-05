open Stdio;;
open List;;
open String;;
open SIL;;
open Hashtbl;;


type terminalval =
    | Numeric of int
    | Label of string
    | Proc of procedure
    | Arr of (int array)
 (* arrays are not really terminal values in the language but this is an artifact of the implementation *)

(* inteval function evaluates the value of an integer expression inside a statement *)
let rec inteval (store: (string, terminalval) t) (expression: expr) : expr  =
    match expression with
    | Num i -> Num i
    | Var str -> (match find store str with
                  | Numeric value -> Num value
                  | _ -> Var str)
    | AddExpr (Num l, Num r) -> Num (l + r)
    | MulExpr (Num l, Num r) -> Num (l * r)
    | NegExpr Num l -> Num (-l)
    | AddExpr (l, r) -> inteval store (AddExpr (inteval store l, inteval store r))
    | MulExpr (l, r) -> inteval store (MulExpr (inteval store l, inteval store r))
    | NegExpr expression -> inteval store (NegExpr (inteval store expression))
    | ArrIndexExpr (Var name, index) -> let label = (match (find store name) with | Label label -> label) in
        Num (Array.get (match (find store label) with | Arr arr -> arr) (match (inteval store index) with | Num v -> v))

let rec booleval (store: (string, terminalval) t) (expression: boolExpr) : boolExpr =
    match expression with
    | True -> True
    | False -> False
    | Not True -> False
    | Not False -> True
    | And (True, True) -> True
    | And (True, False) -> False
    | And (False, True) -> False
    | And (False, False) -> False
    | Or (False, False) -> False
    | Or (True, False) -> True
    | Or (False, True) -> True
    | Or (True, True) -> True
    | Gt (Num l, Num r) -> if l > r then True else False
    | Lt (Num l, Num r) -> if l < r then True else False
    | Eq (Num l, Num r) -> if l = r then True else False
    | And (l, r) -> booleval store (And (booleval store l, booleval store r))
    | Or (l, r) -> booleval store (Or (booleval store l, booleval store r))
    | Gt (l, r) -> booleval store (Gt (inteval store l, inteval store r))
    | Lt (l, r) -> booleval store (Lt (inteval store l, inteval store r))
    | Eq (l, r) -> booleval store (Eq (inteval store l, inteval store r))

let simplifyargs (store: (string, terminalval) t) (args: expr list) : expr list =
    List.map (fun arg -> (inteval store arg)) args

let rec unwraprecursive (freshnames: string list) (args: expr list) : statement list =
    match (freshnames, args) with
    | ([], []) -> []
    | (head1 :: rest1, head2 :: rest2) -> List.append [(VarAssgStmt (head1, head2))] (unwraprecursive rest1 rest2)

let unwrap (proc: procedure) (args: expr list) : statement list =
    match proc with
    | (_, params, decls, body) ->
        let (freshnames, freshstatements) = freshenProcedure params decls body in
            List.append (unwraprecursive freshnames args) freshstatements

let getlabelfromid (labelid: int) : string =
   "l" ^ (Printf.sprintf "%d" labelid)

let getnewarray (arr: int array) (index: expr) (expression: expr) : int array =
    Array.mapi (fun i n -> if i = (match index with | Num index -> index) then (match expression with | Num value -> value) else n) arr

let rec step (store: (string, terminalval) t) (stmtlst: statement list) (labelid: int)  =
    match stmtlst with
    | [Skip] -> ()
    | Skip :: rest -> step store rest labelid
    | ((Block lst)) :: rest -> step store (List.append lst rest) labelid
    | (VarAssgStmt (name, Num i)) :: rest -> add store name (Numeric i); step store (Skip :: rest) labelid
    | (VarAssgStmt (name, Var str)) :: rest -> (match find store str with
                                               | Numeric value -> step store ((VarAssgStmt (name, Num value)) :: rest) labelid
                                               | Proc proc -> add store name (Proc proc); step store (Skip :: rest) labelid
                                               | Label label -> add store name (Label label); step store (Skip :: rest) labelid)
    | (VarAssgStmt (name, expression)) :: rest -> step store ((VarAssgStmt (name, (inteval store expression))) :: rest) labelid
    | (IfStmt (True, thenstmt, _)) :: rest -> step store (thenstmt :: rest) labelid
    | (IfStmt (False, _, elsestmt)) :: rest -> step store (elsestmt :: rest) labelid
    | (IfStmt (cond, thenstmt, elsestmt)) :: rest -> step store ((IfStmt (booleval store cond, thenstmt, elsestmt)) :: rest) labelid
    | (WhileStmt (cond, stmt)) :: rest -> step store ((IfStmt (cond, Block (List.append [stmt] [WhileStmt (cond, stmt)]), Skip)) :: rest) labelid
    | (PrintStmt (Num i)) :: rest -> Printf.printf "%d\n" i; step store (Skip :: rest) labelid
    | (PrintStmt expression) :: rest -> step store ((PrintStmt (inteval store expression)) :: rest) labelid
    | (ProcDecl (procname, args, decls, body)) :: rest -> add store procname (Proc (procname, args, decls, body)); step store (Skip :: rest) labelid
    | (CallStmt (name, args)) :: rest -> (match find store name with
                                         | Proc proc -> let resolvedargs = simplifyargs store args in
                                           step store (List.append [ Block (unwrap proc resolvedargs)] rest)  labelid)
    | (NewArrStmt (name, expression)) :: rest -> add store (getlabelfromid labelid)  (Arr (Array.make (match (inteval store expression) with | Num sz -> sz) 0));
                                         add store name (Label (getlabelfromid labelid)); step store (Skip:: rest) (labelid + 1)
    | (ArrAssgStmt (name, index, expression)) :: rest -> let label = (match find store name with | Label label -> label) in
        add store label (Arr (getnewarray (match (find store label) with | Arr arr -> arr) (inteval store index) (inteval store expression)));
        step store (Skip :: rest) labelid

(* Note that we do a trick where we embed the heap inside the store at the key "0"
which does not occur as an identifier in our language so no collision happens. *)
let run (stmtlst: statement list)  =
    let (store, labelid)  = (Hashtbl.create 10, 0)  in step store stmtlst labelid
