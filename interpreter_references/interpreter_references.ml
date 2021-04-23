exception Eval_error
exception Type_error
exception Substitution_error

type typ = 
    | TBool
    | TInt
    | TArrow of typ * typ
    | TRef of typ
    | TUnit

type exp = 
    | True
    | False
    | If of exp * exp * exp
    | Num of int
    | IsZero of exp
    | Plus of exp * exp
    | Mult of exp * exp
    | Var of string
    | Lambda of string * typ * exp
    | Apply of exp * exp
    | LambdaRec of string * typ * typ * string * exp
    | Div of exp * exp
    | Try of exp * exp
    | RaiseDivByZero of typ * exp
    | Label of int
    | Malloc of exp
    | Mread of exp
    | Assign of exp * exp
    | Sequence of exp * exp
    | Unit

type type_environment = (string * typ) list
type memory = (int * exp) list

let rec string_of_typ (t : typ) = match t with
    | TBool -> "bool"
    | TInt -> "int"
    | TArrow(t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
    | TRef(t) -> "(" ^ string_of_typ t ^ " ref)"
    | TUnit -> "()"

let rec string_of_exp (e : exp) = match e with
    | True -> "true"
    | False -> "false"
    | If(e1, e2, e3) -> "if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2
        ^ " else " ^ string_of_exp e3
    | Num(n) -> string_of_int n
    | IsZero(e') -> "(isZero " ^ string_of_exp e' ^ ")"
    | Plus(e1, e2) -> "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
    | Mult(e1, e2) -> "(" ^ string_of_exp e1 ^ " * " ^ string_of_exp e2 ^ ")"
    | Var(str) -> str
    | Lambda(str, t, e') -> "(lambda " ^ str ^ ":" ^ string_of_typ t ^ "." ^
        string_of_exp e' ^ ")"
    | Apply(e1, e2) -> "(" ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ")"
    | LambdaRec(f, t1, t2, x, e') -> "(lambdaRec (" ^ f ^ " : " ^
        string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ " ) " ^ x ^ " = " ^
        string_of_exp e' ^ ")"
    | Div(e1, e2) -> "(" ^ string_of_exp e1 ^ " / " ^ string_of_exp e2 ^ ")"
    | Try(e1, e2) -> "try " ^ string_of_exp e1 ^ " with " ^ string_of_exp e2
    | RaiseDivByZero(t, e) -> "(raiseDivByZeroError " ^ string_of_typ t ^ " " ^
        string_of_exp e ^ ")"
    | Label(n) -> "(label " ^ string_of_int n ^ ")"
    | Malloc(e') -> "(malloc " ^ string_of_exp e' ^ ")"
    | Mread(e') -> "(mread " ^ string_of_exp e' ^ ")"
    | Assign(e1, e2) -> string_of_exp e1 ^ ":= " ^ string_of_exp e2
    | Sequence(e1, e2) -> string_of_exp e1 ^ ";\n" ^ string_of_exp e2
    | Unit -> "()"

let rec free_variables (e : exp) = match e with
    | True | False | Unit -> []
    | Num(n) | Label(n) -> []
    | If(e1, e2, e3) -> (free_variables e1) @ (free_variables e2) @
        (free_variables e3)
    | IsZero(e') | Malloc(e') | Mread(e') -> free_variables e'
    | Plus(e1, e2) | Mult(e1, e2) | Div(e1, e2) | Apply(e1, e2) | Try(e1, e2)
    | Assign(e1, e2) | Sequence(e1, e2) -> 
        (free_variables e1) @ (free_variables e2)
    | Var(str) -> [str]
    | Lambda(str, t, e') -> List.filter (fun x -> x <> str) (free_variables e')
    | LambdaRec(f, t1, t2, x, e') -> List.filter (fun y -> y <> f && y <> x)
        (free_variables e')
    | RaiseDivByZero(t, e') -> free_variables e'

let rec substitution (e1 : exp) (x : string) (e2 : exp) = match e1 with
    | True -> True
    | False -> False
    | Num(n) -> Num(n)
    | If(e1', e2', e3') -> If(substitution e1' x e2, substitution e2' x e2,
        substitution e3' x e2)
    | IsZero(e) -> IsZero(substitution e x e2)
    | Plus(e1', e2') -> Plus(substitution e1' x e2, substitution e2' x e2)
    | Mult(e1', e2') -> Mult(substitution e1' x e2, substitution e2' x e2)
    | Div(e1', e2') -> Div(substitution e1' x e2, substitution e2' x e2)
    | Var(str) -> if str = x then e2 else e1
    | Lambda(str, t, e) -> (if str = x then e1
        else Lambda(str, t, substitution e x e2))
    | Apply(e1', e2') -> Apply(substitution e1' x e2, substitution e2' x e2)
    | LambdaRec(f, t1, t2, x', e) -> (if x' = x || f = x then e1
        else if let fv = free_variables e2 in
        x' <> x && f <> x && not (List.mem x' fv) && not (List.mem f fv) then
            LambdaRec(f, t1, t2, x', substitution e x e2)
        else raise Substitution_error)
    | Try(e1', e2') -> Try(substitution e1' x e2, substitution e2' x e2)
    | RaiseDivByZero(t, e) -> RaiseDivByZero(t, substitution e x e2)
    | Label(n) -> Label(n)
    | Malloc(e) -> Malloc(substitution e x e2)
    | Mread(e) -> Mread(substitution e x e2)
    | Assign(e1', e2') -> Assign(substitution e1' x e2, substitution e2' x e2)
    | Sequence(e1', e2') ->
        Sequence(substitution e1' x e2, substitution e2' x e2)
    | Unit -> Unit

let rec step (e : exp) (m : memory) : (exp * memory) = match e with
    | If(True, e2, e3) -> e2, m
    | If(False, e2, e3) -> e3, m
    | If(RaiseDivByZero(t, e'), e2, e3) -> RaiseDivByZero(t, e'), m
    | If(e1, e2, e3) -> If(step e1 m |> fst, e2, e3), m
    | IsZero(Num(0)) -> True, m
    | IsZero(Num(n)) -> False, m
    | IsZero(RaiseDivByZero(t, e')) -> RaiseDivByZero(t, e'), m
    | IsZero(e') -> IsZero(step e' m |> fst), m
    | Plus(Num(n1), Num(n2)) -> Num(n1 + n2), m
    | Plus(Num(n), RaiseDivByZero(t, e')) -> RaiseDivByZero(t, e'), m
    | Plus(Num(n), e2) -> Plus(Num(n), step e2 m |> fst), m
    | Plus(RaiseDivByZero(t, e'), e2) -> RaiseDivByZero(t, e'), m
    | Plus(e1, e2) -> Plus(step e1 m |> fst, e2), m
    | Mult(Num(n1), Num(n2)) -> Num(n1 * n2), m
    | Mult(Num(n), RaiseDivByZero(t, e')) -> RaiseDivByZero(t, e'), m
    | Mult(Num(n), e2) -> Mult(Num(n), step e2 m |> fst), m
    | Mult(RaiseDivByZero(t, e'), e2) -> RaiseDivByZero(t, e'), m
    | Mult(e1, e2) -> Mult(step e1 m |> fst, e2), m
    | Div(Num(n), Num(0)) -> RaiseDivByZero(TInt, Num(n)), m
    | Div(Num(n1), Num(n2)) -> Num(n1 / n2), m
    | Div(Num(n), RaiseDivByZero(t, e')) -> RaiseDivByZero(t, e'), m
    | Div(Num(n), e2) -> Div(Num(n), step e2 m |> fst), m
    | Div(RaiseDivByZero(t, e'), e2) -> RaiseDivByZero(t, e'), m
    | Div(e1, e2) -> Div(step e1 m |> fst, e2), m
    | Apply(Var(str), True) -> substitution (Var(str)) str True, m
    | Apply(Var(str), False) -> substitution (Var(str)) str False, m
    | Apply(Var(str), Num(n)) -> substitution (Var(str)) str (Num(n)), m
    | Apply(Var(str1), Lambda(str2, t, e')) ->
        substitution (Var(str1)) str1 (Lambda(str2, t, e')), m
    | Apply(Var(str), LambdaRec(f, t1, t2, x, e')) ->
        substitution (Var(str)) str (LambdaRec(f, t1, t2, x, e')), m
    | Apply(Var(str), RaiseDivByZero(t, e')) -> RaiseDivByZero(t, e'), m
    | Apply(Var(str), e2) -> Apply(Var(str), step e2 m |> fst), m
    | Apply(Lambda(str, t, e'), True) -> substitution e' str True, m
    | Apply(Lambda(str, t, e'), False) -> substitution e' str False, m
    | Apply(Lambda(str, t, e'), Num(n)) -> substitution e' str (Num(n)), m
    | Apply(Lambda(str1, t1, e1), Lambda(str2, t2, e2)) ->
        substitution e1 str1 (Lambda(str2, t2, e2)), m
    | Apply(Lambda(str, t1, e1), RaiseDivByZero(t2, e2)) ->
        RaiseDivByZero(t2, e2), m
    | Apply(Lambda(str, t, e'), e2) ->
        Apply(Lambda(str, t, e'), step e2 m |> fst), m
    | Apply(LambdaRec(f, t1, t2, x, e'), True) ->
        (let re = substitution e' x True in
        substitution re f (LambdaRec(f, t1, t2, x, e')), m)
    | Apply(LambdaRec(f, t1, t2, x, e'), False) ->
        (let re = substitution e' x False in
        substitution re f (LambdaRec(f, t1, t2, x, e')), m)
    | Apply(LambdaRec(f, t1, t2, x, e'), Num(n)) ->
        (let re = substitution e' x (Num(n)) in
        substitution re f (LambdaRec(f, t1, t2, x, e')), m)
    | Apply(LambdaRec(f, t1, t2, x, e1), LambdaRec(g, t3, t4, y, e2)) ->
        (let re = substitution e1 x (LambdaRec(g, t3, t4, y, e2)) in
        substitution re f (LambdaRec(f, t1, t2, x, e1)), m)
    | Apply(LambdaRec(f, t1, t2, x, e1), RaiseDivByZero(t3, e2)) ->
        RaiseDivByZero(t3, e2), m
    | Apply(LambdaRec(f, t1, t2, x, e'), e2) ->
        Apply(LambdaRec(f, t1, t2, x, e'), step e2 m |> fst), m
    | Apply(RaiseDivByZero(t, e'), e2) -> RaiseDivByZero(t, e'), m
    | Apply(e1, e2) -> Apply(step e1 m |> fst, e2), m
    | Try(True, e2) -> True, m
    | Try(False, e2) -> False, m
    | Try(Num(n), e2) -> Num(n), m
    | Try(RaiseDivByZero(t, e'), e2) -> Apply(e2, e'), m
    | Try(e1, e2) -> Try(step e1 m |> fst, e2), m
    | RaiseDivByZero(t, True) -> RaiseDivByZero(t, True), m
    | RaiseDivByZero(t, False) -> RaiseDivByZero(t, False), m
    | RaiseDivByZero(t, Num(n)) -> RaiseDivByZero(t, Num(n)), m
    | RaiseDivByZero(t, RaiseDivByZero(t', e')) -> RaiseDivByZero(t', e'), m
    | RaiseDivByZero(t, e') -> RaiseDivByZero(t, step e' m |> fst), m
    | Malloc(True) -> let ln = (List.length m) in Label(ln), (ln, True)::m
    | Malloc(False) -> let ln = (List.length m) in Label(ln), (ln, False)::m
    | Malloc(Num(n)) -> let ln = (List.length m) in Label(ln), (ln, Num(n))::m
    | Malloc(Label(n)) -> let ln = (List.length m) in
        Label(ln), (ln, Label(n))::m
    | Malloc(RaiseDivByZero(t, e')) -> RaiseDivByZero(t, e'), m
    | Malloc(e') -> let m' = step e' m in Malloc(m' |> fst), m' |> snd
    | Mread(Label(n)) -> (try (List.assoc n m), m with
        | Not_found -> raise Eval_error)
    | Mread(RaiseDivByZero(t, e')) -> RaiseDivByZero(t, e'), m
    | Mread(e') -> let m' = step e' m in Mread(m' |> fst), m' |> snd
    | Assign(Label(n), True) -> Unit, (n, True)::m
    | Assign(Label(n), False) -> Unit, (n, False)::m
    | Assign(Label(ln), Num(n)) -> Unit, (ln, Num(n))::m
    | Assign(Label(n), RaiseDivByZero(t, e')) -> RaiseDivByZero(t, e'), m
    | Assign(Label(n), e2) -> Assign(Label(n), step e2 m |> fst), m
    | Assign(RaiseDivByZero(t, e'), e2) -> RaiseDivByZero(t, e'), m
    | Assign(e1, e2) -> Assign(step e1 m |> fst, e2), m
    | Sequence(True, e2) | Sequence(False, e2) -> e2, m
    | Sequence(Num(n), e2) -> e2, m
    | Sequence(RaiseDivByZero(t, e'), e2) -> RaiseDivByZero(t, e'), m
    | Sequence(e1, e2) -> Sequence(step e1 m |> fst, e2), m
    | _ -> raise Eval_error

let rec multi_step (e : exp) (m : memory) : (exp * memory) = match e with
    | True -> True, m
    | False -> False, m
    | Num(n) -> Num(n), m
    | Lambda(str, t, e') -> Lambda(str, t, e'), m
    | LambdaRec(f, t1, t2, x, e') -> LambdaRec(f, t1, t2, x, e'), m
    | RaiseDivByZero(t, Num(n)) -> RaiseDivByZero(t, Num(n)), m
    | Label(n) -> Label(n), m
    | e' -> let m' = step e' m in multi_step (m' |> fst) (m' |> snd)

let rec type_check (te : type_environment) (e : exp) = match e with
    | True | False -> TBool
    | Num(n) -> TInt
    | If(e1, e2, e3) -> (match type_check te e1, type_check te e2,
        type_check te e3 with
        | TBool, TBool, TBool -> TBool
        | TBool, TInt, TInt -> TInt
        | _ -> raise Type_error)
    | IsZero(e') -> (match type_check te e' with
        | TInt -> TBool
        | _ -> raise Type_error)
    | Plus(e1, e2) | Mult(e1, e2) | Div(e1, e2) -> (match type_check te e1,
        type_check te e2 with
        | TInt, TInt -> TInt
        | _ -> raise Type_error)
    | Var(str) -> (try List.assoc str te with
        | Not_found -> raise Type_error)
    | Lambda(str, t, e') -> TArrow(t, type_check ((str, t)::te) e')
    | Apply(e1, e2) -> (match type_check te e1, type_check te e2 with
        | TArrow(t1, t2), t -> if t <> t1 then raise Type_error else t2
        | _, _ -> raise Type_error)
    | LambdaRec(f, t1, t2, x, e') -> let te' = ((f, TArrow(t1, t2))::te) in
        TArrow(t1, type_check ((x, t1)::te') e')
    | Try(e1, e2) -> (match type_check te e1, type_check te e2 with
        | TInt, TArrow(TInt, TInt) -> TInt
        | _ -> raise Type_error)
    | RaiseDivByZero(t, e') -> t
    | _ -> raise Type_error
