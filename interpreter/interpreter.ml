exception Eval_error
exception Type_error
exception Substitution_error

type typ = 
    | TBool
    | TInt
    | TArrow of typ * typ

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

type type_environment = (string * typ) list

let rec string_of_typ (t : typ) = match t with
    | TBool -> "bool"
    | TInt -> "int"
    | TArrow(t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"

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

let rec free_variables (e : exp) = match e with
    | True -> []
    | False -> []
    | Num(n) -> []
    | If(e1, e2, e3) -> (free_variables e1) @ (free_variables e2) @
        (free_variables e3)
    | IsZero(e') -> free_variables e'
    | Plus(e1, e2) | Mult(e1, e2) | Div(e1, e2) | Apply(e1, e2) | Try(e1, e2) ->
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

let rec step (e : exp) = match e with
    | If(True, e2, e3) -> e2
    | If(False, e2, e3) -> e3
    | If(RaiseDivByZero(t, e'), e2, e3) -> RaiseDivByZero(t, e')
    | If(e1, e2, e3) -> If(step e1, e2, e3)
    | IsZero(Num(0)) -> True
    | IsZero(Num(n)) when n != 0 -> False
    | IsZero(RaiseDivByZero(t, e')) -> RaiseDivByZero(t, e')
    | IsZero(e') -> IsZero(step e')
    | Plus(Num(n1), Num(n2)) -> Num(n1 + n2)
    | Plus(Num(n), RaiseDivByZero(t, e')) -> RaiseDivByZero(t, e')
    | Plus(Num(n), e2) -> Plus(Num(n), step e2)
    | Plus(RaiseDivByZero(t, e'), e2) -> RaiseDivByZero(t, e')
    | Plus(e1, e2) -> Plus(step e1, e2)
    | Mult(Num(n1), Num(n2)) -> Num(n1 * n2)
    | Mult(Num(n), RaiseDivByZero(t, e')) -> RaiseDivByZero(t, e')
    | Mult(Num(n), e2) -> Mult(Num(n), step e2)
    | Mult(RaiseDivByZero(t, e'), e2) -> RaiseDivByZero(t, e')
    | Mult(e1, e2) -> Mult(step e1, e2)
    | Div(Num(n), Num(0)) -> RaiseDivByZero(TInt, Num(n))
    | Div(Num(n1), Num(n2)) -> Num(n1 / n2)
    | Div(Num(n), RaiseDivByZero(t, e')) -> RaiseDivByZero(t, e')
    | Div(Num(n), e2) -> Div(Num(n), step e2)
    | Div(RaiseDivByZero(t, e'), e2) -> RaiseDivByZero(t, e')
    | Div(e1, e2) -> Div(step e1, e2)
    | Apply(Var(str), True) -> substitution (Var(str)) str True
    | Apply(Var(str), False) -> substitution (Var(str)) str False
    | Apply(Var(str), Num(n)) -> substitution (Var(str)) str (Num(n))
    | Apply(Var(str1), Lambda(str2, t, e')) ->
        substitution (Var(str1)) str1 (Lambda(str2, t, e'))
    | Apply(Var(str), LambdaRec(f, t1, t2, x, e')) ->
        substitution (Var(str)) str (LambdaRec(f, t1, t2, x, e'))
    | Apply(Var(str), RaiseDivByZero(t, e')) -> RaiseDivByZero(t, e')
    | Apply(Var(str), e2) -> Apply(Var(str), step e2)
    | Apply(Lambda(str, t, e'), True) -> substitution e' str True
    | Apply(Lambda(str, t, e'), False) -> substitution e' str False
    | Apply(Lambda(str, t, e'), Num(n)) -> substitution e' str (Num(n))
    | Apply(Lambda(str1, t1, e1), Lambda(str2, t2, e2)) ->
        substitution e1 str1 (Lambda(str2, t2, e2))
    | Apply(Lambda(str, t1, e1), RaiseDivByZero(t2, e2)) ->
        RaiseDivByZero(t2, e2)
    | Apply(Lambda(str, t, e'), e2) -> Apply(Lambda(str, t, e'), step e2)
    | Apply(LambdaRec(f, t1, t2, x, e'), True) ->
        (let re = substitution e' x True in
        substitution re f (LambdaRec(f, t1, t2, x, e')))
    | Apply(LambdaRec(f, t1, t2, x, e'), False) ->
        (let re = substitution e' x False in
        substitution re f (LambdaRec(f, t1, t2, x, e')))
    | Apply(LambdaRec(f, t1, t2, x, e'), Num(n)) ->
        (let re = substitution e' x (Num(n)) in
        substitution re f (LambdaRec(f, t1, t2, x, e')))
    | Apply(LambdaRec(f, t1, t2, x, e1), LambdaRec(g, t3, t4, y, e2)) ->
        (let re = substitution e1 x (LambdaRec(g, t3, t4, y, e2)) in
        substitution re f (LambdaRec(f, t1, t2, x, e1)))
    | Apply(LambdaRec(f, t1, t2, x, e1), RaiseDivByZero(t3, e2)) ->
        RaiseDivByZero(t3, e2)
    | Apply(LambdaRec(f, t1, t2, x, e'), e2) ->
        Apply(LambdaRec(f, t1, t2, x, e'), step e2)
    | Apply(RaiseDivByZero(t, e'), e2) -> RaiseDivByZero(t, e')
    | Apply(e1, e2) -> Apply(step e1, e2)
    | Try(True, e2) -> True
    | Try(False, e2) -> False
    | Try(Num(n), e2) -> Num(n)
    | Try(RaiseDivByZero(t, e'), e2) -> Apply(e2, e')
    | Try(e1, e2) -> Try(step e1, e2)
    | RaiseDivByZero(t, True) -> RaiseDivByZero(t, True)
    | RaiseDivByZero(t, False) -> RaiseDivByZero(t, False)
    | RaiseDivByZero(t, Num(n)) -> RaiseDivByZero(t, Num(n))
    | RaiseDivByZero(t, RaiseDivByZero(t', e')) -> RaiseDivByZero(t', e')
    | RaiseDivByZero(t, e') -> RaiseDivByZero(t, step e')
    | _ -> raise Eval_error

let rec multi_step (e : exp) = match e with
    | True -> True
    | False -> False
    | Num(n) -> Num(n)
    | Lambda(str, t, e') -> Lambda(str, t, e')
    | LambdaRec(f, t1, t2, x, e') -> LambdaRec(f, t1, t2, x, e')
    | RaiseDivByZero(t, Num(n)) -> RaiseDivByZero(t, Num(n))
    | e' -> multi_step(step e')

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
