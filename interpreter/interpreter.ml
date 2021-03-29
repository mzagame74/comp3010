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

(* returns a list of free variables in an expression *)
let rec free_variables (e : exp) = match e with
    | If(e1, e2, e3) -> (free_variables e1) @ (free_variables e2) @
        (free_variables e3)
    | IsZero(e') -> free_variables e'
    | Plus(Num(n), e') | Plus(e', Num(n)) -> free_variables e'
    | Mult(Num(n), e') | Mult(e', Num(n)) -> free_variables e'
    | Apply(e', Num(n)) -> free_variables e'
    | Plus(e1, e2) | Mult(e1, e2) | Apply(e1, e2) -> (free_variables e1) @
        (free_variables e2)
    | Var(str) -> [str]
    | Lambda(str, t, e') -> List.filter (fun x -> x <> str) (free_variables e')
    | _ -> raise Substitution_error

(* replaces all free variables "x" in e1 with e2 *)
let rec substitution (e1 : exp) (x : string) (e2 : exp) = match e1 with
    | True -> True
    | False -> False
    | Num(n) -> Num(n)
    | If(e1', e2', e3') -> If(substitution e1' x e2, substitution e2' x e2,
        substitution e3' x e2)
    | IsZero(e) -> IsZero(substitution e x e2)
    | Plus(e1', e2') -> Plus(substitution e1' x e2, substitution e2' x e2)
    | Mult(e1', e2') -> Mult(substitution e1' x e2, substitution e2' x e2)
    | Var(str) -> if str = x then e2 else e1
    | Lambda(str, t, e) -> (if str = x then e1
        else Lambda(str, t, substitution e x e2))
    | Apply(e1', e2') -> Apply(substitution e1' x e2, e2')

let rec step (e : exp) = match e with
    | If(True, e2, e3) -> e2
    | If(False, e2, e3) -> e3
    | If(e1, e2, e3) -> If(step e1, e2, e3)
    | IsZero(Num(0)) -> True
    | IsZero(Num(n)) when n != 0 -> False
    | IsZero(e') -> IsZero(step e')
    | Plus(Num(n1), Num(n2)) -> Num(n1 + n2)
    | Plus(Num(n), e2) -> Plus(Num(n), step e2)
    | Plus(e1, e2) -> Plus(step e1, e2)
    | Mult(Num(n1), Num(n2)) -> Num(n1 * n2)
    | Mult(Num(n), e2) -> Mult(Num(n), step e2)
    | Mult(e1, e2) -> Mult(step e1, e2)
    | Apply(Var(str), True) -> substitution (Var(str)) str True
    | Apply(Var(str), False) -> substitution (Var(str)) str False
    | Apply(Var(str), Num(n)) -> substitution (Var(str)) str (Num(n))
    | Apply(Var(str1), Lambda(str2, t, e')) ->
        substitution (Var(str1)) str1 (Lambda(str2, t, e'))
    | Apply(Var(str), e2) -> Apply(Var(str), step e2)
    | Apply(Lambda(str, t, e'), True) -> substitution e' str True
    | Apply(Lambda(str, t, e'), False) -> substitution e' str False
    | Apply(Lambda(str, t, e'), Num(n)) -> substitution e' str (Num(n))
    | Apply(Lambda(str1, t1, e1), Lambda(str2, t2, e2)) ->
        substitution e1 str1 (Lambda(str2, t2, e2))
    | Apply(Lambda(str, t, e'), e2) -> Apply(Lambda(str, t, e'), step e2)
    | Apply(e1, e2) -> Apply(step e1, e2)
    | _ -> raise Eval_error

let rec multi_step (e : exp) = match e with
    | True -> True
    | False -> False
    | Num(n) -> Num(n)
    | Lambda(str, t, e') -> Lambda(str, t, e')
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
    | Plus(e1, e2) | Mult(e1, e2) -> (match type_check te e1,
        type_check te e2 with
        | TInt, TInt -> TInt
        | _ -> raise Type_error)
    | Var(str) -> (try List.assoc str te with
        | Not_found -> raise Type_error)
    | Lambda(str, t, e') -> TArrow(t, type_check ((str, t)::te) e')
    | Apply(e1, e2) -> (match type_check te e1, type_check te e2 with
        | TArrow(t1, t2), t -> if t <> t1 then raise Type_error else t2
        | _, _ -> raise Type_error)
