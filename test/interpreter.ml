exception Eval_error
exception Type_error

type typ = 
    | TBool
    | TInt

type exp = 
    | True
    | False
    | If of exp * exp * exp
    | Num of int
    | IsZero of exp
    | Plus of exp * exp
    | Mult of exp * exp

let rec string_of_typ (t : typ) = match t with
    | TBool -> "bool"
    | TInt -> "int"

let rec string_of_exp (e : exp) = match e with
    | True -> "true"
    | False -> "false"
    | If(e1, e2, e3) -> "if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2
        ^ " else " ^ string_of_exp e3
    | Num(n) -> string_of_int n
    | IsZero(e) -> "(isZero " ^ string_of_exp e ^ ")"
    | Plus(e1, e2) -> "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
    | Mult(e1, e2) -> "(" ^ string_of_exp e1 ^ " * " ^ string_of_exp e2 ^ ")"

let rec step (e : exp) = match e with
    | If(True, e1, e2) -> e1
    | If(False, e1, e2) -> e2
    | If(e1, e2, e3) -> If(step e1, e2, e3)
    | IsZero(Num(0)) -> True
    | IsZero(Num(n)) when n != 0 -> False
    | IsZero(e) -> IsZero(step e)
    | Plus(Num(n1), Num(n2)) -> Num(n1 + n2)
    | Plus(Num(n), e) -> Plus(Num(n), step e)
    | Plus(e1, e2) -> Plus(step e1, e2)
    | Mult(Num(n1), Num(n2)) -> Num(n1 * n2)
    | Mult(Num(n), e) -> Mult(Num(n), step e)
    | Mult(e1, e2) -> Mult(step e1, e2)
    | _ -> raise Eval_error

let rec multi_step (e : exp) = match e with
    | True -> True
    | False -> False
    | Num(n) -> Num(n)
    | e' -> multi_step(step e')

let rec type_check (e : exp) = match e with
    | True | False -> TBool
    | Num(n) -> TInt
    | If(e1, e2, e3) -> (match type_check e1, type_check e2,
        type_check e3 with
        | TBool, TBool, TBool -> TBool
        | TBool, TInt, TInt -> TInt
        | _ -> raise Type_error)
    | IsZero(e') -> (match type_check e' with
        | TInt -> TBool
        | _ -> raise Type_error)
    | Plus(e1, e2) | Mult(e1, e2) -> (match type_check e1,
        type_check e2 with
        | TInt, TInt -> TInt
        | _ -> raise Type_error)
