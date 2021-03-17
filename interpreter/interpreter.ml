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
    | TBool -> "TBool"
    | TInt -> "TInt"
    | TArrow(t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"

let rec string_of_exp (e : exp) = match e with
    | True -> "true"
    | False -> "false"
    | If(e1, e2, e3) -> "if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2
        ^ " else " ^ string_of_exp e3
    | Num(n) -> string_of_int n
    | IsZero(e) -> "(isZero " ^ string_of_exp e ^ ")"
    | Plus(e1, e2) -> "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
    | Mult(e1, e2) -> "(" ^ string_of_exp e1 ^ " * " ^ string_of_exp e2 ^ ")"
    | Var(str) -> str
    | Lambda(str, t, e) -> "(lambda " ^ str ^ ":" ^ string_of_typ t ^ "." ^
        string_of_exp e ^ ")"
    | Apply(e1, e2) -> "(" ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ")"

(* returns a list of free variables in an expression *)
let rec free_variables (e : exp) = match e with
    | If(e1, e2, e3) -> ((free_variables e1) @ (free_variables e2) @
        (free_variables e3))
    | IsZero(e) -> free_variables e
    | Var(str) -> [str]
    | Lambda(str, t, e) -> List.filter (fun x -> x <> str) (free_variables e)
    | Plus(e1, e2) | Mult(e1, e2) | Apply(e1, e2) -> ((free_variables e1) @
        (free_variables e2))
    | _ -> raise Substitution_error

(* replaces all free variables "x" in e1 with e2 *)
let rec substitution (e1 : exp) (x : string) (e2 : exp) = match e1 with
    | If(e1', e2', e3') -> If(substitution e1' x e2, substitution e2' x e2,
        substitution e3' x e2)
    | IsZero(e) -> (match e with
        | Var(str) -> (if str = x then IsZero(e2)
            else raise Substitution_error)
        | e' -> IsZero(substitution e' x e2))
    | Plus(e1', e2') -> (match e1', e2' with
        | Num(n), Var(str) -> (if str = x then Plus(Num(n), e2)
            else raise Substitution_error)
        | e1'', e2'' -> Plus(substitution e1'' x e2, substitution e2'' x e2))
    | Mult(e1', e2') -> (match e1', e2' with
        | Num(n), Var(str) -> (if str = x then Mult(Num(n), e2)
            else raise Substitution_error)
        | e1'', e2'' -> Mult(substitution e1'' x e2, substitution e2'' x e2))
    | Var(str) -> if str = x then e2 else e1
    | Lambda(str, t, e) -> (if str = x then e1
        else if not (List.mem str (free_variables e2)) then
            Lambda(str, t, substitution e x e2)
        else raise Substitution_error)
    | Apply(e1', e2') -> Apply(substitution e1' x e2, substitution e2' x e2)
    | _ -> raise Substitution_error

let rec step (e : exp) = match e with
    | If(e1, e2, e3) -> (match e1 with
        | True -> e2
        | False -> e3
        | e1' -> If(step e1', e2, e3))
    | IsZero(e) -> (match e with
        | Num(0) -> True
        | Num(n) when n != 0 -> False
        | e' -> IsZero(step e'))
    | Plus(e1, e2) -> (match e1, e2 with
        | Num(n1), Num(n2) -> Num(n1 + n2)
        | Num(n), e2' -> Plus(e1, step e2')
        | e1', e2' -> Plus(step e1', e2'))
    | Mult(e1, e2) -> (match e1, e2 with
        | Num(n1), Num(n2) -> Num(n1 * n2)
        | Num(n), e2' -> Mult(e1, step e2')
        | e1', e2' -> Mult(step e1', e2'))
    | Apply(e1, e2) -> (match e1, e2 with
        | Var(str), True | Var(str), False -> substitution e1 str e2
        | Var(str), Num(n) -> substitution e1 str e2
        | Var(str1), Lambda(str2, t, e) -> substitution e1 str1 e2
        | Var(str), e2' -> Apply(e1, step e2')
        | Lambda(str, t, e), True | Lambda(str, t, e), False ->
            substitution e str e2
        | Lambda(str, t, e), Num(n) -> substitution e str e2
        | Lambda(str1, t1, e1'), Lambda(str2, t2, e2') ->
            substitution e1' str1 e2'
        | Lambda(str1, t, e), e2' -> Apply(e1, step e2')
        | e1', e2' -> Apply(step e1', e2'))
    | _ -> raise Eval_error

let rec multi_step (e : exp) = match e with
    | True -> True
    | False -> False
    | Num(n) -> Num(n)
    | _ -> let e' = step e in multi_step(e')

let rec type_check (te : type_environment) (e : exp) = match multi_step e with
    | True -> TBool
    | False -> TBool
    | Num(n) -> TInt
    | _ -> raise Type_error

let () =
    print_endline("== Typed Lambda Calculus ==");
    print_endline("1) " ^ string_of_exp(substitution
       (Mult (Plus (Num 5, Var "x"), Plus (Num 3, Var "x")))
       "x" (Num 2)));
    print_endline("2) " ^ string_of_exp(substitution
       (If (IsZero (Var "y"), Var "y", Var "x"))
       "y"
       (Mult (Num 5, Var "x"))));
    print_endline("3) " ^ string_of_exp(substitution (Lambda ("x", TInt,
    Plus (Var "x", Num 5))) "x" (Num 0)));
    (*print_endline("4) " ^ string_of_exp(substitution
       (Lambda
          ( "x"
          , TBool
          , If (Var "x", Apply (Var "y", Num 0), Apply (Var "y", Num 1)) ))
       "y"
       (Lambda ("x", TInt, Plus (Var "x", Num 1)))));*)
    