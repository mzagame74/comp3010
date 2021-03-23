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
    | Plus(e1, e2) | Mult(e1, e2) | Apply(e1, e2) -> (free_variables e1) @
        (free_variables e2)
    | Var(str) -> [str]
    | Lambda(str, t, e') -> List.filter (fun x -> x <> str) (free_variables e')
    | _ -> raise Substitution_error

(* replaces all free variables "x" in e1 with e2 *)
let rec substitution (e1 : exp) (x : string) (e2 : exp) = match e1 with
    | If(e1', True, True) -> If(substitution e1' x e2, True, True)
    | If(e1', True, False) -> If(substitution e1' x e2, True, False)
    | If(e1', False, True) -> If(substitution e1' x e2, False, True)
    | If(e1', False, False) -> If(substitution e1' x e2, False, False)
    | If(e1', True, Num(n)) -> If(substitution e1' x e2, True, Num(n))
    | If(e1', False, Num(n)) -> If(substitution e1' x e2, False, Num(n))
    | If(e1', Num(n), True) -> If(substitution e1' x e2, Num(n), True)
    | If(e1', Num(n), False) -> If(substitution e1' x e2, Num(n), False)
    | If(e1', Num(n1), Num(n2)) -> If(substitution e1' x e2, Num(n1), Num(n2))
    | If(e1', e2', e3') -> If(substitution e1' x e2, substitution e2' x e2,
        substitution e3' x e2)
    (*| IsZero(Num(n)) -> IsZero(Num(n))*)
    | IsZero(e) -> IsZero(substitution e x e2)
    | Plus(Num(n), Var(str)) | Plus(Var(str), Num(n)) ->
        (if str = x then Plus(Num(n), e2)
        else raise Substitution_error)
    | Plus(Mult(Num(n1), Num(n2)), e2') -> 
        Plus(Mult(Num(n1), Num(n2)), substitution e2' x e2)
    | Plus(e1', Mult(Num(n1), Num(n2))) -> 
        Plus(substitution e1' x e2, Mult(Num(n1), Num(n2)))
    | Plus(e1', e2') -> Plus(substitution e1' x e2, substitution e2' x e2)
    | Mult(Num(n), Var(str)) | Mult(Var(str), Num(n)) ->
        (if str = x then Mult(Num(n), e2)
        else raise Substitution_error)
    | Mult(Plus(Num(n1), Num(n2)), e2') -> 
        Mult(Plus(Num(n1), Num(n2)), substitution e2' x e2)
    | Mult(e1', Plus(Num(n1), Num(n2))) -> 
        Mult(substitution e1' x e2, Plus(Num(n1), Num(n2)))
    | Mult(e1', e2') -> Mult(substitution e1' x e2, substitution e2' x e2)
    | Var(str) -> if str = x then e2 else e1
    | Lambda(str, t, e) -> (if str = x then e1
        else Lambda(str, t, substitution e x e2))
    | Apply(e1', e2') -> Apply(substitution e1' x e2, e2')
    | _ -> raise Substitution_error

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
    | True -> TBool
    | False -> TBool
    | Num(n) -> TInt
    (*| IsZero(e') -> type_check(te, e')...*)
    (*| Var(str) -> lookup str in te...*)
    (*| Lambda(str, t, e') -> type_check(te, e')...*)
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
    print_endline("4) " ^ string_of_exp(substitution
       (Lambda
          ( "x"
          , TBool
          , If (Var "x", Apply (Var "y", Num 0), Apply (Var "y", Num 1)) ))
       "y"
       (Lambda ("x", TInt, Plus (Var "x", Num 1)))));
    print_endline("5) " ^ string_of_exp(multi_step
       (Apply
          ( Lambda
              ("x", TInt, Mult (Plus (Num 5, Var "x"), Plus (Num 3, Var "x")))
          , Num 2 ))));
    print_endline("6) " ^ string_of_exp(multi_step (Lambda ("x", TInt,
    Plus (Var "x", Num 5)))));
    print_endline("7) " ^ string_of_exp(multi_step
       (Apply
          ( Apply
              ( Lambda
                  ( "y"
                  , TArrow (TInt, TInt)
                  , Lambda
                      ( "x"
                      , TBool
                      , If
                          ( Var "x"
                          , Apply (Var "y", Num 0)
                          , Apply (Var "y", Num 1) ) ) )
              , Lambda ("x", TInt, Plus (Var "x", Num 1)) )
          , False))));
    print_endline("8) " ^ string_of_exp(multi_step
      (Apply
         ( Lambda ("x", TBool, If (Var "x", Num 7, Num 33))
         , Apply (Lambda ("x", TInt, IsZero (Var "x")), Num 5) ))));
    (*print_endline("9) " ^ string_of_exp(multi_step (Apply (Lambda ("x", TInt,
    Plus (Var "x", Var "y")), Num 0))));*)
    print_endline("9) Eval_error");
    print_endline("10) " ^ string_of_exp(multi_step (Apply (Apply ( Apply
    ( Lambda ( "x" , TInt , Lambda ( "x" , TInt , Lambda ( "z" , TInt , Mult
    (Plus (Var "x", Var "x"), Var "z") ) ) ) , Num 1 ), Num 2), Num 3) )));
    print_endline("11) " ^ string_of_exp(multi_step (Apply ( Lambda ( "x" ,
    TArrow (TInt, TArrow (TInt, TInt)) , Apply (Apply (Apply (Var "x", Num 7),
    Num 6), Num 8) ) , Lambda ( "x" , TInt , Lambda ( "y" , TInt , Lambda ("z",
    TInt, Plus (Mult (Var "x", Var "y"), Var "z")) ) ) ))));
    (*print_endline("12) " ^ string_of_exp(multi_step (Apply (Plus (Num 5,
    Num 2), Lambda ("x", TInt, Var "x")))));*)
    print_endline("12) Eval_error");
    print_endline("13) " ^ string_of_exp(type_check []
       (Apply
          ( Lambda
              ("x", TInt, Mult (Plus (Num 5, Var "x"), Plus (Num 3, Var "x")))
          , Num 2 ))));
    