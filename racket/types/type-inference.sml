type id = string

exception TypeUnifyError of string

datatype expr =
    Var of id                    (* id *)
  | True | False                 (* true, false *)
  | IntConst of int              (* n *)
  | If of expr * expr * expr     (* if e1 then e2 else e3 *)
  | Let of (decl list) * expr    (* let d in e end *)
  | Fun of id * expr             (* fn (id) => expr -- NO TYPE DECL *)
  | Op of expr * expr            (* e1 + e2 *)
  | Select of int*expr           (* #i e *)
  | Pair of expr * expr          (* (e1, e2) *)
  | Apply of expr * expr         (* e1(e2); *)

and decl =
    VarDecl of id * expr         (* val id = e -- NO TYPE DECL *)
  | FunDecl of id * id * expr    (* fun id1(id2) = e -- NO TYPE DECL *)

and type_ =
    Int
  | Bool
  | Arrow of type_ * type_
  | Product of type_ * type_
  | TypeVar of type_ option ref * int

(* Create a fresh type variable. *)
val cur_index = ref 0
fun freshTypeVar(): type_ = (cur_index := 1 + !cur_index;
                             TypeVar(ref NONE, !cur_index-1))

fun typeToString(t: type_): string =
  case t
    of Int => "int"
     | Bool => "bool"
     | Arrow(t1,t2) => "("^typeToString(t1)^"->"^typeToString(t2)^")"
     | Product(t1,t2) => "("^typeToString(t1)^"*"^typeToString(t2)^")"
     | TypeVar(r,nm) =>  case !r
                           of NONE => "'"^Char.toString(chr(nm + ord #"a"))
                            | SOME t => typeToString(t)

(* Determine whether t1 can be made equal to t2, by unification.
 * Effects: may solve for some type variables, as necessary. Raises
 * Fail if the two types cannot be unified. *)
fun unify(t1: type_, t2: type_): unit =
  (
   print ("Unify " ^ typeToString(t1) ^ " with " ^ typeToString(t2) ^ "\n");
   case t1
     of TypeVar(r,nm) => unifyVar(t2, r)
      | Int =>
       (case t2
          of Int => ()
           | TypeVar(r,_) => unifyVar(t1, r)
           | _ => raise TypeUnifyError "Int")
      | Bool =>
          (case t2
             of Bool => ()
              | TypeVar(r,_) => unifyVar(t1, r)
              | _ => raise TypeUnifyError "Bool")
      | Arrow(t3, t4) =>
             (case t2
                of Arrow(t3',t4') => (unify(t3,t3');
                                      unify(t4,t4'))
                 | TypeVar(r,_) => unifyVar(t1,r)
                 | _ => raise TypeUnifyError "Arrow")
      | Product(t3,t4) =>
                (case t2
                   of Product(t3', t4') => (unify(t3,t3');
                                         unify(t4,t4'))
                    | TypeVar(r,_) => unifyVar(t1, r)
                    | _ => raise TypeUnifyError "Product")
                   )
and unifyVar(t1:type_, r:type_ option ref): unit =
  case !r
    of NONE =>
      (case t1 of
         TypeVar(r1,_) => if r1 <> r then r := SOME t1 else () (* cycle *)
       | _ => r := SOME t1)
     | SOME t => unify(t1, t)

type env = type_ Env.env

(* tinfer(r,e) is the type of e in type environment r.
 * The execution of tinfer(r,e) triggers type unifications.
 * Raises Fail if the expression e does not type-check. *)
fun tinfer(r: env, e: expr):type_ =
  case e of
    Var(x) => Env.lookup(r, x)
  | True => Bool
  | False => Bool
  | IntConst(n) => Int

  | Let(d,e) => tinfer(foldl declcheck r d, e)

  | If(e1, e2, e3) =>
      let
        val t1 = tinfer(r, e1)
        val t2 = tinfer(r, e2)
        val t3 = tinfer(r, e3)
      in
        unify(t1, Bool);
        unify(t2, t3);
        t2
      end

  | Op(e1,e2) =>
      (unify(tinfer(r,e1), Int);
       unify(tinfer(r,e2), Int);
       Int)

  | Fun(x,e) =>
      let val t = freshTypeVar()
      in Arrow(t, tinfer(Env.update(r,x,t), e))
      end

  | Apply(e1, e2) =>
      let
        val t = freshTypeVar()
        val t' = freshTypeVar()
      in
        unify(tinfer(r, e1), Arrow(t, t'));
        unify(tinfer(r, e2), t);
        t'
      end

  | Pair(e1,e2) => Product(tinfer(r, e1), tinfer(r, e2))

  | Select(i, e) =>
      let
        val t1 = freshTypeVar()
        val t2 = freshTypeVar()
      in
        unify(Product(t1,t2), tinfer(r, e));
        (case i
           of 1 => t1
            | 2 => t2
            | _ => raise Fail("Illegal index"))
      end

(* declcheck(d, r) is the environment r extended with the
 * declaration d. Raises Fail if the declaration d does not
 * type-check. *)
and declcheck(d: decl, r:env):env =
  case d of
    VarDecl(x,e) =>
      Env.update(r, x, tinfer(r, e))
  | FunDecl(f,x,e) =>
      let val t1 = freshTypeVar()
          val t2 = freshTypeVar()
          val tf = Arrow(t1,t2)
          val r' = Env.update(r,f,tf)
          val r'' = Env.update(r',x,t1)
          val te = tinfer(r'', e)
      in
        unify(te, t2); r'
      end

(* EXAMPLES *)

fun ti(e) = (cur_index := 0; typeToString(tinfer(Env.empty, e)))

(* let fun f(n) = n + 2 in f end *)
val e1 = Let(FunDecl("f", "n", Op(Var("n"), IntConst(2)))::nil, Var("f"))

(* let fun f(n) = n in f end *)
val e2 = Let(FunDecl("f", "n", Var("n"))::nil, Var("f"))

(* let fun f(n) = n in f(2) end *)
val e2a = Let(FunDecl("f", "n", Var("n"))::nil, Apply(Var("f"), IntConst(2)))

(* let fun f(x) = fn(y) => x + y in f end *)
val e3 = Let(FunDecl("f", "x", Fun("y", Op(Var("x"), Var("y"))))::nil, Var("f"))

(* let fun loop(x) = loop(x+1) in loop end *)
val e4 = Let([FunDecl("loop", "x",
                      Apply(Var("loop"),
                            Op(Var("x"),IntConst(1))))],
             Var("loop"))

val S = Fun("x", Fun("y", Fun("z", Apply(Apply(Var("x"),Var("z")),
                                         Apply(Var("y"),Var("z"))))))

val K = Fun("x", Fun("y", Var("x")))
val O = Fun("f", Fun("g", Fun("x", Apply(Var("f"),
                                         Apply(Var("g"), Var("x"))))))
val I = Fun("x", Var("x"))
