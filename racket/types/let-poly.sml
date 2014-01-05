(* Let-polymorphism: type inference and polymorphism ala SML *)

type type_ = Type'.type_
(*optional reference to type_ * int*)
type typeVar = type_ option ref * int
type typeSchema = type_ * (typeVar list)

(* An environment now bind a variable to a type schema rather
 * than to just a type. *)
type env = typeSchema Env.env

(* Union of l1 and l2 considered as sets.
   Requires: l1 and l2 contain no duplicates *)
fun union(l1: typeVar list,l2: typeVar list): typeVar list =
  foldl (fn (r,l) =>
         if List.exists(fn(r') => r=r') l then l else r::l)
  l1 l2

(* Difference of l1 and l2 considered as sets.
   Requires: l1 and l2 contain no duplicates *)
fun diff(l1,l2): typeVar list =
  List.filter (fn(r) => not (List.exists(fn(r') => r=r') l2)) l1

(* All unsolved type variables in t. *)
fun unsolved(t: Type'.type_): typeVar list =
  case t
    of Type'.Int => nil | Type'.Bool => nil
     | Type'.Arrow(t1,t2) => union(unsolved(t1), unsolved(t2))
     | Type'.Pair(t1, t2) => union(unsolved(t1), unsolved(t2))
     | Type'.TypeVar(r,nm) => (case !r
                    of NONE => [(r,nm)]
                     | SOME(t) => unsolved(t))

(* All unsolved type variables mentioned in the type environment. This
 * implementation is not very efficient! *)
fun envUnsolved(r: env): typeVar list =
  foldl (fn((x,(t,tvs)),l0) =>
         let val l1 = unsolved(t) in
           union(l0,l1)
         end) nil r

(* Given an environment r, produce a schema for t that identifies
 * all the new type variables in t, which can be arbitrarily
 * substituted for. *)
fun schema(t: type_, r: env): typeSchema =
  let
    val uv = unsolved(t)
    val ev = envUnsolved(r)
    val uv' = diff(uv,ev)
  in
    (t, uv')
  end

(* A type just like t except that every type variable in tvs has
 * been consistently replaced by a fresh type variable. *)
fun instantiate(t: type_, tvs: typeVar list): type_ =
  let
    val tm = foldl (fn (tv: typeVar, tm: (typeVar*type_) list) =>
                    (tv, Type'.freshTypeVar())::tm) nil tvs
    fun instVar(tv: typeVar, tm: (typeVar*type_) list): type_ =
      case tm of nil => Type'.TypeVar(tv)
    | (tv1, tv2)::tm' => if (tv1 = tv) then tv2 else instVar(tv,tm')
    fun inst(t: type_) =
      case t of
        Type'.Int => t | Type'.Bool => t
      | Type'.Arrow(t1,t2) => Type'.Arrow(inst(t1), inst(t2))
      | Type'.Pair(t1,t2) => Type'.Pair(inst(t1), inst(t2))
      | Type'.TypeVar(tv) => instVar(tv, tm)
  in
    inst(t)
  end


(* tcheck(r,e) is the type of e in type environment r.
 * Raises Fail if the expression e does not type-check. *)
fun tcheck(r: env, e: expr):type_ =
  case e of
    Var(x) => instantiate(Env.lookup(r, x)) (* instantiate schema here *)
  | True => Type'.Bool
  | False => Type'.Bool
  | IntConst(n) => Type'.Int
  | Let(d,e) => tcheck(foldl declcheck r d, e)
  | If(e1, e2, e3) =>
      let val t2 = tcheck(r, e2)
          val t3 = tcheck(r, e3)
      in
        Type'.unify(Type'.Bool, tcheck(r, e1));
        Type'.unify(t2,t3);
        t2
      end
  | Op(e1,e2) => (Type'.unify(tcheck(r,e1), Type'.Int);
                  Type'.unify(tcheck(r,e2), Type'.Int);
                  Type'.Int)
  | Fun(x,e) =>
      let val t = Type'.freshTypeVar()
        val ts = (t, []) (* no polymorphism inside the -> *)
      in
        Type'.Arrow(t, tcheck(Env.update(r,x,ts), e))
      end
  | Apply(e1, e2) => (case tcheck(r, e1)
                       of Type'.Arrow(t1, t2) =>
                         (Type'.unify(t1, tcheck(r, e2)); t2)
                        | _ => raise Fail("wrong arg type"))
  | Pair(e1,e2) => Type'.Pair(tcheck(r, e1), tcheck(r, e2))
  | Select(i, e) => let val t1 = Type'.freshTypeVar()
                        val t2 = Type'.freshTypeVar()
    in Type'.unify(Type'.Pair(t1,t2), tcheck(r, e));
      (case i
         of 1 => t1
          | 2 => t2
          | _ => raise Fail("Illegal index"))
    end
and
  (* The environment r extended with the declaration d.
   * Raises Fail if the declaration d does not
   * type-check. *)
  declcheck(d: decl, r:env):env =
  case d of
       (*x = e*)
    VarDecl(x,e) =>
      Env.update(r, x, schema(tcheck(r, e), r)) (* generate schema here *)
      (*fun f(x) = e*)
  | FunDecl(f,x,e) =>
      let
        val t1 = Type'.freshTypeVar()
        val t2 = Type'.freshTypeVar()
        val tf = Type'.Arrow(t1,t2)
        val r' = Env.update(r,f,(tf,[]))
        val r'' = Env.update(r',x,(t1,[]))
        val te = tcheck(r'', e)
      in
        Type'.unify(te, t2);
        Env.update(r, f, schema(tf,r)) (* generate schema here *)
      end

val ident = Fun("x", Var("x"))

(* let fun f(x) = x in f(2) + (f f)(3) end *)
val e1 = Let([FunDecl("f", "x", Var("x"))],
  Op(Apply(Var("f"), IntConst(2)),
     Apply(Apply(Var("f"), Var("f")), IntConst(3))))

