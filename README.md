# Diamondback

![A diamondback](https://upload.wikimedia.org/wikipedia/commons/d/d4/Crotalus_ruber_02.jpg)

In this assignment, you'll implement a compiler for a small language with
functions declarations and function calls, conforming to the C stack layout.
You'll also add some static well-formedness checks to the compiler.

This is the best I could do:

- **D**ouble-word
- **I**ntel **A**rchitecture
- **MO**stly dynamic
- **N**ested-expression
- (**D**iamondback supports recursion)
- **B**oolean-tagged
- **A**NF-transformed
- **C**ompiler.
- **K**?

## The Diamondback Language

As usual, we have concrete and abstract syntaxes, along with a specification
of semantics.

### Concrete Syntax

The major addition to Diamondback are _function declarations_.  Our programs
are now a sequence of zero or more function declarations, followed by a single
_main expression_.

```
<program> :=
  | decls expr
  | expr

<decls> :=
  | decl
  | decl decls

<expr> :=
  | let <bindings> in <expr>
  | if <expr>: <expr> else: <expr>
  | <binop-expr>

<binop-expr> :=
  | <identifier>
  | <number>
  | true
  | false
  | add1(<expr>)
  | sub1(<expr>)
  | isnum(<expr>)
  | isbool(<expr>)
  | print(<expr>)
  | <identifier>(<exprs>)
  | <identifier>()
  | <expr> + <expr>
  | <expr> - <expr>
  | <expr> * <expr>
  | <expr> < <expr>
  | <expr> > <expr>
  | <expr> == <expr>
  | ( <expr> )

<exprs> :=
  | <expr>
  | <expr> , <exprs>

<bindings> :=
  | <identifier> = <expr>
  | <identifier> = <expr>, <bindings>
```

The other addition is _function applications_, which are written
`<identifier>(<exprs>)`, for example `f(1, 2, 3)`.  This is the syntax for a
_call_ to a function.

### Abstract Syntax

As usual, we have a user-facing syntax and a compiler-facing syntax.  In this
assignment, these definitions have been split into the separate `expr.ml`
file.

```
type prim1 =
  | Add1
  | Sub1
  | Print
  | IsNum
  | IsBool

type prim2 =
  | Plus
  | Minus
  | Times
  | Less
  | Greater
  | Equal

type expr =
  | ELet of (string * expr) list * expr
  | EPrim1 of prim1 * expr
  | EPrim2 of prim2 * expr * expr
  | EApp of string * expr list
  | EIf of expr * expr * expr
  | ENumber of int
  | EBool of bool
  | EId of string

type decl =
  | DFun of string * string list * expr

type program =
  | Program of decl list * expr


type immexpr =
  | ImmNumber of int
  | ImmBool of bool
  | ImmId of string

and cexpr =
  | CPrim1 of prim1 * immexpr
  | CPrim2 of prim2 * immexpr * immexpr
  | CApp of string * immexpr list
  | CIf of immexpr * aexpr * aexpr
  | CImmExpr of immexpr

and aexpr =
  | ALet of string * cexpr * aexpr
  | ACExpr of cexpr

and adecl =
  | ADFun of string * string list * aexpr

and aprogram =
  | AProgram of adecl list * aexpr
```

The `EApp` and `CApp` constructors correspond to function applications, and
the `decl` and `adecl` types respresent function declarations.  A program is a
list of `decls` (surface) or `adecls` (ANF), with a main expression.

### Semantics

There are several distinguishing features of Diamondback.  The first is
function applications.  A function application should give the answer we'd get
if we followed the rules for substituting argument values for parameter names.
So, for example:

```
def f(x, y):
  x + y

f(1, 2)
```

Should produce 3.

Your compiler should use the rules for C stacks discussed in class and at
[this assembly guide](http://www.cs.virginia.edu/~evans/cs216/guides/x86.html) 
to implement this behavior.

There are a number of new _errors_ that can occur now that we have function
declarations and calls.  Your implementation should catch all of these cases
_statically_; that is, before the program runs:

- A function application with the wrong number of arguments should signal an
  error containing the string "arity"
- A function application of a non-existent function should signal an error
  containing the string "not defined"
- An identifier without a corresponding binding location should report an
  error containing the string "unbound"
- A let binding with duplicate names should report an error containg the
  string "duplicate binding"
- A function declaration with duplicate names in the argument list should
  report an error containg the string "duplicate parameter"
- If there are multiple function definitions with the same name, report an
  error containing the string "duplicate function"

Again, these errors should stop the program from compiling, _not_ happen at
runtime.  You can continue to assume that all identifiers within a function
body have different names, which is a requirement for ANF (we could implement
another pass to rename variables, but we won't do that here).

### Implementation

You shouldn't need any new assembly instructions to tackle this
implementation.  You're free to add your own new instructions (some of you
used `sar` and `test` and `jg`, for example, and you can carry those over).

There are a few new pieces to the implementation:

- A new structure for ANF designed to reduce the number of intermediate
  variables created.  You should study and understand it so you can add the
  `ECall` case.  This is the first thing you should add, and you'll probably
  want to define a helper that performs ANF on a _list_ of expressions, and
  takes a "k" that expects to receive a _list_ of results.
- A new set of `well_formed` functions, which are called prior to performing
  ANF and are used to report the errors above.
- The `AProgram` structure, which contains both function declarations and the
  expression for `our_code_starts_here`.  You will probably want to generate
  an assembly structure that looks like:

  ```
  ;; extern and global stuff
  fun_decl1

  ```

