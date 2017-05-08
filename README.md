# What is this?

Lua parser implemented in OCaml using menhir.

# Requirements

- OCaml
- menhir
- ocamlbuild

It is strongly recommended to use OPAM to setup your environment.

# How to use

First invoke `make` to build `main.byte`, which counts the number of function definitions (just a sample application).

```
./main.byte "path to lua program"
```

# Why conflicts?

You could see that when you build menhir warns that parser.mly has 2 shift/reduce conflicts.  This ambiguity is inherent to the lua syntax.  The following code snippet (excerpt from the lua manual https://www.lua.org/manual/5.3/manual.html#3.3) illustrates this situation:

```
a = b + c
(print or io.write)('done')
```

This statement could be read in two ways.

1.

```
a = b + c;
(print or io.write)('done')
```

In this case, when the first open parenthesis is reached, reducing is occured and `c` is interpreted as some integer value.

The corresponding AST is

```
[ Assign ([Name "a"], [BinOp (Addition, PrefixExp (Var (Name "b")), PrefixExp (Var (Name "c")))])
; FunctionCall
   (Function
     (Exp
       (BinOp (LogicalOr, PrefixExp (Var (Name "print")),
         PrefixExp (Var (IndexTable (Var (Name "io"), LiteralString "write"))))),
     [LiteralString "done"]))]
```

2.

```
a = b + (c(print or io.write)('done'))
```

In this case, when the first open parenthesis is reached, shifting is occured and `(print or io.write)` is interpreted as an argument to the function `c`.

The corresponding AST is

```
[ Assign ([Name "a"], [BinOp (Addition, PrefixExp (Var (Name "b")),
   PrefixExp
    (FunctionCallExp
      (Function
        (FunctionCallExp
          (Function (Var (Name "c"),
            [BinOp (LogicalOr, PrefixExp (Var (Name "print")),
              PrefixExp
               (Var (IndexTable (Var (Name "io"), LiteralString "write"))))])),
        [LiteralString "done"]))))])]
```

The another conflict is almost same as the above code, but starts from the function call instead of the assignment (e.g. `foo(x)(bar or baz)(z)`).
