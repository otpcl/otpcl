# OTPCL

## What is it?

Open Telecom Platform Command Language, a.k.a. Tcl-flavored Erlang.
Or maybe it's Erlang-flavored Tcl?

## How do I use it?

For now, clone this repo, and make sure you have rebar3 installed.
Then, from the repo's root:

```
$ rebar3 compile
[ ... bunch of rebar3 output that hopefully looks successful ... ]

$ bin/otpcl
OTPCL Shell (WIP!)

otpcl> import lists; sum (1 2 3 4 5)
15
otpcl>
```

You can also use it from an existing project by installing the library
per your BEAM-based-language's standard mechanism (TODO: upload to
Hex) and calling the relevant modules/functions directly:

```
$ rebar3 shell
===> Verifying dependencies...
===> Compiling otpcl
Eshell V10.0  (abort with ^G)
1> {RetVal, State} = otpcl:eval("import lists; sum (1 2 3 4 5)").
[ ... bunch of output because we just imported everything from
      Erlang's lists module and otpcl:eval returns the full
      interpreter state when it's done executing stuff ... ]
2> RetVal.
15
```

## What (else) can it do?

Well, as you might've guessed from above, it can parse a Tcl-like
language:

```erlang

3> otpcl:parse("foo {bar $baz {bam [bat $baf]} bal} $bad $bak$bae [bah $bay]").
{ok,{parsed,program,
        [{parsed,command,
             [{parsed,unquoted,
                  [{102,{nofile,0,0}},{111,{nofile,0,1}},{111,{nofile,0,2}}]},
              {parsed,braced,
                  [{98,{nofile,0,5}},
                   {97,{nofile,0,6}},
                   {114,{nofile,0,7}},
                   {32,{nofile,0,8}},
                   {36,{nofile,0,9}},
                   {98,{nofile,0,10}},
                   {97,{nofile,0,11}},
                   {122,{nofile,0,12}},
                   {32,{nofile,0,13}},
                   {123,{nofile,0,14}},
                   {98,{nofile,0,...}},
                   {97,{nofile,...}},
                   {109,{...}},
                   {32,...},
                   {...}|...]},
              {parsed,var_unquoted,
                  [{98,{nofile,0,37}},{97,{nofile,0,38}},{100,{nofile,0,39}}]},
              {parsed,var_unquoted,
                  [{98,{nofile,0,42}},
                   {97,{nofile,0,43}},
                   {107,{nofile,0,44}},
                   {36,{nofile,0,45}},
                   {98,{nofile,0,46}},
                   {97,{nofile,0,47}},
                   {101,{nofile,0,48}}]},
              {parsed,funcall,
                  [{parsed,unquoted,
                       [{98,{nofile,0,51}},
                        {97,{nofile,0,52}},
                        {104,{nofile,0,53}}]},
                   {parsed,var_unquoted,
                       [{98,{nofile,0,56}},
                        {97,{nofile,0,57}},
                        {121,{nofile,0,...}}]}]}]}]},
    []}

```

And it can interpret that language, too:

```erlang

4> otpcl:eval("print {Hello, world!~n}").
Hello, world!
{ok,{#{decr => fun otpcl_stdlib:decr/2,
       'if' => fun otpcl_stdlib:if/2,
       incr => fun otpcl_stdlib:incr/2,
       print => fun otpcl_stdlib:print/2,
       set => fun otpcl_stdlib:set/2,
       unless => fun otpcl_stdlib:unless/2},
     #{'RETVAL' => ok}}}
     
5> otpcl:eval("set foo 1; set bar 2; set baz 3; incr foo").
{2,
 {#{decr => fun otpcl_stdlib:decr/2,
    'if' => fun otpcl_stdlib:if/2,
    incr => fun otpcl_stdlib:incr/2,
    print => fun otpcl_stdlib:print/2,
    set => fun otpcl_stdlib:set/2,
    unless => fun otpcl_stdlib:unless/2},
  #{'RETVAL' => 2,bar => 2,baz => 3,foo => 2}}}

```

And as demonstrated above, you can do things from the OTPCL shell/REPL:

```
otpcl> print "Hello, world!~n"
Hello, world!
ok
otpcl> import math
ok
otpcl> exp 4
54.598150033144236
otpcl> exp foo
error error: badarg
```

You can't define your own functions from within OTPCL yet (there's no
`def` or `fun` or `proc` or anything like that), but you can certainly
define some by tweaking the interpreter state directly:

```erlang

4> Sum = fun (Nums, State) -> {lists:sum(Nums), State} end.
#Fun<erl_eval.12.127694169>
5> {ok, State} = otpcl_stdlib:funset([sum, Sum], otpcl_env:default_state()).
{ok,{#{decr => fun otpcl_stdlib:decr/2,
       eval => fun otpcl_stdlib:eval/2,
       'if' => fun otpcl_stdlib:if/2,
       import => fun otpcl_stdlib:import/2,
       incr => fun otpcl_stdlib:incr/2,
       print => fun otpcl_stdlib:print/2,
       set => fun otpcl_stdlib:set/2,
       sum => #Fun<erl_eval.12.127694169>,
       unless => fun otpcl_stdlib:unless/2},
     #{'RETVAL' => ok}}}
6> {RetVal, NewState} = otpcl:eval("sum 1 2 3 4 5", State).
{15,
 {#{decr => fun otpcl_stdlib:decr/2,
    eval => fun otpcl_stdlib:eval/2,
    'if' => fun otpcl_stdlib:if/2,
    import => fun otpcl_stdlib:import/2,
    incr => fun otpcl_stdlib:incr/2,
    print => fun otpcl_stdlib:print/2,
    set => fun otpcl_stdlib:set/2,
    sum => #Fun<erl_eval.12.127694169>,
    unless => fun otpcl_stdlib:unless/2},
  #{'RETVAL' => 15}}}
7> RetVal.
15

```

Or, as demonstrated above, you can even import them, whether as whole
modules:

```
otpcl> import random; uniform 8675309
3848234
```

Or as individual functions:

```
otpcl> import string (split uppercase)
ok
otpcl> split [uppercase "foo,bar,baz"], ","
[<<"FOO">>,<<"BAR,BAZ">>]
```

As you can tell, it's all pretty spartan, but it's a decent-enough
starting point, I'd say.

## What *should* it do?

* Tokenizer (100%)

* Parser (90%) (probably more stuff that can be added, like pipes; I
  like pipes, and therefore want to add them)

* Interpreter (90%) (pretty sure it's fully functional; just needs
  better error handling)

* Standard library / built-in functions (10%)

* Compiler (0%)

* REPL/shell (50%) (it "works", but needs proper error handling)

* Tests (no idea what the test coverage is right now, but hey, at
  least I wrote tests this time!)

* Docs (0%; something something self-documenting code something
  something)

* Install procedure that's actually sane (or for that matter exists at
  all)

## What's the actual syntax?

Like with Tcl, an OTPCL program is a sequence of
vertical-whitespace-delimited commands (semicolons counting as
"vertical whitespace" in this context), each of which is a sequence of
horizontal-whitespace-delimited words (note: not all forms of
horizontal/vertical whitespace are currently recognized as such by the
parser).

A word may be any of the following:

* An atom (either `unquoted` or `'Single Quoted'`)
* An integer (`123` or `-123`)
* A float (`123.456` or `-123.456`)
* A binary string (either `"double quoted"` or `{curly braced}`)
* A charlist string (backquoted)
* A list (`(word-elements surrounded by parentheses)`)
* A tuple (`<word-elements surrounded by angle brackets>`)
* A variable substitution (either `$unquoted` or `${braced}`)
* A function call substitution (`[command inside square brackets]`)

### Crash Course

```tcl

this is a command  # this is a comment
this is one command; this is another command
this command (accepts a list)
this command <accepts a tuple>
this command "accepts a binary string"
this command {also accepts a binary string}
this command `accepts an Erlang-style charlist string`
this command 'Accepts an atom that has spaces in it'
this command will use a $variable ${another variable} and a [function call]

```

## What's the license?

OpenBSD-style ISC License:

> Copyright (c) 2018, 2019 Ryan S. Northrup <northrup@yellowapple.us>

> Permission to use, copy, modify, and distribute this software for
> any purpose with or without fee is hereby granted, provided that the
> above copyright notice and this permission notice appear in all
> copies.

> THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
> WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
> WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
> AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
> DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
> OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
> TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
> PERFORMANCE OF THIS SOFTWARE.
