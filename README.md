# OTPCL

## What is it?

Open Telecom Platform Command Language, a.k.a. Tcl-flavored Erlang.  Or maybe it's Erlang-flavored Tcl?

## What can it do?

Well, it can parse a Tcl-like language:

```erlang

1> otpcl:parse("foo {bar $baz {bam [bat $baf]} bal} $bad $bak$bae [bah $bay]").
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

2> otpcl:eval("print {Hello, world!~n}").
Hello, world!
{ok,{#{decr => fun otpcl_stdlib:decr/2,
       'if' => fun otpcl_stdlib:if/2,
       incr => fun otpcl_stdlib:incr/2,
       print => fun otpcl_stdlib:print/2,
       set => fun otpcl_stdlib:set/2,
       unless => fun otpcl_stdlib:unless/2},
     #{'RETVAL' => ok}}}
     
3> otpcl:eval("set foo 1; set bar 2; set baz 3; incr foo").
{2,
 {#{decr => fun otpcl_stdlib:decr/2,
    'if' => fun otpcl_stdlib:if/2,
    incr => fun otpcl_stdlib:incr/2,
    print => fun otpcl_stdlib:print/2,
    set => fun otpcl_stdlib:set/2,
    unless => fun otpcl_stdlib:unless/2},
  #{'RETVAL' => 2,bar => 2,baz => 3,foo => 2}}}

```

Right now it's pretty minimal; the current "standard library" only
consists of a handful of demo functions (you can see them in the
returned final state of both those `otpcl:eval/1` examples above), and
OTPCL does not currently support calling Erlang-native functions
(though this will hopefully be implemented in the near future!).
However, you can certainly define your own functions:

```erlang

4> Sum = fun (Nums, State) -> {lists:sum(Nums), State} end.
#Fun<erl_eval.12.127694169>
5> {ok, sum, Sum, MyState} = otpcl_env:set_fun(sum, Sum, otpcl_env:default_state()).
{ok,sum,#Fun<erl_eval.12.127694169>,
    {#{decr => fun otpcl_stdlib:decr/2,
       'if' => fun otpcl_stdlib:if/2,
       incr => fun otpcl_stdlib:incr/2,
       print => fun otpcl_stdlib:print/2,
       set => fun otpcl_stdlib:set/2,
       sum => #Fun<erl_eval.12.127694169>,
       unless => fun otpcl_stdlib:unless/2},
     #{'RETVAL' => ok}}}
6> otpcl_eval:eval("set foo [sum 1 2 3 4 5]", MyState).
{ok,{#{decr => fun otpcl_stdlib:decr/2,
       'if' => fun otpcl_stdlib:if/2,
       incr => fun otpcl_stdlib:incr/2,
       print => fun otpcl_stdlib:print/2,
       set => fun otpcl_stdlib:set/2,
       sum => #Fun<erl_eval.12.127694169>,
       unless => fun otpcl_stdlib:unless/2},
     #{'RETVAL' => ok,foo => 15}}}

```


## What *should* it do?

* Tokenizer (100%)

* Parser (90%) (probably more stuff that can be added, like pipes; I
  like pipes, and therefore want to add them)

* Interpreter (90%)

* Standard library / built-in functions (5%)

* Compiler (0%)

* REPL/shell (0%)

* Tests (no idea what the test coverage is right now, but hey, at
  least I wrote tests this time!)

* Docs (0%)

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

> Copyright (c) 2018 Ryan S. Northrup <northrup@yellowapple.us>

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
