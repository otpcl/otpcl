# OTPCL

## What is it?

Open Telecom Platform Command Language, a.k.a. Tcl-flavored Erlang.  Or maybe it's Erlang-flavored Tcl?

## What can it do?

Well, it can parse a Tcl-like language:

```erlang

1> otpcl_parse:parse("foo {bar $baz {bam [bat $baf]} bal} $bad $bak$bae [bah $bay]").
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

2> otpcl_eval:eval("set foo puts; $foo {Hello, world!~n}").
Hello, world!
{ok,{#{puts => #Fun<otpcl_eval.1.27999811>,
       set => #Fun<otpcl_eval.0.27999811>},
     #{foo => puts}}}

```

Right now it's pretty minimal; the current "standard library" only
consists of `puts` and `set`, and OTPCL does not currently support
calling Erlang-native functions (though this will hopefully be
implemented in the near future!).  However, you can certainly define
your own functions:

```erlang

3> Sum = fun (Nums, State) -> {lists:sum(Nums), State} end.
#Fun<erl_eval.12.127694169>
4> {ok, sum, Sum, MyState} = otpcl_eval:set_fun(sum, Sum, otpcl_eval:default_state()).
{ok,sum,#Fun<erl_eval.12.127694169>,
    {#{puts => #Fun<otpcl_eval.1.27999811>,
       set => #Fun<otpcl_eval.0.27999811>,
       sum => #Fun<erl_eval.12.127694169>},
     #{}}}
5> otpcl_eval:eval("set foo [sum 1 2 3 4 5]", MyState).
{ok,{#{puts => #Fun<otpcl_eval.1.27999811>,
       set => #Fun<otpcl_eval.0.27999811>,
       sum => #Fun<erl_eval.12.127694169>},
     #{foo => 15}}}

```


## What *should* it do?

* Tokenizer (100%)

* Parser (80%)

* Interpreter (90%)

* Standard library / built-in functions (1%)

* Compiler (0%)

* REPL/shell (0%)

* Tests (no idea what the test coverage is right now, but hey, at
  least I wrote tests this time!)

* Docs (0%)

## What's the license?

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
