# ecl

## What is it?

Tcl-flavored Erlang.  Or maybe it's Erlang-flavored Tcl?

## What can it do?

Well, it can parse a Tcl-like language:

```erlang

1> ecl:parse_program("foo {bar $baz {bam [bat $baf]} bal} $bad $bak$bae [bah $bay]").
{ok,{program,[{command,[{string,"foo"},
                        {string,"bar $baz {bam [bat $baf]} bal"},
                        {string,[{var_sub,"bad"}]},
                        {string,[{var_sub,"bak"},{var_sub,"bae"}]},
                        {string,[{cmd_sub,[98,97,104,32,{var_sub,"bay"}]}]}]}]}}

```

Pretty soon it'll (hopefully) be able to actually interpret and run
scripts written in said language.

## What's the license?

> Copyright (c) 2017 Ryan S. Northrup <northrup@yellowapple.us>

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
