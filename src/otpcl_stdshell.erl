-module(otpcl_stdshell).

-export([chgrp/2, chmod/2, chown/2, source/2, cp/2, rmdir/2, rm/2, cwd/2, ls/2,
         dir/2, mkdir/2, ln/2, cd/2, env/2, trap/2, run/2]).

-otpcl_funs([chgrp, chmod, chown, source, cp, rmdir, rm, cwd, ls, dir, mkdir, ln,
             cd, env, trap, run]).

chgrp([Gid, Name|Names], State) ->
    case file:change_group(Name, Gid) of
        ok ->
            chgrp([Gid|Names], State);
        Err ->
            {Err, State}
    end;
chgrp([_], State) ->
    {ok, State}.


chmod([Mode, Name|Names], State) ->
    case file:change_mode(Name, Mode) of
        ok ->
            chmod([Mode|Names], State);
        Err ->
            {Err, State}
    end;
chmod([_], State) ->
    {ok, State}.

chown([Uid, Name|Names], State) ->
    case file:change_owner(Name, Uid) of
        ok ->
            chown([Uid|Names], State);
        Err ->
            {Err, State}
    end;
chown([_], State) ->
    {ok, State}.

source([Filename], State) ->
    otpcl_eval:eval_file(Filename, State).

cp([Src, Dest|Dests], State) ->
    case file:copy(Src, Dest) of
        {ok, _} ->
            cp([Src|Dests], State);
        Err ->
            {Err, State}
    end;
cp([_], State) ->
    {ok, State}.

rmdir([Dir|Dirs], State) ->
    case file:del_dir(Dir) of
        ok ->
            rmdir(Dirs, State);
        Err ->
            {Err, State}
    end;
rmdir([], State) ->
    {ok, State}.

rm([Name|Names], State) ->
    case file:delete(Name) of
        ok ->
            rm(Names, State);
        Err ->
            {Err, State}
    end;
rm([], State) ->
    {ok, State}.

cwd([], State) ->
    {file:get_cwd(), State}.

ls([Dir], State) ->
    {file:list_dir(Dir), State};
ls(['-a', Dir], State) ->
    {file:list_dir_all(Dir), State}.

dir(Args, State) ->
    ls(Args, State).

mkdir([Dir|Dirs], State) ->
    case file:make_dir(Dir) of
        ok ->
            mkdir(Dirs, State);
        Err ->
            {Err, State}
    end;
mkdir([], State) ->
    {ok, State}.

ln(['-s'|Args], State) ->
    ln_s(Args, State);
ln([Src, Dest|Dests], State) ->
    case file:make_link(Src, Dest) of
        ok ->
            ln([Src|Dests], State);
        Err ->
            {Err, State}
    end;
ln([_], State) ->
    {ok, State}.

ln_s([Src, Dest|Dests], State) ->
    case file:make_symlink(Src, Dest) of
        ok ->
            ln_s([Src|Dests], State);
        Err ->
            {Err, State}
    end;
ln_s([_], State) ->
    {ok, State}.

cd([Dir], State) ->
    {file:set_cwd(Dir), State}.

env([get], State) ->
    {os:getenv(), State};
env([get, Name], State) ->
    {os:getenv(Name), State};
env([set, Name, Value], State) ->
    {os:putenv(Name, Value), State};
env([unset, Name], State) ->
    {os:unsetenv(Name), State}.

trap([Signal, Option], State) ->
    {os:set_signal(Signal, Option), State}.

run([Cmd], State) ->
    {os:cmd(Cmd), State}.
