#!/usr/bin/env escript
%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%======================================================================
%%
%% LeoFS
%%
%% Copyright (c) 2012-2015 Rakuten, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%%======================================================================
%% ex: ft=erlang ts=4 sw=4 et
%%-module(bootstrap).
%%-export([main/1]).

-define(DEF_SRC_DIR_PATH, "./").
-define(DEF_DST_DIR_PATH, "./").

main(Args) ->
    
    %% Make sure file:consult can parse the .app file
    case file:consult("leofs_l2l.app") of
        {ok, _} ->
            ok;
        {error, Reason} ->
            io:format("Invalid syntax in leofs_l2l.app: ~p\n", [Reason]),
            halt(1)
    end,

    %% Add third party ebin to our path
    EbinDirs = filelib:wildcard("./deps/*/ebin"),
    code:add_paths(EbinDirs),

    %% Execute
    run(Args),
    ok.


%% @doc Parse arguments
%% @private
run([]) ->
    help();
run(["help"]) ->
    help();
run(["info"|_]) ->
    help();
run(["version"]) ->
    ok = application:load(leofs_l2l),
    version();
run(RawArgs) ->
    ok = application:load(leofs_l2l),
    {Opts, _NonOptArgs}= parse_args(RawArgs),
    case proplists:get_value(help, Opts) of
        undefined -> void;
        _ ->
            help(),
            halt(0)
    end,
    case proplists:get_value(version, Opts) of
        undefined -> void;
        _ ->
            version(),
            halt(0)
    end,
    SrcDir = case proplists:get_value(src_dir, Opts) of
        undefined -> ?DEF_SRC_DIR_PATH;
        Val -> 
            Val
    end,
    DstDir = case proplists:get_value(dst_dir, Opts) of
        undefined -> ?DEF_DST_DIR_PATH;
        Val2 -> 
            Val2
    end,
    SrcHandler = case eleveldb:open(SrcDir, []) of
        {error, Cause} ->
            io:format(user, "[error]Source LevelDB open failed: cause:~p~n", [Cause]),
            exit(1);
        {ok, Handler}->
            Handler
    end,
    DstHandler = case eleveldb:open(DstDir, [{create_if_missing, true}]) of
        {error, Cause2} ->
            io:format(user, "[error]Destination LevelDB open failed: cause:~p~n", [Cause2]),
            exit(2);
        {ok, Handler2}->
            Handler2
    end,
    try
        %% place business logic here
        Fun = fun({K, V}, Count) ->
                %io:format(user, "[debug]iterate key:~p value:~p~n", [K, V]),
                case eleveldb:put(DstHandler, K, V, []) of
                    ok ->
                        Count + 1;
                    {error, Cause3} ->
                        io:format(user, "[error]Failed to put data into the destination leveldb key:~p cause:~p~n", [K, Cause3]),
                        Count
                end
        end,
        Total = eleveldb:fold(SrcHandler, Fun, 0, []),
        io:format(user, "[info]# of records in src leveldb:~p~n", [Total]),
        %% confirm records in leveldb
        {ok, Itr} = eleveldb:iterator(DstHandler, []),
        fold_loop(eleveldb:iterator_move(Itr, <<>>), Itr),
        eleveldb:iterator_close(Itr)
    catch
        _Class:Error ->
            io:format(user, "[error]Unexpected error occured: cause:~p~n", [Error])
    after
        eleveldb:close(DstHandler),
        eleveldb:close(SrcHandler)
    end,
    ok.

fold_loop({ok, K, V}, Itr) ->
    Term = binary_to_term(V),
    io:format(user, "[debug]iterate:leveldb key:~p value:~p~n", [K, Term]),
    fold_loop(eleveldb:iterator_move(Itr, next), Itr);
fold_loop({error, _Cause},_Itr) ->
    %% Reach EOF
    ok.

%% @doc Retrieve the version
%% @private
version() ->
    {ok, Vsn} = application:get_key(leofs_l2l, vsn),
    io:format("leofs_l2l~s~n", [Vsn]).


%% @doc Output the help
%% @private
help() ->
    OptSpecList = option_spec_list(),
    getopt:usage(OptSpecList, "leofs_l2l").


%% @doc Parse arguments
%% @private
parse_args(RawArgs) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, RawArgs) of
        {ok, Args} ->
            Args;
        {error, {_Reason, _Data}} ->
            help(),
            halt(1)
    end.


%% @doc Option spec list
%% @private
option_spec_list() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,    $h, "help",    undefined,  "Show the program options"},
     {src_dir, $s, "leveldb_src_dir", string, "Specify a leveldb source directory"},
     {dst_dir, $d, "leveldb_dst_dir", string, "Specify a leveldb destination directory"},
     {version, $v, "version", undefined,  "Show version information"}
    ].

