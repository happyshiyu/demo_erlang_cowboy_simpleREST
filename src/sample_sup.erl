-module(sample_sup).
-behaviour(supervisor).

-include("sample.hrl").

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    SupFlags = #{strategy => one_for_one
                ,intensity => 5
                ,period => 10
                },
    Children = [?WORKER(sample_crud_store)
               ],
    {ok, {SupFlags, Children}}.
