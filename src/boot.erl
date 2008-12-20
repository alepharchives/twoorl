%% This file is part of Twoorl.
%% 
%% Twoorl is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% Twoorl is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with Twoorl.  If not, see <http://www.gnu.org/licenses/>.
%%
%% Copyright Yariv Sadan, 2008
%%
%% @author Yariv Sadan <yarivsblog@gmail.com> [http://yarivsblog.com]
%% @copyright Yariv Sadan, 2008

-module(boot).
-compile(export_all).
-include("twoorl.hrl").
-include("app.hrl").

start() ->
    process_flag(trap_exit, true),
    Inets = (catch application:start(inets)),
    Crypto = (catch application:start(crypto)),
    Mnesia = (catch application:start(mnesia)),
    start_phase(mysql, normal, [{?DB_HOSTNAME, ?DB_USERNAME, ?DB_PASSWORD, ?DB_DATABASE, ?DB_POOL_SIZE}]),
    start_phase(mnesia, normal, [session]),
    start_phase(compile, normal, []),
    [{inets, Inets}, {crypto, Crypto}, {mnesia, Mnesia}].

start(_Type, Args) ->
    twoorl_sup:start_link([Args]).

start_phase(mysql, _, DBConfigs) ->
    [mysql_connect(Hostname, User, Password, Database, PoolSize)
     || {Hostname, User, Password, Database, PoolSize} <- DBConfigs],
    ok;

start_phase(compile, _, _) ->
    compile(),
    ok;

%% Having the mnesia store on a separate but connected node with a module
%% to handle its maintenance would move a lot of this foo out of the
%% application stack. Eventually that really needs to happen. -- nkg
start_phase(mnesia, _, Tables) ->
    %% Mnesia should have been started already, because of that the schema
    %% is in memory if the schema doesn't already exist on disc. If so we
    %% change the type so that it writes to the mnesia dir we set. -- nkg
    case mnesia:table_info(schema, storage_type) of
        ram_copies -> 
            mnesia:change_table_copy_type(schema, node(), disc_copies);
        _ ->
            ok
    end,
    ExistingTables = mnesia:system_info(tables) -- [schema],
    [create_table(Table) ||
	Table <- Tables, not lists:member(Table, ExistingTables)],
    ok.

create_table(session) ->
    mnesia:create_table(session, [{attributes, record_info(fields, session)}]),
    ok.

mysql_connect(Hostname, User, Password, Database, PoolSize) ->
    erlydb:start(
      mysql, [{hostname, Hostname},
	      {username, User},
	      {password, Password},
	      {database, Database},
	      {logfun, fun twoorl_util:log/4}]),
    lists:foreach(
      fun(_PoolNumber) ->
	      mysql:connect(erlydb_mysql, Hostname, undefined, User, Password,
			    Database, true)
      end, lists:seq(1, PoolSize)).

compile() ->
    compile([]).

compile_dev() ->
    compile([{auto_compile, true}]).

compile_update() ->
    compile([{last_compile_time, auto}]).

compile(Opts) ->
    erlyweb:compile(compile_dir(default),
		    [{erlydb_driver, mysql}, {erlydb_timeout, 20000} | Opts]).

compile_dir(auto) ->
    {ok, CWD} = file:get_cwd(), CWD;
compile_dir(default) ->
    ?APP_PATH;
compile_dir(appconfig) ->
    {ok, CDir} = application:get_env(twoorl, compile_dir),
    CDir;
compile_dir(Dir) ->
    Dir.

