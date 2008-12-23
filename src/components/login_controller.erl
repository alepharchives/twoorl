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

-module(twoorl.login_controller).

-import(crypto).
-import(yaws_arg).
-import(erlyweb_forms).
-import(erlyweb_util).

-compile(export_all).
-include("twoorl.hrl").

index(A) ->
    case yaws_arg:method(A) of
	'POST' ->
	    {[Usr, Password], Errs} =
		erlyweb_forms:validate(
		  A,
		  ["username", "password"],
		  fun(Field, Val) ->
			  case Val of
			      [] -> {error, {missing_field, Field}};
			      _ ->
				  case Field of
				      "username" ->
					  get_usr(Val);
				      _ ->
					  ok
				  end
			  end
		  end),
	    Errs1 = 
		if Errs == [] ->
%%			twoorl_stats:cast({record, site_login}),
			Hash = crypto:sha([usr:username(Usr), Password]),
			case usr:password(Usr) of
			    Hash ->
				[];
			    _ ->
				[invalid_login]
			end;
		   true ->
			Errs
		end,
	    if Errs1 == [] ->
		    do_login(A, Usr);
	       true ->
		    [?Data(A, undefined),
		     {ewc, ui_msgs, [A, Errs1]}]
	    end;
	_ ->
	    [?Data(A, undefined), {data, []}]
    end.

get_usr(Username) ->
    Usr = usr:find_first({username,'=',Username}),
    if Usr == undefined ->
	    {error, invalid_login};
       true ->
	    {ok, Usr}
    end.

do_login(A, Usr) ->
    Key = util:gen_key(),
    LangCookie = erlyweb_util:get_cookie("lang", A),
    Usr1 = case LangCookie of
	       undefined ->
		   Usr;
	       Lang ->
		   LangBin = list_to_binary(Lang),
		   case Usr:language() of
		       LangBin ->
			   Usr;
		       _ ->
			   spawn(fun() -> 
					 usr:update([{language, Lang}],
						    {id,'=',Usr:id()})
				 end),
			   usr:language(Usr, Lang)
		   end
	   end,

    util:update_session(A, Usr1, Key),
    spawn(fun() ->
		  usr:update([{session_key, Key}], {id,'=',usr:id(Usr1)})
	  end),
    Response = [util:cookie("key", Key)],
    
    %% set the language cookie for the session if it's not defined
    Response1 = if LangCookie == undefined ->
			case Usr1:language() of
			    undefined ->
				Response;
			    Other ->
				[util:cookie("lang", Other) | Response]
			end;
		   true ->
			Response
		end,
    {response, [{ewr, home} | Response1]}.

