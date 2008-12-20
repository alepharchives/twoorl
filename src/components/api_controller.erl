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

-module(twoorl.api_controller).
-export([send/1, follow/1, toggle_twitter/1]).
-include("twoorl.hrl").

send(A) ->
    util:auth(
      A,
      fun(Usr) ->
	      Params = .yaws_api:parse_post(A),
	      {[Body], Errs} =
		  .erlyweb_forms:validate(
		    Params, ["msg"],
		    fun("msg", Val) ->
			    case Val of
				[] ->
				    {error, empty_msg};
				_ ->
				    %% helps avoid DOS
				    {ok, .lists:sublist(Val, ?MAX_MSG_SIZE)}
			    end
		    end),
	      case Errs of
		  [] ->
		      {Body1, BodyNoLinks, RecipientNames} =
			  msg:process_raw_body(Body),

		      TwitterEnabled = Usr:twitter_enabled() == 1,
		      TwitterStatus = 
			  if TwitterEnabled ->
				  ?TWITTER_SENT_PENDING;
			     true ->
				  ?TWITTER_NOT_SENT
			  end,
		      Msg = msg:new_with([{usr_username, Usr:username()},
					  {usr_id, Usr:id()},
					  {body, Body1},
					  {body_nolinks, BodyNoLinks},
					  {body_raw, Body},
					  {usr_gravatar_id,
					   util:gravatar_id(
					     Usr:email())},
					  {usr_gravatar_enabled,
					   Usr:gravatar_enabled()},
					  {twitter_status, TwitterStatus},
					  {spam, Usr:spammer()}]),
		      Msg1 = Msg:save(),

%%		      .twoorl_stats:cast({record, twoorl}),

		      if TwitterEnabled andalso RecipientNames == [] ->
			      spawn(twoorl_twitter, send_tweet, [Usr, Msg1]);
			 true ->
			      ok
		      end,

		      spawn(
			fun() ->
				RecipientIds = 
				    usr:find(
				      {username, in,
				       .lists:usort(
					 [Name || Name <- RecipientNames])}),
				reply:save_replies(Msg1:id(), RecipientIds)
			end),
		      
		      case .proplists:get_value("get_html", Params) of
			  "true" ->
			      Msg2 = msg:created_on(
				       Msg1, .calendar:local_time()),
			      {ewc, timeline, show_msg, [A, Msg2]};
			  _ ->
			      {data, "ok"}
		      end;
		  _ ->

		      %% TODO need decent error reporting
		      exit(Errs)
	      end
      end).

follow(A) ->
    util:auth(
      A,
      fun(Usr) ->
	      {[{Username, OtherUsrId}, Val], Errs} =
		  .erlyweb_forms:validate(
		    A, ["username", "value"],
		    fun(F, V) ->
			    if V == [] ->
				    {error, {missing_field, F}};
			       F == "username" ->
				    OtherUsr = usr:find_first(
						 {username,'=',V}),
				    if OtherUsr == undefined ->
					    {error, {invalid_value, {F, V}}};
				       true ->
					    {ok, {V, OtherUsr:id()}}
				    end;
			       F == "value" ->
				    if V == "0" orelse V == "1" ->
					    {ok, V == "1"};
				       true ->
					    {error, {invalid_value, {F, V}}}
				    end
			    end
		    end),
	      Errs2 =
		  if Errs == [] ->
			  if Val ->
				  following:insert(
				    following:new_with(
				      [{usr_id1,Usr:id()},
				       {usr_id2,OtherUsrId},
				       {usr_username1,Usr:username()},
				       {usr_username2,Username}])),
				  [];
			     true ->
				  case following:delete_where(
					 {'and',
					  [
					   {usr_id1,'=',Usr:id()},
					   {usr_id2,'=',OtherUsrId}]}) of
				      0 ->
					  [{error, {not_following, Username}}];
				      1 ->
					  []
				  end
			  end;
		     true ->
			  Errs
		  end,

	      %% TODO need decent error reporting
	      if Errs2 == [] ->
		      {data, "ok"};
		 true ->
		      exit(Errs2)
	      end
      end).

toggle_twitter(A) ->
    util:auth(
      A,
      fun(Usr) ->
	      Params = .yaws_api:parse_post(A),
	      Enabled =
		  case .proplists:get_value("value", Params) of
		      "true" -> 1;
		      "false" -> 0;
		      %% TODO need better error handling
		      Val -> exit({unexpected_value, Val})
		  end,
	      Usr1 = usr:twitter_enabled(Usr, Enabled),
	      util:update_session(A, Usr1),
	      usr:update([{twitter_enabled, Enabled}], {id,'=',Usr:id()}),
	      {response, [{html, <<"ok">>}]}
      end).
	      
