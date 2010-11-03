%%%-------------------------------------------------------------------
%%% File    : utils.erl
%%% Author  : klemo <klemo.vladimir@gmail.com>
%%% Description : Misc utility functions for erl-metafeed
%%% Created : 25 Aug 2010 by klemo <klemo.vladimir@gmail.com>

%%% erl-metafeed, Copyright (C) 2010 Klemo Vladimir

%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as published
%%% by the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.

%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.

%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

%%%-------------------------------------------------------------------
-module(utils).

-include_lib("xmerl/include/xmerl.hrl").

-include("mf.hrl").

-export([log/2, rpc/2, get_titles/1, generate_feed/3,
         gen_rss/2, gen_json/2, get_element/2, read_file/1,
         random_seed/0, on_exit/2]).


-ifdef(debug).
-define(LOG(Msg, Args), io:format(Msg, Args)).
-else.
-define(LOG(Msg, Args), ok).
-endif.

%%%-------------------------------------------------------------------
%% Logs message
%%%-------------------------------------------------------------------

log(Msg, Args) ->
    ?LOG(Msg, Args).

%%%-------------------------------------------------------------------
%% Remote procedure call
%% Send request to a server and wait for response
%%%-------------------------------------------------------------------

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        Response ->
            Response
    end.

%%%-------------------------------------------------------------------
%% Handle process crashing
%% From book Pragmatic Programming Erlang
%%%-------------------------------------------------------------------
on_exit(Pid, Fun) ->
    spawn(fun() -> 
		  process_flag(trap_exit, true),
		  link(Pid),
		  receive
		      {'EXIT', Pid, Why} ->
			  Fun(Why)
		  end
	  end).

%%%-------------------------------------------------------------------
%% Extracts titles from feed Items and returns them as list.
%%%-------------------------------------------------------------------
get_titles({_, Items}) ->
    lists:reverse(get_titles(Items, [])).

get_titles([], Titles) ->
    Titles;
get_titles([Item|Rest], Titles) ->
    [Node|_] = xmerl_xpath:string("//title/text()", Item),
    #xmlText{value=Title} = Node,
    get_titles(Rest, [Title|Titles]).

%%%-------------------------------------------------------------------
%% Dispaches Feed generation based on feed Format
%%%-------------------------------------------------------------------
generate_feed(Feed, rss, Id) ->
    gen_rss(Feed, Id);

generate_feed(Feed, {json, JSONP}, _) ->
    gen_json(Feed, JSONP).

%%%-------------------------------------------------------------------
%% Generates rss xml document from parsed feed
%% {RSS element Attrs} = Meta
%%%-------------------------------------------------------------------
gen_rss({Meta, Items}, Id) ->
    RSSElement = wrap_rss(Meta, Id),
    RSSText = lists:flatten(
                xmerl:export_content(Items, xmerl_xml)
               ),
    io_lib:format("<?xml version=\"1.0\" encoding=\"UTF-8\"?>~n~ts~ts~n</channel>~n</rss>",
                  [RSSElement,
                   binary_to_list(unicode:characters_to_binary(RSSText))]).

wrap_rss(Meta, Id) ->
    %% remove duplicates from rss element attributes
    UMeta = lists:ukeysort(2, Meta),
    %% generate rss element attributes xml
    Attrs = lists:map(
             fun(X) ->
                    atom_to_list(X#xmlAttribute.name) ++ "=\"" ++ X#xmlAttribute.value ++ "\" " end,
             UMeta),
    ChannelElement = gen_rss_channel(Id),
    io_lib:format("<rss ~ts>~n~ts", [lists:flatten(Attrs), ChannelElement]).

%% generates rss channel element xml representation
gen_rss_channel(Id) ->
    % TODO: this should be propagated from do(Query)
    case persistence:get_metafeed(Id) of
        {atomic, Resp} ->
            case Resp of
                [] ->
                    Title = "erl-metafeed",
                    Link = "http://gitorious.org/erl-metafeed",
                    Desc = "erl-metafeed feed";
                [#metafeed{name=Name, description=Desc}] ->
                    Title = Name,
                    Link = "/feed/" ++ Id,
                    Desc = Desc
            end
    end,
    io_lib:format(
      "<channel>\n"
      "<title>~ts</title>\n"
      "<link>~ts</link>\n"
      "<description>~ts</description>\n",
      [Title, Link, Desc]).

%%-------------------------------------------------------------------------
%% @doc
%% Generates json from parsed feed
%% @end
%%-------------------------------------------------------------------------
gen_json({_, Items}, Jsonp) ->
    JsonBody = rfc4627:encode(xml_to_json(Items)),
    case Jsonp of
        {ok, JsonCallback} ->
            JsonCallback ++ "(" ++ JsonBody ++ ")";
        undefined ->
            JsonBody
    end.

xml_to_json(Items) ->
    ItemsJson = lists:map(fun(Item) ->
                                  item_extract_elements(Item) end,
                          Items),
    {obj, [{"items", ItemsJson}]}.

item_extract_elements(Item) ->
    Elements = [title, link, description, pubDate],
    Content = lists:map(fun(E) ->
                                ElJson = get_element(E, Item),
                                {atom_to_binary(E, unicode), ElJson} end,
                        Elements),
    {obj, Content}.

get_element(E, Item) ->
    XPathExpr = "//" ++ atom_to_list(E) ++ "/text()",
    [Str|_] = json(xmerl_xpath:string(XPathExpr, Item)),
    Str.

%% convert xmerl structures to rfc4627 compatible structure
json(List) when is_list(List) ->
    json_1(List, []);

json(#xmlElement{name=Name, content=Content}) ->
    Res = json(Content),
    case length(Res) of
        1 ->
            [H|_] = Res,
            NewContent = H;
        _ ->
            NewContent = Res
    end,
    {obj, [{atom_to_binary(Name, unicode), NewContent}]}.

%% ----------

json_1([], Acc) ->
    lists:reverse(Acc);

json_1([#xmlElement{name=Name, content=Content}=Element|Rest], Acc) ->
    IsExport = lists:any(fun(X) ->
                                 X == Name end,
                         [item, title, link, description, pubDate]),
    case IsExport of
        true ->
            json_1(Rest, [json(Element)|Acc]);
        false ->
            json_1(Rest, Acc)
    end;

json_1([#xmlText{value=Value1}, #xmlText{value=Value2}|Rest], Acc) ->
    json_1([#xmlText{value = Value1 ++ Value2} | Rest], Acc);

json_1([#xmlText{value=Value}|Rest], Acc) ->
    %% skip all empty elements
    case string:span(Value, "\n\t") of
        0 ->
             json_1(Rest, [list_to_binary(xmerl_ucs:to_utf8(Value))|Acc]);
        _ ->
             json_1(Rest, Acc)
    end;

json_1([_Other|Rest], Acc) -> 
    json_1(Rest, Acc).

%%%-------------------------------------------------------------------
%% Read contents of local text file
%%%-------------------------------------------------------------------
read_file(FileName) ->
    case file:open(FileName, [read]) of
        {ok, Device} -> read_lines(Device, "");
        {error, _} -> {error}
    end.

read_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> read_lines(Device, Accum ++ Line)
    end.

%% seed random number generator
random_seed() ->
    {_,_,X} = erlang:now(),
    {H,M,S} = time(),
    H1 = H * X rem 32767,
    M1 = M * X rem 32767,
    S1 = S * X rem 32767,
    put(random_seed, {H1,M1,S1}).
