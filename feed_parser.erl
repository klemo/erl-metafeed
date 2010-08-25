%%%-------------------------------------------------------------------
%%% File    : feed_parser.erl
%%% Author  : klemo <klemo@klemo-desktop>
%%% Description : Fetches feeds from various sources
%%% Created : 14 Apr 2010 by klemo <klemo@klemo-desktop>
%%%-------------------------------------------------------------------
-module(feed_parser).
-include_lib("xmerl/include/xmerl.hrl").
-export([init/0, fetch/1, filter/2, replace/2, sort/2, union/2, tail/2, unique/1, get_titles/1]).

%%%-------------------------------------------------------------------
%% Initialize feed_parser
%%%-------------------------------------------------------------------
init() ->
    ibrowse:start().

%%%-------------------------------------------------------------------
%% Feed parser API.
%% Called from metafeed interpreter module.
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% Read feed from given URL and return list of parsed feed items
%%%-------------------------------------------------------------------
fetch(Url) ->
    try xmerl_scan:file(Url) of
        {error, _} ->
            throw({fetch, "error reading fetch source", Url});
        {ParsResult, _} ->
            case length(xmerl_xpath:string("/rss", ParsResult)) of
                0 -> throw({fetch, "not an rss feed"});
                _ -> true
            end,
            xmerl_xpath:string("//item", ParsResult)
    catch
        _:_ -> throw({fetch, "error reading fetch source", Url})
    end.

%%%-------------------------------------------------------------------
%% Filter feed items that contain Text in Elements
%%%-------------------------------------------------------------------
filter({contains, Text, Elements}, Items) ->
    lists:filter(fun(X) ->
                         eval_contains(X, Text, Elements) end,
                 Items);

%%%-------------------------------------------------------------------
%% Filter feed items that do not contain Text in Elements
%%%-------------------------------------------------------------------
filter({does_not_contain, Text, Elements}, Items) ->
    lists:filter(fun(X) ->
                         not eval_contains(X, Text, Elements) end,
                 Items);

%%%-------------------------------------------------------------------
%% Filter feed items with text in Element equal to Text
%%%-------------------------------------------------------------------
filter({equal, Text, Element}, Items) ->
    lists:filter(fun(X) ->
                         eval_equal(X, Text, Element) end,
                 Items).


%%%-------------------------------------------------------------------
%% Replace Str1 with Str2 in Elements
%%%-------------------------------------------------------------------
replace({Str1, Str2, Elements}, Items) ->
    lists:map(fun(X) ->
                      replaceStr(X, Str1, Str2, Elements) end,
              Items).

%%%-------------------------------------------------------------------
%% sort Items based on Element in ascending order
%%%-------------------------------------------------------------------
sort({ascending, Element}, Items) ->
    lists:sort(fun(X, Y) ->
                       compare(ascending, Element, X, Y) end,
               Items);

%%%-------------------------------------------------------------------
%% Sort Items based on Element in descending order
%%%-------------------------------------------------------------------
sort({descending, Element}, Items) ->
    lists:sort(fun(X, Y) ->
                        compare(descending, Element, X, Y) end,
               Items).

%%%-------------------------------------------------------------------
%% Merge two feeds into union
%%%-------------------------------------------------------------------
union(Items1, Items2) ->
    %first make sure feed items are sorted on pubDate descending
    %make union by merging two lists of items using custom order func
    lists:merge(fun(X, Y) ->
                        temporal_order(X, Y) end,
                sort({descending, "pubDate"}, Items1),
                sort({descending, "pubDate"}, Items2)).

%%%-------------------------------------------------------------------
%% Take first Count feed items
%%%-------------------------------------------------------------------
tail(Count, Items) ->
    lists:sublist(Items, Count).

%%%-------------------------------------------------------------------
%% Removes duplicate items from feed
%%%-------------------------------------------------------------------
unique(Items) ->
    % first make sure feed items are sorted on guid, then remove
    % duplicates
    remove_duplicates(
      sort({ascending, "guid"}, Items)).

%%%-------------------------------------------------------------------
%% Feed parser implementation functions.
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% Removes duplicate items from feed based on guid
%% Feed must be sorted on guid before calling this function
%%%-------------------------------------------------------------------
remove_duplicates([]) ->
    [];
remove_duplicates(Items) ->
    lists:reverse(remove_duplicates(Items, [])).

remove_duplicates([Item,NextItem|T], Result) ->
    XPathExpr = "//guid/text()",
    [Node1|_] = xmerl_xpath:string(XPathExpr, Item),
    [Node2|_] = xmerl_xpath:string(XPathExpr, NextItem),
    case string:equal(Node1#xmlText.value, Node2#xmlText.value) of
        true ->
            remove_duplicates([NextItem|T], Result);
        false ->
            remove_duplicates([NextItem|T], [Item|Result])
    end;

remove_duplicates([X|_], Result) ->
    [X|Result].

%%%-------------------------------------------------------------------
%% Function for temporal ordering of two feed items
%%%-------------------------------------------------------------------
temporal_order(X, Y) ->
    XPathExpr = "//pubDate/text()",
    [Node1|_] = xmerl_xpath:string(XPathExpr, X),
    [Node2|_] = xmerl_xpath:string(XPathExpr, Y),
    httpd_util:convert_request_date(Node1#xmlText.value) >
        httpd_util:convert_request_date(Node2#xmlText.value).

%%%-------------------------------------------------------------------
%% Function for comparison of two feed items based on feed Element.
%% Type is ascending or descending.
%%%-------------------------------------------------------------------
compare(Type, Element, X, Y) ->
    XPathExpr = "//" ++ Element ++ "/text()",
    [Node1|_] = xmerl_xpath:string(XPathExpr, X),
    [Node2|_] = xmerl_xpath:string(XPathExpr, Y),
    % date comparison is special case
    Date = string:equal(Element, "pubDate"),
    if
        Date ->
            FC = fun(DateText) ->
                         httpd_util:convert_request_date(DateText)
                 end;          
        true -> FC = fun(Text) -> Text end
    end,
    case Type of
        ascending ->
            FC(Node1#xmlText.value) < FC(Node2#xmlText.value);
        descending ->
            FC(Node1#xmlText.value) > FC(Node2#xmlText.value)
    end.

%%%-------------------------------------------------------------------
%% Evaluates to true if feed Item Element equals Text
%%%-------------------------------------------------------------------
eval_equal(Item, Text, Element) ->
    XPathExpr = "//" ++ Element ++ "/text()",
    [Node|_] = xmerl_xpath:string(XPathExpr, Item),
    string:equal(Node#xmlText.value, Text).

%%%-------------------------------------------------------------------
%% Check if Item contains Text in any of Elements array
%%%-------------------------------------------------------------------
eval_contains(_, _, []) ->
    false;
eval_contains(Item, Text, [Element|RestElements]) ->
    XPathExpr = "//" ++ Element ++ "/text()",
    case contains_text(Text, xmerl_xpath:string(XPathExpr, Item)) of
        false -> eval_contains(Item, Text, RestElements);
        true -> true
    end.

%%%-------------------------------------------------------------------
%% Find text in list
%%%-------------------------------------------------------------------
contains_text(_, []) ->
    false;
contains_text(Text, [Node|Rest]) ->
    case re:run(Node#xmlText.value, Text) of
        nomatch -> contains_text(Text, Rest);
        {match, _} -> true
    end.

%%%-------------------------------------------------------------------
%% Replace strings in list
%%%-------------------------------------------------------------------
replace_str_list(Str1, Str2, List) ->
    replace_str_list(Str1, Str2, List, []).

replace_str_list(_, _, [], A) ->
    A;
replace_str_list(Str1, Str2, [Node|Rest], A) ->
    Replaced = re:replace(Node#xmlText.value, Str1, Str2, [{return,list}]),
    H = Node#xmlText{value=Replaced},
    replace_str_list(Str1, Str2, Rest, [H|A]).

%%%-------------------------------------------------------------------
%% Replace Str1 with Str2 in Elements in Item
%%%-------------------------------------------------------------------

replaceStr(Item, Str1, Str2, Element, No) ->
    XPathExpr = "//" ++ Element ++ "[" ++ No  ++ "]/text()",
    Node = xmerl_xpath:string(XPathExpr, Item),
    Replaced = re:replace(Node#xmlText.value, Str1, Str2, [{return,list}]),
    H = Node#xmlText{value=Replaced}.

%%%-------------------------------------------------------------------
%% Extracts titles from feed Items and returns them as list.
%%%-------------------------------------------------------------------
get_titles(Items) ->
    lists:reverse(get_titles(Items, [])).
get_titles([], Titles) ->
    Titles;
get_titles([Item|Rest], Titles) ->
    [Node|_] = xmerl_xpath:string("//title/text()", Item),
    #xmlText{value=Title} = Node,
    get_titles(Rest, [Title|Titles]).
