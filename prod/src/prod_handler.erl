-module(prod_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include_lib("xmerl/include/xmerl.hrl").

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.


handle(Req, State=#state{}) ->
	{ok, Body, Req2} = cowboy_req:body(Req),
	{XML, _Misc} = xmerl_scan:string(binary_to_list(Body)),
	Attributes = [
		{"PROD_COVER_GTIN", required},
		{"PROD_NAME", required},
		{"PROD_DESC", optional},
		{"BRAND_OWNER_NAME", optional}
	],
	{ok, Req3} = case collect_attribute_values(Attributes, XML) of
		{not_found, Attribute} ->
			cowboy_req:reply(400, [], Attribute ++ " not found", Req2);
		Values ->
			ok = write_csv(Values),
			cowboy_req:reply(200, [], "ok", Req2)
	end,
	{ok, Req3, State}.


terminate(_Reason, _Req, _State) ->
	ok.


collect_attribute_values(Attributes, XML) ->
	collect_attribute_values(Attributes, [], XML).


collect_attribute_values([], Values, _XML) ->
	lists:reverse(Values);
collect_attribute_values([{Attribute, Requirement} | Rest], Values, XML) ->
	case get_attribute_value(Attribute, XML) of
		{ok, Value} ->
			collect_attribute_values(Rest, [Value | Values], XML);
		{error, not_found} when Requirement =:= required ->
			{not_found, Attribute};
		{error, not_found} ->
			collect_attribute_values(Rest, ["" | Values], XML)
	end.


get_attribute_value(Attribute, XML) ->
	AttributePath = [$/ | string:join([
		"S:Envelope",
		"S:Body",
		"ns2:GetItemByGTINResponse",
		"ns2:GS46Item",
		"DataRecord",
		"record",
		"BaseAttributeValues",
		"value[@baseAttrId=\"" ++ Attribute ++ "\"]",
		"@value"
	], "/")],
	case xmerl_xs:select(AttributePath, XML) of
		[#'xmlAttribute'{value = Value}] ->
			{ok, Value};
		[] ->
			{error, not_found}
	end.


write_csv(Data) ->
	Line = [unicode:characters_to_binary(string:join(Data, ",")), "\n"],
	file:write_file("prod.csv", Line, [append]).
