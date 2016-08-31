-module(gtin).

-export([validate/1]).

-spec validate({gtin, iodata()}) -> ok | {error | term()}.
validate({gtin, GTIN}) ->
	validate(iolist_to_binary(GTIN));
validate(GTIN) ->
	case size(GTIN) of
		8 ->
			validate_checksum(GTIN);
		12 ->
			validate_checksum(GTIN);
		13 ->
			validate_checksum(GTIN);
		14 ->
			validate_checksum(GTIN);
		_ ->
			{error, bad_size}
	end.


validate_checksum(GTIN) ->
	validate_checksum(GTIN, 0).

validate_checksum(<<CheckSum>>, Sum) ->
    case Sum * 9 rem 10 of
        M when M =:= CheckSum - $0 ->
            ok;
        _M ->
            {error, checksum_invalid}
    end;
validate_checksum(<<N, Rest/binary>>, Sum) when size(Rest) rem 2 =:= 1 ->
    validate_checksum(Rest, Sum + (N - $0) * 3);
validate_checksum(<<N, Rest/binary>>, Sum) ->
    validate_checksum(Rest, Sum + N - $0).
