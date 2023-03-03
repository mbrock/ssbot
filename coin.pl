:- module(coin, []).

:- use_module(apis).
:- use_module(secrets).

% https://api.etherscan.io/api
%    ?module=account
%    &action=tokentx
%    &contractaddress=0x9f8f72aa9304c8b593d555f12ef6589cc3a579a2
%    &address=0x4e83362442b8d1bec281594cea3050c8eb01311c
%    &page=1
%    &offset=100
%    &startblock=0
%    &endblock=27025780
%    &sort=asc
%    &apikey=YourApiKeyToken

etherscan_api_token -->
    {secret(etherscan, Token)},
    [apikey=Token].

etherscan_params(erc20_transfers(Address)) -->
    etherscan_api_token,
    [module=account],
    [action=tokentx],
    [address=Address],
    [page=1],
    [offset=100],
    [startblock=0],
    [endblock=99999999],
    [sort=asc].

etherscan(Query, Result) :-
    phrase(etherscan_params(Query), Params),
    api_get(etherscan, [api], Params, Result).
