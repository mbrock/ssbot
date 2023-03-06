:- module(coin, [etherscan_query_page_result/3,
                 etherscan_query_page_rows/3,
                 etherscan_query_rows/2
                ]).

:- use_module(library(lists), [member/2]).

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

etherscan_page_size(100).

etherscan_api_token -->
    {secret(etherscan, Token)},
    [apikey=Token].

etherscan_params(erc20_transfers(Address), Page) -->
    etherscan_base_params(Page),
    [module=account],
    [action=tokentx],
    [address=Address].

etherscan_params(txlist(Address), Page) -->
    etherscan_base_params(Page),
    [module=account],
    [action=txlist],
    [address=Address].

etherscan_base_params(Page) -->
    etherscan_api_token,
    {etherscan_page_size(PageSize)},
    [page=Page],
    [offset=PageSize],
    [startblock=0],
    [endblock=99999999],
    [sort=asc].

etherscan_query_page_result(Query, Page, Result) :-
    phrase(etherscan_params(Query, Page), Params),
    api_get(etherscan, [api], Params, Result).

etherscan_query_page_rows(Query, Page, Rows) :-
    between(1, inf, Page),
    etherscan_query_page_result(Query, Page, Result),
    Rows = Result.result,
    length(Rows, Len),
    etherscan_page_size(PageSize),
    (Len < PageSize -> ! ; true).

etherscan_query_row(Query, Row) :-
    etherscan_query_page_rows(Query, _Page, Rows),
    member(Row, Rows).

etherscan_query_rows(Query, Rows) :-
    findall(Row, etherscan_query_row(Query, Row), Rows).
