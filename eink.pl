:- module(nt_eink, eink_show/1).
:- use_module(apis).

% The `urbion' computer has an e-ink display.
% It listens for requests on http://urbion/epap/DISPLAY-LATEX.
% You send a LaTeX document and it displays it.

eink_show(latex(Latex)) :-
    api_post(urbion, ["DISPLAY-LATEX"], text(Latex), _).
