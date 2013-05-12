-module(c_math).
-export([fact/1, fibo/1, mathexp/1, pow/2, root/2, root/4, 
    is_fraction/1
    ]).

-import(c_util, [rep/2, prod/1]).


%%% TODO Remember to use tail recursion.

% Tail recursive factorial.
%% Factorial %%
fact(N) -> fact(N,1).
fact(1, Acc) -> Acc;
fact(N, Acc) -> fact(N-1, N*Acc).


%% Fibonacci %%
fibo(0) -> 0;
fibo(1) -> 1;
fibo(N) when N>0 -> fibo(N-1) + fibo(N-2).

fib(1,0,Acc) -> Acc;
fib(A,B,Acc) -> fib(B, x, Acc+A).
% TODO: Tail recursive fibonacci.



%% is F a fraction?
is_fraction({fraction,_,_}) -> true.

%%
float_to_fraction(Flt) -> Flt.
% This one takes a lot of work.  To do it properly requires
% number theoretic funcs such as gcd/lcm/primes_lt_n etc blah
% da.



% Goal: Create an exponential function without using any
% built in power func.

%% N^M %%
% handles integers and fractions but not floats.
pow(_,0) -> 1;
pow(N,M) when is_integer(M), M>0 -> prod(rep(N,M));
pow(N,M) when is_integer(M), M<0 -> 1/pow(N,-M);
pow(_,{fraction, _, 0})  -> undefined;
pow(_,{fraction, 0, _})  -> 1;
pow(N,{fraction, A, B}) when A*B>0 -> pow(root(N,abs(B)), abs(A));
pow(N,{fraction, A, B}) -> 1/pow(N,{fraction, abs(A), abs(B)});
pow(N,M) when is_float(M), M>0 -> pending;
pow(N,M) when is_float(M), M<0 -> 1/pow(N,-M).

%% Nth root of A %%
root(0,_) -> 0;
root(1,_) -> 1;
root(_,0) -> 1;
root(A,1) -> A;
root(A,N) when N>1, is_integer(N) -> root(A,N,N,N).


%Delta() -> 0.00001.
% TODO make this a simple assignment.
-define(Delta, 0.000001).

% The actual calculations.
root(A,N,N,N) -> root(A,N, A/N, N); % initial guess.

root(A,N, Xk, Diff) when Diff > ?Delta -> 
    Xk1 = (1/N)* ((N-1)*Xk + A/pow(Xk,N-1)),
    root(A,N, Xk1, abs(Xk1-Xk));

root(_,_, Xk, Diff) when Diff =< ?Delta -> Xk.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Attempting to rewrite root/4 as a single function and
%% failing.  Writing this way (with if expressions) leads to
%% convoluted, hard-to-follow code.
% rb(A, N, Xk, Diff) ->
%     Xk1 = (1/N)* ((N-1)*Xk + A/pow(Xk,N-1)),
%     if N =:= Xk, Xk =:= Diff -> rb(A,N, A/N, N); % initial guess.  
%     true ->
%         if Diff =< 0.0000001 -> Xk,
%         true -> rb(A,N, Xk1, abs(Xk1-Xk));
%         end;
%     end.


mathexp({sum, N,M}) -> N+M ;
mathexp({sub, N,M}) -> N-M ;
mathexp({pow, N,M}) -> pow(N,M) ;
mathexp({square, N}) -> N*N.


