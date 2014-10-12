% Functions and tests for Assignment 6, Part 2: MusixXMatch Dataset in AP2014
% Written by Martin Jrgensen, tzk173
%        and Casper B Hansen, xxxyyy

-module(part2).
-export([lsum/2]).


lsum([]   , Sum) -> Sum;
lsum([H|T], Sum) -> lsum(T, H + Sum).
