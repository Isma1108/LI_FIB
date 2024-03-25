print_one_ten :- 
  between(1, 10, X),  % set X to 1, with choice point 
  writeln(X),  % write it out 
  fail,    % fail - so we backtrack to line 2 and get X=2 
  !.
print_one_ten.  % eventually we hit 10 and have no more choices 
                    % so we do this, which just succeeds 
