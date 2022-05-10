# Loop

A interpreter for the loop programming language

## Loop Syntax


```
<loop_program> ::= <assignment> ;<loop_program>| <for_loop> <loop_program> | ε
<assignment> ::= <var> := 0 | <var> := 1 | <var> := <var> | <var> := <var> + 1 | <var> := <var> - 1

<assignment> ::= <var> := <id>(<variable_list>)
<variable_list> ::= <var> , <variable_list> | <var>
<for_loop> ::= for <var> := 0 to <var> do <loop_program> done
<for_loop> ::= for <var> := 1 to <var> do <loop_program> done

```
Comments are lines that start with "--"  

## Some semantic stuff
- Programs that need input use the variables i1, i2, i3, ...
- Programs that write output write to the variable o1
- Programs that get have a definition of the type x := add(x, y) need to provide
--lib add.loop with the definition of the add program of the function exactly