(*

Questions:

1. What are the types of the following expressions and what do they evaluate to and why?

17
is of type int
evaluates to 17 because simple types evaluate to themselves

1 + 2 * 3 + 4
is of type int because the arithmetic operators +, * accept and return integers
evaluation:
1 + (2 * 3) + 4
1 + 6 + 4
7 + 4
11

800 / 80 / 8
is of type int because / accepts and returns integers
evaluation:
800 / 80 / 8
10 / 8
1 (the integer quotient of 10 and 8)

400 > 200
is of type bool
evaluates to true because 400 is greater than 200

1 <> 1
is of type bool
evaluates to false because 1 is not not equal to 1

true || false
is of type bool
evaluates to true because one of the arguments to || is true

true && false
is of type bool
evaluates to false because both of the arguments to && are not true

if true then false else true
is of type bool
evaluates to false because true is always true

'%'
is of type char
evaluates to '%' because chars evaluate to themselves

'a' + 'b'
is not well typed
this is a type error because + is only defined for integers

2. Consider the evaluations of the expressions 1 + 2 mod 3, (1 + 2) mod 3, and 1 + (2 mod 3).
What can you conclude about the + and mod operators?

1 + 2 mod 3
evaluates to 3

(1 + 2) mod 3
evaluates to 0

1 + (2 mod 3)
evaluates to 3

so we can conclude that mod has a higher precedence than +

3. A programmer writes 1+2 * 3+4. What does this evaluate to? What advice would you give them?

1+2 * 3+4 evalutates to
1 + (2 * 3) + 4
1 + 6 + 4
7 + 4
11

I would advise that the programmer use parentheses to achieve the grouping of evaluation they want:
(1 + 2) * (3 + 4)
3 * 7
21

4. The range of numbers available is limited. There are two special numbers: min_int and max_int.
What are their values on your computer? What happens when you evaluate the expressions max_int + 1
and min_int - 1?

max_int
4611686018427387903

min_int
-461168601842738790

max_int + 1 evaluates to min_int
min_int - 1 evaluates to max_int

5. What happens when you try to evaluate the expression 1 / 0? Why?
This expression raises a Division_by_zero expression because dividing by zero is undefined

6. Can you discover what the mod operator does when one or both of the operands are negative?
What about if the first operand is zero? What if the second is zero?

If the arguments to mod are both negative the result is the negative of the result if they were both positive
If the first argument to mod is negative the result is the negative of the result if both arguments were positive
If the second argument to mod is negative it is the same as if it were not negative
If the first argument to mod is zero the result is 0
If the second argument to mod is zero it raises a Division_by_zero exception

7. Why not just use, for exmaple, the integer 0 to represent false and the integer 1 for true?
Why have a separate bool type at all?

Without a bool type logical functions would have the type int -> int -> int and then you would need to worry
about if the input the function was given was neither of the values 0 or 1 because the type int includes all
of the values from min_int to max_int. By having a separate bool type only valid values to logical functions
are well typed.

8. What is the effect of the comparison operators like < and > on alphabetic values of type char? What is the
effect of the comparison operators on the booleans true and false?

Looks ike chars are compared lexicographically (presumably based on their ascii code value)
true > false
*)
