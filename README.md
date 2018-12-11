# Table of Contents

1.  [Advent of Code 2018](#org321daed)
2.  [Thoughts on Different Parts](#org1246d1e)
    1.  [Day 9](#orgf3054e1)
    2.  [Day 11](#org7deedfa)


<a id="org321daed"></a>

# Advent of Code 2018

Code for advent of code 2018 written in haskell


<a id="org1246d1e"></a>

# Thoughts on Different Parts


<a id="orgf3054e1"></a>

## Day 9

In part 2 of this calculating scores by latest first caused the program to stack overflow. This was solved by reversing the list before calculating it though it is likely that a better solution would be to deeply make GameState a strict data type. My solution was very slow. Maybe something could be done with the Vector package though this would make adding and popping elements slow so probably not. Ideally what was wanted was a mutable doubly linked list but I would try making GameState strict first and see if this solves the issues. Also we do not need to link the scores to the GameState as this causes a lot of copying data around so this should be separated.


<a id="org7deedfa"></a>

## Day 11

The code for this was very slow. Possible improvements for this that I can think of are:

-   Perform the even steps by adding horizontal parts together first, then adding vertical parts of the horizontal parts. This reduces the number of additions from 4 to 3
-   Find some better configuration for odd squares
-   Try making the code parallel
-   Finding some heuristic that rules out large sections
-   Taking some vastly different approach
