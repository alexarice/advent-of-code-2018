Advent of Code 2018
===================

Code for advent of code 2018 written in Haskell

Thoughts on Different Parts
===========================

Day 9
-----

In part 2 of this calculating scores by latest first caused the program
to stack overflow. This was solved by reversing the list before
calculating it though it is likely that a better solution would be to
deeply make GameState a strict data type. My solution was very slow.
Maybe something could be done with the Vector package though this would
make adding and popping elements slow so probably not. Ideally what was
wanted was a mutable doubly linked list but I would try making GameState
strict first and see if this solves the issues. Also we do not need to
link the scores to the GameState as this causes a lot of copying data
around so this should be separated.

Day 10
------

This was done in the most naive way possible and therefore took quite a
lot of time. The correct image was found by finding the image where the
points were closest together and then manually inspecting the images
before and after this one. Haskell lazyness should mean that not all the
images needed to be generated. This could have been sped up a lot by
using some form of binary search.

Day 11
------

The code for this was very slow. Possible improvements for this that I
can think of are:

-   Perform the even steps by adding horizontal parts together first,
    then adding vertical parts of the horizontal parts. This reduces the
    number of additions from 4 to 3
-   Find some better configuration for odd squares
-   Try making the code parallel
-   Finding some heuristic that rules out large sections
-   Taking some vastly different approach
