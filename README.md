Advent of Code 2018
===================

Code for advent of code 2018 written in Haskell

Thoughts on Different Parts
===========================

Day 1
-----

The first part was a simple sum. For the second part my answer was very
slow as it just created all intermediate sums (creating an infinite list
in Haskell) and then checking for duplicates. This can be made quicker
by noticing that we can add combined sum of the list on to one set of
intermediate sums.

Day 2
-----

Computed checksum by sorting lists and gathering equal elements. Did
second part by checking all pairs

Day 3
-----

Part 1 was done by listing all the points in each rectangle and
gathering them as in the previous day. To do part 2 I had algorithm
which did a following,

1.  Take a rectangle
2.  Get a list of all the rectangles this intersects
    -   If this list is empty then we have found the answer
    -   Otherwise partition the remaining Rectangles into ones that have
        been intersected and ones that haven\'t
3.  Use the intersected rectangles as a queue, popping off the top, and
    adding all rectangles that it intersects with (out of those which
    have not been added to the queue yet) to the back of the queue
4.  Repeat this until the queue is empty at which point we are left with
    some Rectangles that have no intersection with any of the rectangles
    we have removed and every rectangle removed had an intersection with
    some other rectangle
5.  Now go back to 1. with this reduced list

Day 4
-----

Most of the work here was parsing the input file, though this was simple
with `Parsec`. For the actual question I used `Data.Map` to sum up
amounts of times guards were sleeping.

Day 5
-----

This task was really about free groups. Although I didn't realise that
there was a built in free group library in Haskell, it still allowed me
to see that as removing a letter is a group homomorphism and reducing
the element just takes it to a different representation of the same
element, that

``` {.Haskell}
reduce . filter ((/=x) . toLower) . reduce = reduce . filter ((/=x) . toLower)
```

Day 6
-----

For the first part I used the observation that any coordinate that was
closest to some point on the boundary was also closest to infinitely
more points by the properties of taxicab distance.

For the second part my solution was quite slow, in that it was basically
the naive solution except noticing that the grid we needed to consider
points that were at most `` 10000 `div` n `` outside the grid instead of
`10000` where n is the number of coordinates we had. This could have
been sped up by cutting out large rectangles that were all safe at a
time or using some heuristic to easily do the middle, leaving only the
edge cases.

Day 7
-----

This was a bit of a mess. I originally tried using a tree but this
didn\'t work well as we needed a tree where child nodes could be shared.
Ultimately I kept a tag on each element keeping track of how many
dependencies were unsatisfied. I decided to keep this in a mutable array
mostly because I wanted to see how they worked in Haskell. The second
part built on this by also passing around a list of workers which
specified what they were working on and how much they had left to do. I
also passed round a list of things that were ready to do. Finally for
part 2 I realised I didn\'t need to go through each second but could
take the minimum out of the remaining time each worker needed to finish
and move forward by that amount of time. In the end the mutable array
proved to be a decent way of doing this.

Day 8
-----

This was straightforward with the power of the `Text.Parsec` library and
using the built in `Data.Tree` library in Haskell

Day 9
-----

In part 2 of this calculating scores by latest first caused the program
to stack overflow. This was solved by reversing the list before
calculating it though it is likely that a better solution would be to
deeply make `GameState` a strict data type. My solution was very slow.
Maybe something could be done with the `Data.Vector` package though this
would make adding and popping elements slow so probably not. Ideally
what was wanted was a mutable doubly linked list but I would try making
`GameState` strict first and see if this solves the issues. Also we do
not need to link the scores to the `GameState` as this causes a lot of
copying data around so this should be separated.

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
