---
#Advent of Code 2018
---

-   [Thoughts on Different Parts](#markdown-header-thoughts-on-different-parts)
    -   [Day 1](#markdown-header-day-1)
    -   [Day 2](#markdown-header-day-2)
    -   [Day 3](#markdown-header-day-3)
    -   [Day 4](#markdown-header-day-4)
    -   [Day 5](#markdown-header-day-5)
    -   [Day 6](#markdown-header-day-6)
    -   [Day 7](#markdown-header-day-7)
    -   [Day 8](#markdown-header-day-8)
    -   [Day 9](#markdown-header-day-9)
    -   [Day 10](#markdown-header-day-10)
    -   [Day 11](#markdown-header-day-11)
    -   [Day 12](#markdown-header-day-12)
    -   [Day 13](#markdown-header-day-13)
    -   [Day 14](#markdown-header-day-14)

Code for [Advent of Code 2018](https://adventofcode.com/2018) written in
Haskell

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
        been intersected and ones that haven't
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

Day 12
------

This question was about Cell Automata. Parsing was again
straightforward. I used that Cell Automata form comonads as described in
[this
article](http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html).
This worked well for part1, where I also used the arrow library though
this was very unnecessary. I realised that this would not work for
part2, which required it to work on a much larger number. Unfortunately
I didn\'t quite grasp how much larger the number was (which should have
been obvious as it was huge) and so I spent a lot of time trying to
optimize it by culling of areas where there were just no pots and uses
sequences. The result was exponentially quicker though still struggled
with any amount of iterations into the thousands. It should have been
obvious earlier but if we look at the output after 200 iterations we get
(with the summing removed):

``` {.Haskell}
*Day12Part2 Day12Part2> sumIterate 200 <$> readParseFile
[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 125, 0, 127, 0, 129, 0,
131, 0, 133, 0, 135, 0, 137, 0, 139, 0, 141, 0, 143, 0, 145, 0, 147, 0, 149, 0,
151, 0, 153, 0, 155, 0, 157, 0, 159, 0, 161, 0, 163, 0, 165, 0, 167, 0, 169, 0,
171, 0, 173, 0, 175, 0, 177, 0, 179, 0, 181, 0, 183, 0, 185, 0, 187, 0, 189, 0,
191, 0, 193, 0, 195, 0, 197, 0, 199, 0, 201, 0, 203, 0, 205, 0, 207, 0, 209, 0,
211, 0, 213, 0, 215, 0, 217, 0, 219, 0, 221, 0, 223, 0, 225, 0, 227, 0, 229, 0,
231, 0, 233, 0, 235, 0, 237, 0, 239, 0, 241, 0, 243, 0, 245, 0, 247, 0, 249, 0,
251, 0, 253, 0, 255, 0, 257, 0, 259, 0, 261, 0, 263, 0, 265, 0, 267, 0, 269, 0,
271, 0, 273, 0, 275, 0, 277, 0, 279, 0, 281, 0, 283, 0, 285, 0, 287, 0, 289, 0,
291, 0, 293, 0, 295, 0, 297, 0, 0]
```

Then two things are obvious.

1.  My code could have been further optimized by letting the focus point
2.  Doing the above would not have helped at all as it is clear you were
    meant to realise that there was a pattern of every other pot
    containing a plant and they all move 1 to the right each time. Then
    it is easy to calculate any further iteration.

Day 13
------

This question was theoretically straightforward though had a lot of data
to keep track of and cases to implement which led to many bugs. Using
the example given (in day13datasmall.txt) and heavy use of `Debug.Trace`
allowed the errors to be tracked down. The second part followed easily
from the first.

Day 14
------

I originally tried to do a dynamic programming approach as specified in
[this article.](https://wiki.haskell.org/Dynamic_programming_example)
However this did not work as our recurrence relation could produce
either 1 or 2 elements and it relied on the previous data to know how
many it was producing. This meant that the haskell arrays could not
produce the indices to create the array and haskell arrays are strict in
the indices of the generating list. I wonder if there is an (unsafe)
array which does not need to do this in haskell.

I then tried some different things involving mutable vectors but in the
end using Data.Seq gave me the correct answer. This question really
wanted a data structure like C++ `Std::Vector`.
