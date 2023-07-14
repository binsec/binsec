# How to \#2: Read the SSE exploration board

In this post, we will introduce the Symbolic Execution exploration board.
As of today, BINSEC is not an interactive program. Basically, you configure
it, run it and finally get the output in the logs. Yet, it can be quite
frustrating when the logs say nothing, and everything -- except the CPU usage
-- seems to be frozen.  
We now propose to follow in real time -- a part of -- what is happening
behind the scene.

### Prerequisites

BINSEC must be compiled with the `curses` library.  
If it is not already the case, simply run the following:
```console
$ opam install curses
```
:warning:
*You will also have to rebuild BINSEC if you are working from sources.*

### First illustration

We are now ready to try this feature. We can simply start by running an
existing SSE script, for example, let us run BINSEC on the
[2nd 2015 Flare-On challenge](../../examples/sse/flare-on/2015.2/README.md).

Nothing ground breaking, everything should work as before, except for the
very first line of log.

```console
$ binsec -sse -sse-legacy-engine -sse-script crackme.ini very_success.exe
[sse:info] TTY: press [space] to switch between log and monitor modes.
```
Here is what we get if we have the courage to press the `space bar` on
the keyboard.

![](exploration_board.png)

*Cursed things are so beautiful...*

We have what we want: the figures are updated in real-time and we no longer
wonder whether or not the execution is stuck.  
In the following, we explain how to read and use this board to quickly detect
unwanted behaviors.

:information_source: *Nothing is lost, pressing the `space bar` again will
switch back to the standard log.*

### Paths

The number of paths is an important metric in symbolic execution.
Path explosion is the first issue that comes to mind when talking about
limitations. Indeed, each **branching point** may create an additional path
to explore -- in the worst case, the number of paths is exponential in
the number of conditional jumps. So, when the number of **pending paths**
sky-rockets, it may be a sign that symbolic inputs should be more
constrained in order to explore fewer but more significant paths.

:warning: **Discontinued paths** are the ones for which BINSEC has given up.
It may arise either because an non supported instruction has been met
or because the **maximal instruction depth** has been reached
or because the **SMT solver** failed to answer a satisfiability query.

### Exploration depth

We report here the instruction depth of the longest visited path so far
(**max reached depth**).
This metric is important since depth limit is used to prevent the analysis
being trapped in an infinite loop. Yet, the downside is that a path may be
thrown away before completion if the depth limit is too small.  
By default, this limit is set to `1000` assembly instructions,  but it
is recommanded to manually set a tailored depth limit with the
command-line option `-sse-depth`.

### Solver timeout

SMT solvers have become very powerful. Still, they are often pointed at as
a major bottleneck of symbolic execution since a satisfiability query can
use large amounts of ressources, both in time and memory.  
By default, BINSEC runs each query with a timeout of `5s`. This parameter
can be set with a higher value using the command-line option
`-fml-solver-timeout`.

:information_source: Timeouts can be totally disabled by using the
special value `0`.

Performance may be improved by choosing another SMT solver.
By default, BINSEC tries to detect one of available supported solvers,
but it can be manually set with the command-line option `-smt-solver`.  
Also remember that BINSEC can be directly compiled with a native support to
the **bitwuzla** solver.
```console
$ opam install bitwuzla
```

### Execution speed

Analysis time is spent on both the symbolic emulation of the program
and the constraint solving of path predicates. The screen gives a view
of the ratio between the two. According to past observations,
we spend most of the time on the satisfiability queries.
Yet, if concrete code emulation is faster than symbolic reasoning,
it is dramatically slower than real execution
-- it can easily be a million factor.  
*When it is possible, it is always good to start the exploration from
the point closest to the function under analysis instead of choosing
a generic entry point like `main`.* This can be achieved by using
the command-line option `-entrypoint` or the script directive
`starting from`.

:bell: The attentive readers will have noticed that most of the time
is spent in exploration instead of reasoning in the illustrative example.  
In most case, you will get the best out of BINSEC not using the option
`-sse-legacy-engine`.  
Well... here, you may not even have the time to switch between screen ;-)

### Miscellaneous

Other reported metrics include the number of visited 
**unique assembly instructions** which is related to the program coverage
in opposition to the **total of visited instructions** that sum up all of
them, regardless of the path involved, since the start of the analysis.
It is likely a bug if none of the instruction metrics nor the solver
reasoning time increase. The **emulation speed** shows how many instructions
are handled in one second, which heavily depends on the number and difficulty
of solver queries.  
There are also the number of visited **assertions** and those for which
it is possible to find a counter example (**failed** ones).
Finally, the board reports the number of queries issued by the analysis:
assumptions, assertions, conditional or dynamic jumps.
Each query is first analysed by preprocessing simplifications that
may be enough to figure out the solution (**sat** or **unsat**) or to assign
a valid value (**enum**). Otherwise, the query is discharged to an SMT solver.

### Conclusion

The exploration board makes it possible to monitor how the analysis performs.
It may help to determine pretty quickly that the current configuration is not
suitable to the target binary and we hope this post helped you to figure out
on which elements you can play with. It is worth remembering that
**discontinued paths** means that the completeness is lost and
that it may happen because the  **exploration depth** or the
**solver timeout** are too small.  
Anyhow, we are happy to propose an alternative to the ridiculously long 
frozen log.

Have a nice day :-)
