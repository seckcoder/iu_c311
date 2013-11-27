based on continuation

Unlike the book has claimed, the process registerization
is actually not easy because the introduction of global shared
variable, which makes the program hard to maintain.


This is buggy implementation of registerization. One of the most complicated
part the update of environment.

For example, for the following test case

    let x = 1
    in if zero?(let x = 0 in x)
       then x
       else -(x,2)

After you enter into if-test-cont, you should restore the
original environment, otherwise the return result of
the above program will be 0 since you are inheriting 
the environment after the evaluation if-test expression.
