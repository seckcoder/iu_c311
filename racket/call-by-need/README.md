based on implicit reference

call by need and call by name

During the implementation of call by need, I finally could
understand the usage of datatype:expval and datatype:procedure.

call by name is not suggested when program has side-effect since it's
hard to determine the order of evaluation(which is important for program has side-effect)
for program in lazy-evaluation style.

call by need must not be used in program has side-effect. for a bad case, see
the test code in "interp.rkt"
