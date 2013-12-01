based on continuation

This is the implementation of threads on eopl. The implementation is
different from mine in several points:
    1. use procedural representation for abstraction of Thread. This makes
       the code more concise and neat.(typical method to create snapshot)
    2. Timing in apply-cont instead of interp-exp. Combining this with
       the procedural representation, it makes the code elegant.
       continuation passed to apply-cont already contains much meaningful
       information. Therefore, timing in apply-cont is a wise choice.
