; A good demo to understand mk

(run* (x)
  (all
    (conde
      ((== x #f))
      ((== x #t)))
    (== x 'c)))


; let's analyze the execution process

; all: generate a goal `a-g` from sequence of goals g....
;      `a-g` will only succeed when g... succeed.
; conde -> (anye (== x #f)
;                (anye (== x #t)))
; anye(g1,g2): (mplus (g1 s) (lambda () (g2 s)))
; mplus(a f): if a fail, then tranfer to f,
;             if a = subst, then return (cons subst f)
;             if a = (subst ...) return (cons subst ... f)
;
;
; (all g1 g2) : return a goal: (bind (g1 s) g2)
; bind(a g): if a fail, then all fail
;            if a = subst, ret (g a)
;            if a = (a f), mplus((g a) (bind (f) g))
;            -> Fuck Bind...
;
; (all g1 g2) ->
; (bind (g1 s) g2); g1 : conde; g2: (== x 'c)
; (g1 s) = (anye g2 g3); g2 : (== x #f); g3 : (== x #t)
; (anye g2 g3) -> (lambda (s)
;                   (mplus
