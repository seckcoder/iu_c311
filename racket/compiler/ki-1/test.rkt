#lang racket


(match '(defn (f ([v int]) : (vec int)
                  (vec int v)))
  [`(defn (,f ((,v* ,id*) ...) : ,id ,body ...))
    (printf "~a ~a ~a ~a\n" v* id* id body)])
