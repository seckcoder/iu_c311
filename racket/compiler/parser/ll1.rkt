#lang racket

(require "parsing-table.rkt")

; omit the scanner part
(define (make-parser grammar)
  (let ((parsing-tbl (build-parsing-table grammar)))
    (define (parse exp)
      'unfinished)
    parse))
