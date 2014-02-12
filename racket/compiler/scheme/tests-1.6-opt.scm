(add-tests-with-string-output "let*"
  [(let* ([x 5]
          [y (fx+ x 2)])
     (- y x)) => "2\n"])
