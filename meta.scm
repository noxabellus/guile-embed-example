#! /usr/bin/csi -script

(import (scheme base)
        (scheme process-context)
        (prefix (shell) shell:))

(define args (cdr (command-line)))

(define build-cmd '(
    gcc main.c
        -o ./build/main
        -fPIC -pthread
        -lguile-3.0 -lgc -lpthread -ldl -lm
        -I/usr/include/guile/3.0))

(define (run) (shell:run (./build/main)))
(define (build)
    (clean)
    (shell:run (mkdir build) ,build-cmd))
(define (clean) (shell:run (rm -rf build)))
(define (help exit-code)
    (display "Usage: ./meta.scm [help | run | build | build-and-run | clean]\n")
    (exit exit-code))

(unless (= 1 (length args)) (help 1))
    
(case (string->symbol (car args))
    ((help) (help 0))
    ((run) (run))
    ((build) (build))
    ((build-and-run) (begin (build) (run)))
    ((clean) (clean))
    (else (help 1)))