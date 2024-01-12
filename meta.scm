#! /usr/bin/guile -s
!#

(use-modules
    (srfi srfi-1)
    (ice-9 ftw))

(define-syntax extract-options
    (syntax-rules ()
        ((extract-options exp option ...)
         (begin
            (define name exp)
            (%extract-options name option ...)))))

(define-syntax %extract-options
    (syntax-rules (allow-none)
        ((%extract-options exp (allow-none option))
         (define option
            (let ((entry (assoc `option exp)))
                (if entry (cdr entry) '()))))

        ((%extract-options exp (option default))
         (define option
            (let ((entry (assoc `option exp)))
                (if entry (cdr entry) `(default)))))

        ((%extract-options exp option)
         (define option
            (let ((entry (assoc `option exp)))
                (if entry
                    (cdr entry)
                    (error "extract-options: option not found!" `option)))))

        ((%extract-options exp option options ...)
         (begin
            (%extract-options exp option)
            (%extract-options exp options ...)))))

(extract-options (call-with-input-file "config.scm" read)
    (compiler gcc)
    (source-dirs ./src)
    (allow-none source-ignore)
    (output-file a.out)
    (allow-none features)
    (allow-none libraries)
    (allow-none include-paths))

(define (build-cmd)
    `(,@compiler
        ,@(gather-source-files source-dirs source-ignore)
        -o ,@output-file
        ,@features
        ,@(map (symbol-prefixer '-l) libraries)
        ,@(map (symbol-prefixer '- 'I) include-paths)))

(define (runner)
    (define args (cdr (command-line)))

    (unless (= 1 (length args)) (help 1))
        
    (case (string->symbol (car args))
        ((help) (help 0))
        ((run) (run))
        ((build) (build))
        ((build-and-run) (begin (build) (run)))
        ((clean) (clean))
        (else (help 1))))

(define (debug arg)
    (display arg)
    (newline)
    arg)

(define (symbol-append . args)
    (string->symbol (apply string-append (map symbol->string args))))

(define (symbol-prefixer . pfx)
    (lambda (sym) (apply symbol-append `(,@pfx ,sym))))

(define (shell-run . arg-lists)
    (define (fix-arg a) (if (symbol? a) (symbol->string a) a))
    (define fixed-lists (map (lambda (args) (map fix-arg args)) arg-lists))
    (for-each (lambda (args) (apply system* (debug args))) fixed-lists))

(define (gather-source-files dirs ignore)
    (define (is-dir? p)
        (call-with-input-file p
            (lambda (port)
                (eq? (stat:type (stat p)) 'directory))))
    (define (ignored? p)
        (member p `("." ".." ,@(map symbol->string ignore))))
    (define (traverse acc path)
        (fold (lambda (file acc)
                (let ((full-path (string-append path "/" file)))
                    (cond ((ignored? file) acc)
                          ((is-dir? full-path) (traverse acc full-path))
                          ((string-suffix? ".c" file) (cons full-path acc))
                          (else acc))))
              acc
              (scandir path)))
    (concatenate (map (lambda (dir) (traverse '() dir)) (map symbol->string dirs))))

(define (run)
    (shell-run '(./build/main)))

(define (build)
    (clean)
    (shell-run '(mkdir build) (build-cmd)))

(define (clean)
    (shell-run '(rm -rf build)))

(define (help exit-code)
    (display "Usage: ./meta.scm [help | run | build | build-and-run | clean]\n")
    (exit exit-code))

(runner)