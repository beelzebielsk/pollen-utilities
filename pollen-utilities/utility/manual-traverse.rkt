#lang racket
(require "../utility.rkt" txexpr pollen/decode)
(provide (all-defined-out))
(module+ test (require rackunit))

; tag-list is a list of tag functions, which are all of the form
; (cons symbol? procedure)
; where the symbol is the name of the tag, and the procedure is the
; function corresponding to that tag.
; tag-list txexpr? -> txexpr?
(define (apply-tag-funcs tag-list expr)
  (define (get-tag-names tag-list)
    (map car tag-list))
  (define (tag-to-apply? tag-name)
    (member tag-name (get-tag-names tag-list)))
  (define (get-tag-func tag-name)
    (let [(result (findf (λ (elem) (eq? tag-name (car elem))) tag-list))]
      (if result
        (cdr result)
        #f)))
  (define (apply-tag-func tag-name texpr)
    ((get-tag-func tag-name) texpr))
  (decode expr
          #:txexpr-proc
          (lambda (t)
            (if (and (txexpr? t) (tag-to-apply? (get-tag t)))
              (apply-tag-func (get-tag t) t)
              t))))

(module+ test
  (test-case
    "apply-tag-funcs"
    (define tests `(
      (root ($ "a"))
      (root ($ "a") ($ "b"))
      (root (forget-1st-element "1" "2" "3")
            (forget-2nd-element "1" "2" "3"))
      (root (to-flatten "a"
                        (to-flatten "b" "c")
                        (to-flatten "d" 
                                    (to-flatten "e"))
                        "f"
                        (to-flatten "g"
                                    ($ "h")
                                    (to-flatten "i"))))
      (root (to-flatten "a"
                        (to-flatten "b" "c")
                        (to-flatten "d" 
                                    (to-flatten "e"))
                        "f"
                        (to-flatten "g"
                                    ($ "h" (to-flatten "i"))
                                    (to-flatten "j"))))
      ))
    (define expected-results `(
      (root "$" "a" "$")
      (root "$" "a" "$" "$" "b" "$")
      (root "2" "3" "1" "3")
      (root (to-flatten "a" "b" "c" "d" "e"
                        "f" "g" "$" "h" "$" "i"))
      (root (to-flatten "a" "b" "c" "d" "e"
                        "f" "g" "$" "h" "i" "$" "j"))
      ))
    (define tag-list 
      (list
        (cons '$
              (λ (tx) `("$" ,@(get-elements tx) "$")))
        (cons 'forget-1st-element
              (λ (tx)
                 (define elems (get-elements tx))
                 (rest elems)))
        (cons 'forget-2nd-element
              (λ (tx)
                 (define elems (get-elements tx))
                 `( ,(first elems)
                    ,@(drop elems 2))))
        (cons 'to-flatten
            (λ (tx)
               (txexpr 'to-flatten
                       null
                       (decode-elements
                         (get-elements tx)
                         #:txexpr-proc
                         (decode-flattener #:only '(to-flatten))))))))
    (for ([the-case tests]
          [result expected-results])
      (check-equal? (apply-tag-funcs tag-list the-case)
                    result))))

(define (apply-tags-to-children expr . tag-list)
  (define (get-tag-names tag-list)
    (map car tag-list))
  (define (tag-to-apply? tag-name)
    (member tag-name (get-tag-names tag-list)))
  (define (get-tag-func tag-name)
    (let [(result (findf (λ (elem) (eq? tag-name (car elem))) tag-list))]
      (if result
        (cdr result)
        #f)))
  (define (apply-tag-func tag-name texpr)
    ((get-tag-func tag-name) texpr))
  (txexpr
    (get-tag expr)
    (get-attrs expr)
    ; This is the major difference between apply-tag-funcs and
    ; apply-tags-to-children. Instead of calling the tag functions on
    ; all of the descendants in an xexpr, apply them only to the
    ; children (elements) of an xexpr.
    (map
      (lambda (t)
        (if (and (txexpr? t) (tag-to-apply? (get-tag t)))
          (apply-tag-func (get-tag t) t)
          t))
      (get-elements expr))))

(define (apply-tag-funcs-to-elements tag-list elements)
  (get-elements 
    (apply-tag-funcs tag-list 
                     (txexpr (gensym "temp-tag") null elements))))

; These two functions are pretty close to my "with-syntax" form.
; They're convenience forms of the above functions. I find the syntax
; to be nicer if the tag-list is implicitly formed as a collection of
; the cons pairs which are parameters to the function.

(define (apply-tags expr . tag-list)
  (apply-tag-funcs tag-list expr))

(define (apply-tags-to-elements elements . tag-list)
  (apply-tag-funcs-to-elements tag-list elements))

