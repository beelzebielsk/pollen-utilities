#lang racket
(require "../utility.rkt" txexpr pollen/decode)
(provide (all-defined-out))
(module+ test (require rackunit))

(define (tag-list->hash tag-list)
  (apply hash tag-list))

; tag-list is a list like (list symbol? procedure? symbol? procedure?  ...)
; where the symbol is the name of a tag, and the procedure is the
; function corresponding to that tag.
; Each procedure? should have the same signature as txexpr-proc
; procedures from decode: (txexpr? -> (or xexpr? (listof xexpr?)))
; tag-list txexpr? -> txexpr?
(define (apply-tag-funcs expr . tag-list)
  (define tags (apply hash tag-list))
  (define (tag-to-apply? tag-name)
    (hash-has-key? tags tag-name))
  (define (apply-tag-func txpr)
    ;(apply (hash-ref tags tag-name) (get-attrs txpr) (get-elements txpr)))
    ; One of these is right. I think it is this one. I want to feed
    ; the attributes and keyword arguments and elements to the tag
    ; function.
    ;(write (format "Applying ~v to ~v" tag-name txpr))
    (apply (hash-ref tags (get-tag txpr)) (get-elements txpr)))
  (decode expr
          #:txexpr-proc
          (lambda (t)
            (if (and (txexpr? t) (tag-to-apply? (get-tag t)))
              (apply-tag-func  t)
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
        '$
        (λ elements `("$" ,@elements "$"))
        'forget-1st-element
        (λ elements (rest elements))
        'forget-2nd-element
        (λ elements
           `( ,(first elements)
              ,@(drop elements 2)))
        'to-flatten
        (λ elements
           (txexpr 'to-flatten
                   null
                   (decode-elements
                     elements
                     #:txexpr-proc
                     (decode-flattener #:only '(to-flatten)))))))
    (for ([the-case tests]
          [result expected-results])
      (check-equal? (apply apply-tag-funcs the-case tag-list)
                    result))))

(define (apply-tags-to-children expr . tag-list)
  (define tags (tag-list->hash tag-list))
  (define (tag-to-apply? tag-name)
    (hash-has-key? tags tag-name))
  (define (apply-tag-func txpr)
    (apply (hash-ref tags (get-tag txpr)) (get-elements txpr)))
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
          (apply-tag-func t)
          t))
      (get-elements expr))))

(define (apply-tags-to-elements elements . tag-list)
  (get-elements 
    (apply apply-tag-funcs (txexpr (gensym "temp-tag") null elements) tag-list)))

