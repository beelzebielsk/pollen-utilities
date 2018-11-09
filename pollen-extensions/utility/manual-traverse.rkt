#lang racket
(require "../utility.rkt" txexpr pollen/decode)

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

(define (apply-tags expr . tag-list)
  (apply-tag-funcs tag-list expr))

(define (apply-tags-to-elements elements . tag-list)
  (apply-tag-funcs-to-elements tag-list elements))

(provide (all-defined-out))
