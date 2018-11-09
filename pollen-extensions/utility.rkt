#lang racket
(require txexpr pollen/decode)

; Pollen Helpers: {{{ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (report EXPR)
  (let 
    [(result EXPR)
     (expr-length 25)
     (expr-string (~v (syntax->datum #'EXPR)))]
    (displayln
      (string-append
        ; If the syntax object is from some other module...
        (if (syntax-source-module #'EXPR)
          (format "[From ~a]" 
                  ; Report only the filename of the module.
                  (last
                    (explode-path
                      (resolved-module-path-name
                        (module-path-index-resolve 
                          (syntax-source-module #'EXPR))))))
          "")
        (format "(line ~a, col ~a): Expression ~a results in ~v"
                (syntax-line #'EXPR)
                (syntax-column #'EXPR)
                (if (> (string-length expr-string) expr-length)
                  (string-append (substring expr-string 0 expr-length) "...")
                  expr-string)
                result)))
    result))

; Makes it so that a list of elements appears in the pollen document
; as if they were not enclosed in a list. Like when/splice, but always
; happens. The map here is for just in case something is produced that
; is not a txexpr-element. Any number that's not a symbol (I think) is
; not a txexpr-element, so that's nonpositive numbers, floating-point,
; exact rational numbers which do not reduce to an integer.
(define (list-splice . args)
  (if (= 1 (length args))
    (let ([lst (first args)])
      (cons '@ 
            (map (lambda (e) (if (txexpr-element? e) e (~a e)))
                 lst)))
    (list-splice args)))

(define-syntax let-splice
  (lambda (stx)
    (syntax-case stx ()
      [(_ ((id val) ...) body)
       #'(let ((id val) ...) body)]
      [(_ ((id val) ...) body ...)
       #'(let ((id val) ...) 
           (list-splice body ...))])))

(define (@-flatten txpr) 
  (decode txpr
          #:txexpr-proc (decode-flattener #:only '(@))))

; TODO: Under construction macro, which will take a list of tag names
; and create tag functions that output the empty string.

; }}} ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; lambda? -> lambda?
; This takes a procedure and alters it so that it is safe to use as
; the #:txexpr-proc argument to the decode-elements function. It's a
; temporary hack. The problem is that decode-elements functions like
; decode, but it places the txexpr-elements? inside a temporary tag.
; And this temporary tag is a vaid target of the #:txexpr-proc. This
; can be very confusing.
(define (make-d-el-proc proc)
  (lambda (tx) 
    (if (string-prefix? (symbol->string (get-tag tx)) "temp-tag") 
      tx
      (proc tx))))
; list? list? -> lambda
; Takes in one of two optional lists of tags: 
; #:only, which specifies which tags to flatten and nothing else.
; #:exclude, which specifies flattens all tags but those listed.
; Returns a function that, when given as #:txexpr-proc in
; pollen/decode, will flatten those tags and leave all others alone.
(define (decode-flattener #:only [only-these (void)]
                          #:exclude [except-these (void)])
  (cond [(and (void? only-these) (void? except-these))
         get-elements]
        [(void? except-these)
         (λ (tx) 
            (if (ormap (λ (name) (is-tag? tx name)) only-these)
              (get-elements tx)
              tx))]
        [else 
          (λ (tx)
             (if (ormap (λ (name) (is-tag? tx name)) except-these)
               tx
               (get-elements tx)))]))

(define printable? (or/c string? number? symbol?))
(define (is-tag? tag name)
  (and (txexpr? tag) (eq? (get-tag tag) name)))
; list? procedure? #:keep-where procedure? -> list?
; Somewhat similar to the split procedure for strings. Takes a list
; and returns a list of the same elements of lst, in the same order,
; but placed in sublists. Each sublist ends where an element occurs
; that causes (split-pred? element current-split tail) to be true. The
; next sublist picks up from there. If a split should be empty (such
; as when there are two consecutive elements that cause split-pred? to
; be true), then those splits are not kept.
; The split-map option is supplied because the output of split-where
; may not be a list of splits if the #:keep-where function returns
; 'separate. In this case, the split element is placed on it's own in
; the list of splits. Mapping over the splits (and only the splits) is
; a common enough use-case, I think, that the optional parameter is
; warranted.
; The reason why the objects at which we split are not placed in the
; list as splits is that this function takes after split-string, and
; functions like it from other languages. The thing upon which we
; split is normally removed. Not considered. There are use cases where
; you wouldn't want to throw away that which you split upon, but you'd
; want to run a function over everything else.
;
; TODO:
; Consider splitting this into two functions:
; - a splitter-function, which handled the iteration over the
;   elements, and expects a function which determines the next state
;   of everything. Like, the output of this function will be the next
;   current split, the next list of splits, and the new value of the
;   remaining elements to iterate upon.
; - A function which takes in the arguments like split-pred?
;   #:keep-where #:split-map, and uses those to create the function
;   that splitter-function uses. This will make the resulting
;   functionality of splitting easier to think about, especially as I
;   extend the capabilities of the function, stating allowing the user
;   to do something like directly supply the function
;   splitter-function wants, allowing them the greatest control over
;   the splitting method.
;(define (splitter-func lst loop-body finally)
  ;(define (iter current-split splits remaining)
    ;(cond
      ;[(null? remaining) (finally current-split splits)]
      ;[else (call-with-values 
              ;iter 
              ;(loop-body current-split splits remaining))]))
  ;(iter null null lst))
;
;(define (split-where lst [split-pred? #f]
                     ;#:keep-where  [keep-pred? #f]
                     ;#:split-map  [split-cun #f]
                     ;#:action [loop-body #f]
                     ;#:finally [finally #f])
  ;(cond 
    ;[(and loop-body finally) (splitter-func lst loop-body finally)]
    ;[(not split-pred?) 
     ;(error "Must provide either both #:action and #:finally or split-pred?")]
    ;[else 
      ;(let
        ;([loop-body
           ;(λ (current-split splits remaining)
              ;(match-let
                ;[((cons elem tail) remaining)]
                ;(if (split-pred? elem current-split tail)
                  ;(let* 
                    ;[(decision (keep-pred? elem current-split
                                           ;tail))
                     ;(new-current-split
                       ;(case decision
                         ;[(next) (list elem)]
                         ;[else null]))
                     ;(final-current-split-contents
                       ;(reverse
                         ;(case decision
                           ;[(current) (cons elem current-split)]
                           ;[else current-split])))
                     ;(processed-current-split
                       ;(cond
                         ;[(null? final-current-split-contents)
                          ;final-current-split-contents]
                         ;[split-func
                           ;(split-func final-current-split-contents)]
                         ;[else final-current-split-contents]))
                     ;(new-splits
                       ;(begin 
                         ;(report decision)
                         ;(if (eq? decision 'separate)
                           ;(report (list elem processed-current-split))
                           ;(void))
                         ;(report 
                           ;(case decision
                             ;[(separate #t)
                              ;(if (null? processed-current-split)
                                ;(cons elem splits)
                                ;(append (list elem processed-current-split)
                                        ;splits))]
                             ;[else
                               ;(if (null? processed-current-split)
                                 ;splits
                                 ;(cons processed-current-split splits))]))))]
                    ;(values new-current-split
                            ;new-splits
                            ;tail))
                  ;(values (cons elem current-split)
                          ;splits
                          ;tail))))]
         ;[finally
           ;(λ (current-split splits)
              ;(cond
                ;[(null? current-split) splits]
                ;[split-func (cons (split-func (reverse current-split))
                                  ;splits)]
                ;[else (cons (reverse current-split) splits)])

(define (split-where lst split-pred? 
          #:keep-where [keep-pred? (λ _ #f)]
          #:split-map [split-func #f]
          #:action [loop-body #f])
  (define (iter current-split splits remaining)
    (cond 
      [(null? remaining) 
       (cond
         [(null? current-split) splits]
         [split-func (cons (split-func (reverse current-split)) splits)]
         [else (cons (reverse current-split) splits)])]
      [else
        (match-let
          [((cons elem tail) remaining)]
          (if (split-pred? elem current-split tail)
            (let* 
              [(decision (keep-pred? elem current-split
                                     tail))
               (new-current-split
                 (case decision
                   [(next) (list elem)]
                   [else null]))
               (final-current-split-contents
                 (reverse
                   (case decision
                     [(current) (cons elem current-split)]
                     [else current-split])))
               (processed-current-split
                 (cond
                   [(null? final-current-split-contents)
                    final-current-split-contents]
                   [split-func
                     (split-func final-current-split-contents)]
                   [else final-current-split-contents]))
               (new-splits
                 (begin 
                   (if (eq? decision 'separate)
                     (list elem processed-current-split)
                     (void))
                   (case decision
                     [(separate #t)
                      (if (null? processed-current-split)
                        (cons elem splits)
                        (append (list elem processed-current-split)
                                splits))]
                     [else
                       (if (null? processed-current-split)
                         splits
                         (cons processed-current-split splits))])))]
              (iter new-current-split
                    new-splits
                    tail))
            (iter (cons elem current-split)
                  splits
                  tail)))]))
  (reverse (iter null null lst)))

(define (reverse* val [leaf? (λ (v) (not (list? v)))])
  (define (helper val)
    (if (leaf? val)
      val
      (reverse (map helper val))))
  (helper val))

(provide (all-defined-out))
