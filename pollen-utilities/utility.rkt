#lang racket
(require txexpr pollen/decode)
(module+ test (require rackunit))
(provide (all-defined-out))

; Pollen Helpers: {{{ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (list-splice (list* args))))

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

; }}} ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (is-tag? tag . names)
  (and (txexpr? tag) (member (get-tag tag) names)))
(define (has-attr? tag attr)
  (attrs-have-key? (get-attrs tag) attr))
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
; #:only, which specifies which tags to flatten and nothing else (a
; whitelist for flattening).
; #:exclude, which will flattens all tags except for those listed (a
; blacklist for flattening).
; Returns a function that, when given as #:txexpr-proc in
; pollen/decode, will flatten those tags and leave all others alone.
;
; #:only and #:exclude should not be given together, as the effect of
; only one of the two will work.
;
; Here, flattening means that the contents of the tag will be placed
; in the parent tag. The flattening here is recursive, so for a given
; tag which is to be flattened, first all of it's descendants will
; have their contents flattened (if specified to be), then those
; results will be placed in the parent tag of that tag.
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

(define (flatten-tags elements 
                      #:only [only-these (void)]
                      #:exclude [except-these (void)])
  (if (txexpr? elements)
    (decode elements
            #:txexpr-proc 
            (decode-flattener #:only only-these
                              #:exclude except-these))
    (decode-elements elements
                     #:txexpr-proc 
                     (decode-flattener #:only only-these
                                       #:exclude except-these))))


(define printable? (or/c string? number? symbol?))

; list? procedure? 
;   #:keep-where procedure? 
;   #:split-map procedure?
;   #:keep-empty-splits? boolean?
;   #:action (or/c procedure? #f)
; -> list?
; Somewhat similar to the split procedure for strings. Takes a list
; and returns a list of the same elements of lst, in the same order,
; but placed in sublists. Each sublist ends where an element occurs
; that causes (split-pred? element current-split tail) to be true. The
; next sublist picks up from there. 
; - The #:keep-where procedure determines where an element that causes
;   a split will go. If the procedure returns:
;       - 'current: The element which caused the split will be placed
;         at the end of the sublist that was being built when the
;         element was encountered.
;       - 'next: The element which caused the split will be placed
;         at the start of the next sublist that will be built up.
;       - 'separate: The element will be placed on its own. It won't
;         be in a sublist, the element itself will be placed between
;         the current sublist being built, and the next sublist that
;         will be built.
;       - 'ignore: The element will not be placed in any split, nor on
;         its own. It just gets ignored.
; - The split-map option is supplied because the output of split-where
;   may not be a list of splits if the #:keep-where function returns
;   'separate. In this case, the split element is placed on it's own
;   in the list of splits. Mapping over the splits (and only the
;   splits) is a common enough use-case, I think, that the optional
;   parameter is
; warranted.
; - If a split should be empty (such as when there are two consecutive
;   elements that cause split-pred? to be true), then those splits are
;   not kept if #:keep-empty-splits? is false. Otherwise, the splits
;   are kept.
; - #:action is a procedure that should take the following paramters:
;   current-split, splits, remaining and return (values
;   next-current-split, next-splits, next-remaining). This allows
;   total control over how to produce splits from the function, if
;   that's desired.
;
; The reason why the objects at which we split are not placed in the
; list as splits is that this function takes after split-string, and
; functions like it from other languages. The thing upon which we
; split is normally removed. Not considered. There are use cases where
; you wouldn't want to throw away that which you split upon, but you'd
; want to run a function over everything else.
(define (split-where 
          lst split-pred?
          #:keep-where [keep-pred? (λ _ 'ignore)]
          #:split-map [split-func identity]
          #:action [loop-body #f])
  (define (update-splits-list current-split element decision splits)
    (define finished-splits 
      (make-finished-splits current-split element decision))
    (append finished-splits splits))
  (define (make-finished-splits current-split element decision)
    (define current-split-before-map
      (if (eq? decision 'current)
        (cons element current-split)
        current-split))
    (define finished-current-split
      (if (null? current-split-before-map)
        current-split-before-map
        (split-func (reverse current-split-before-map))))
    (define splits null)
    (unless (null? finished-current-split)
      (set! splits (cons finished-current-split splits)))
    ; split-func is not applied when a split is kept using 'separate.
    ; Nor is it wrapped in a list like other splits.
    (when (eq? decision 'separate)
      (set! splits (cons element splits)))
    splits)
  (define (make-new-split element decision)
    (case decision
      [(next) (list element)]
      [else null]))

  (define-values (last-split splits _)
    (if loop-body
      (for/fold
        ([current-split null] [splits null] [remaining lst])
        ([element lst])
        (loop-body current-split splits remaining))
      (for/fold 
        ([current-split null] [splits null] [remaining lst])
        ([element lst])
        (define tail (cdr remaining))
        (define split? (split-pred? element current-split tail))
        (define decision (keep-pred? element current-split tail))
        (if (not split?)
          (values (cons element current-split)
                  splits
                  tail)
          (values (make-new-split element decision)
                  (update-splits-list
                    current-split element decision splits)
                  tail)))))
  (reverse
    (if (null? last-split)
      splits
      (cons (split-func (reverse last-split)) splits))))

; any/c lambda? -> any/c
; If a value is a list, then it will reverse the list and all lists
; contained within. If the value is not a list, or it satisfies the
; leaf? predicate, then the value will be untouched.
(define (reverse* val [leaf? (λ (v) (not (list? v)))])
  (define (helper val)
    (if (leaf? val)
      val
      (reverse (map helper val))))
  (helper val))

; list? lambda? -> list?
; Takes a list and a predicate. Removes all contiguous series of
; elements at the front and back of the list which satisfy the
; predicate.
(define (list-strip lst pred?)
  (dropf-right (dropf lst pred?) pred?))

(define (attr-list-ref lst key [failure-result #f])
  (let ([pair (findf (λ (p) (eq? (first p) key)) lst)])
    (if pair (second pair) failure-result)))

(module+ test
  (define (zip l1 l2)
    (cond ((or (null? l1) (null? l2)) null)
          (else (cons (list (car l1) (car l2))
                      (zip (cdr l1) (cdr l2))))))
  (test-case 
    "reverse*"
    (define tests
      `((a b c)
        (a b c d)
        (0 1 2 3 4)
        (a b 0 1)
        (a (b c) d e (f g))
        (a (b c d) 0 1 2 (3 4 5 e f (g h)))))
    (define expected-results
      `((c b a)
        (d c b a)
        (4 3 2 1 0)
        (1 0 b a)
        ((g f) e d (c b) a)
        (((h g) f e 5 4 3) 2 1 0 (d c b) a)))
    (for ([test-case tests]
          [result expected-results])
      (check-equal? (reverse* test-case) result "reverse* failed")))
  (test-case
    "decode-flattener"
    (define xexp1
        `(root 
           (a "content") 
           (b "content") 
           (c "content")
           (d "content")))
    (match-let ([(list 'root a b c d) xexp1])
      (check-equal? 
        `(root ,@(get-elements a) ,b ,c ,d)
        (decode xexp1 #:txexpr-proc (decode-flattener #:only '(a))))
      (check-equal? 
        `(root ,a ,@(get-elements b) ,c ,d)
        (decode xexp1 #:txexpr-proc (decode-flattener #:only '(b))))
      (check-equal? 
        `(root ,a ,b ,@(get-elements c) ,d)
        (decode xexp1 #:txexpr-proc (decode-flattener #:only '(c))))
      (check-equal? 
        `(root ,a ,b ,c ,@(get-elements d))
        (decode xexp1 #:txexpr-proc (decode-flattener #:only '(d))))
      (check-equal? 
        `(root ,@(get-elements a) ,@(get-elements b) ,c ,d)
        (decode xexp1 #:txexpr-proc (decode-flattener #:only '(a b))))
      (check-equal? 
        `(root ,a ,@(get-elements b) ,@(get-elements c) ,d)
        (decode xexp1 #:txexpr-proc (decode-flattener #:only '(b c))))
      (check-equal? 
        `(root ,a ,b ,@(get-elements c) ,@(get-elements d))
        (decode xexp1 #:txexpr-proc (decode-flattener #:only '(c d))))
      (check-equal? 
        `(root ,@(get-elements a) ,b ,c ,@(get-elements d))
        (decode xexp1 #:txexpr-proc (decode-flattener #:only '(a d))))))
    (define (split a-case) 
      (split-where a-case (λ (e . _) (eq? 's e))))
  (test-case
    "split-where 1"
    (define the-case '(0 1 2 s 3 4 5 s s 6 7 8 9 10 s))
    (define expected-result '((0 1 2) (3 4 5) (6 7 8 9 10)))
    (check-equal? (split the-case) expected-result))
  (test-case
    "split-where 2"
    (define the-case '(10 8 9 7 6 5 4 3 2 1))
    (define expected-result (list the-case))
    (check-equal? (split the-case) expected-result))
  (test-case
    "split-where 3"
    (define the-case '(10 8 9 s 7 6 s 5 s s 4 s 3 2 1 s))
    (define expected-result '((10 8 9) (7 6) (5) (4) (3 2 1)))
    (check-equal? (split the-case) expected-result))
  (test-case
    "split-where 4"
    (define the-case '(s 10 8 9 s 7 6 s 5 s s 4 s 3 2 1 s))
    (define expected-result '((10 8 9) (7 6) (5) (4) (3 2 1)))
    (check-equal? (split the-case) expected-result))
  (test-case
    "split-where 5"
    (define the-case '(s))
    (define expected-result null)
    (check-equal? (split the-case) expected-result)))


; TODO:
; - make let-splice convert numbers to strings in the final product.
