#lang racket
(require 
  "utility.rkt" "utility/manual-traverse.rkt"
  txexpr pollen/decode pollen/core pollen/tag)
(provide macro macro-1 environment group define-math-tag ->ltx $ $$
         ensure-math combine-math-things math-thing?)
(provide split-bullets store-args)
(module+ test (require rackunit))

; any/c -> any/c
; Returns a truthy value is the argument is considered to be
; whitespace.
(define (whitespace? val)
  (and (string? val) (regexp-match #px"^\\s*$" val)))
; any/c -> any/c
; Returns a truthy value is the argument is considered to be
; a newline.
(define (newline? val)
  (and (whitespace? val) (string-contains? val "\n")))
; (or/c txexpr? any?) -> (or/c txexpr? any?)
; If it gets a txexpr, then checks for math tags and performs the
; flattening, making sure that no math tag is nested within another
; math tag, by bringing the children of a nested math tag out of the
; math tag (and getting rid of it). However, it does not get rid of
; the containing 'math tag.
(define (math-flatten xexpr)
  (cond [(is-tag? xexpr 'math)
         (flatten-tags xexpr #:only '(ensure-math))]
        ; We know that this is not contained within a math tag, for if
        ; it were, the above branch would have been taken already, and
        ; decode would've been applied to this tag.
        [(is-tag? xexpr 'ensure-math)
         (math-flatten (txexpr 'math null (get-elements xexpr)))]
        [(txexpr? xexpr)
         ; It's not necessary to apply math-flatten recursively here,
         ; as other parts of math-flatten will hanlde that already.
         (map math-flatten xexpr)]
        [else xexpr]))

(module+ test
  (test-case
    "math-flatten"
    (define cases `(
        (root (math "a"))
        (root (math "a" "bc" (ensure-math "d")))
        (root (math "a" "bc" 
                    (ensure-math "d" "e"
                                 (ensure-math "f"))))
        (root (math "a" "bc" 
                    (ensure-math "d" "e")
                    "f"
                    (other-element 
                      (ensure-math "g"))))
        ))
    (define expected-results `(
        (root (math "a"))
        (root (math "a" "bc" "d"))
        (root (math "a" "bc" "d" "e" "f"))
        (root (math "a" "bc" "d" "e" "f"
                    (other-element "g")))

        ))
    (for ([the-case cases]
          [result expected-results])
      (check-equal? (math-flatten the-case) result))))

(define (group . _)
  (list-splice `("{" ,@_ "}")))
(define math (default-tag-function 'math))
(define ensure-math (default-tag-function 'ensure-math))
; NOTE: To make use of the $ and $$ tags, you must provide them
; explicitly in your pollen.rkt. They are convenience aliases for the
; math tag, but core->ltx expects 'math tags explicitly, not '$ or
; '$$.
(define-tag-function ($ _ text)
  (apply math text))
(define-tag-function ($$ _ text)
  (apply math #:display "" text))

(module+ test
  (test-case
    "math-tags"
    (define cases `(
        (root ($ "a"))
        (root ($$ "a"))
        (root ($ "a") ($ "b"))
        (root ($ "a") ($$ "b"))
        (root ($ "a") ($$ "b") (math "c"))
        ))
    (define expected-results `(
        (root (math "a"))
        (root (math ((display "")) "a"))
        (root (math "a") (math "b"))
        (root (math "a") (math ((display "")) "b"))
        (root (math "a") (math ((display "")) "b") (math "c"))
        ))
    (for ([the-case cases]
          [result expected-results])
      (check-equal? 
        (apply-tags 
          the-case
          (cons '$ (λ (tx) (apply $ (get-attrs tx) (get-elements tx))))
          (cons '$$ (λ (tx) (apply $$ (get-attrs tx) (get-elements tx)))))
        result))))

(define-syntax-rule (define-math-tag (name attrs elems) body ...)
  (define-tag-function 
    (name attrs elems) 
    (let [(result ((lambda (attrs elems) body ...) attrs elems))]
      (ensure-math result))))

; - Core tags
;   - macro : For latex macros.
;   - environment : For latex environments.
;   - math : Represents stuff that thould go in between dollar-signs.
;   - symbol : Represents stuff that's actually one output character.
;     macros that translate to a single output character. Will also
;     have other connotations of being things meant for math mode.
; - Convenience tags
;   - ol (ordered list)
;       - Initially, it will have a mixture of 'i tags and lines that
;         start with some bullet.
;       - After processing, it's children will all be 'i tags and
;         newline?.
;   - l (unordered list)
;       - Same as for ol.
;   - eql (equation list)
;       - Similar to ol, but after processing, children will all be 'i
;         tags.
;   - list
;       - This is a general list tag. It's only children are 'list and
;         'i. This is the final form of the 'l, 'ol, and 'eql tags,
;         before they're transformed into strings.
;   - table
;       - The only children that table can have are rows.
;       - Rows can have plain text or 'cell tags as children. 
;       - After processing, all lines of plain text will become a
;         'cell tag.

; TODO: Modify to allow for different group start and group end
; characters. The default is "{" for group start and "}" for group
; end.
(define (macro-args args)
  (add-between args '("}{")
               #:splice? #t
               #:before-first '("{")
               #:after-last '("}")))
; (listof txexpr-elements?) -> (listof txexpr?)
; Takes a list of txexpr-elements? and encases each of those lists in
; 'arg tags.
(define (store-args args)
  (map (λ (a) (txexpr 'arg null a)) args))

(define (core? val)
  (or (string? val)
      (apply is-tag? val
             '(macro math environment symbol))))
; (or/c string? symbol?) (listof core?) ...
; This tag represents a latex macro. The first argument is the name of
; the macro.
; The remaining arguments are the arguments to the macro, where each
; argument is a list of (or/c string? core-tag?).
(define (macro name . args)
  (txexpr 'macro 
          `((start "{") (end "}") (name ,(~a name)))
          (store-args args)))
; A convenience functions for latex macros which use only a single
; argument. With this, you no longer have to create a list around the
; elements of the 1st argument to the macro.
(define (macro-1 name . args)
  (macro name args))

; core? ... -> txexpr?
; Takes a series of core? and encases them within 'symbol tag.
; Signifies that the contents should all be considered a single
; symbol (character). Example use: (tex-symbol (macro-1 'pi)). That
; macro will become the greek lowercase letter pi.
(define (tex-symbol #:type [type #f] . args )
  (txexpr 'symbol
          (if type `((type ,type)) null)
          args))

; list? lambda? -> (or/c bool? nonnegative?)
; If an element satisfying the predicate exists within lst, then
; index-where-last returns the position of the last such element.
; Otherwise, index-where-last returns false.
(define (index-where-last lst pred)
  (call/cc 
    (λ (stop)
       (let loop ([remaining lst] [pos 0] [last-seen #f])
         (cond
           ((null? remaining) (stop last-seen))
           ((pred (first remaining))
            (loop (rest remaining) (add1 pos) pos))
           (else (loop (rest remaining) (add1 pos) last-seen)))))))

; any/c -> bool?
; True if the argument is considered a math thing, a tag whose
; contents ought to be rendered in latex's math mode.
(define (math-thing? value)
  (cond
    ((txexpr? value)
     (or (is-tag? value 'math 'ensure-math)
         (and (is-tag? value 'symbol)
              (eq? (attr-ref value 'type) "math"))))
    (else #f)))

(define (combination math-stuff)
  (let ((start (first math-stuff))
        (end (last math-stuff)))
    (cond
      ((and (is-tag? start 'math)
            (is-tag? end 'math))
       math-stuff)
      ((is-tag? start 'math)
       (list 
         (apply math (append (get-elements start) (rest math-stuff)))))
      ((is-tag? end 'math)
       (list
         (apply math (append (drop-right math-stuff 1) 
                             (get-elements end)))))
      (else (list (apply math math-stuff))))))

; txexpr-elements? -> txexpr-elements?
; Takes txexpr-elements? and looks for math-thing? elements to
; combine. The returned result will contain all of the same elements
; in the same order, except that math-thing? elements will be
; combined.
; - any two math-thing? separated by zero or more elements of
;   whitespace? will be combined as children of a 'math tag.
; - if one of the two math-things? is a 'math tag, then the other
;   math-thing? will be added into the 'math tag as a child. If the
;   other 'math thing comes before the 'math tag, then it will be
;   prepended to the children of the 'math tag; otherwise it will get
;   appended to the children of the 'math tag.
; - two 'math tags are not combined.
(define (combine-math-things elements)
  ; Here's a weird thought: let's make a function that acts like a
  ; state machine by making the states functions. The state-functions
  ; will return the next state to transition to.
  ;
  ; a state without a matching state function is a state with no
  ; transitions, ie a terminal state.
  (define states
    (list
      (cons 'searching
            (lambda (token)
              (if (math-thing? token) 'found-math-thing 'searching)))
      (cons 'found-math-thing
            (lambda (token)
              (cond
                ((whitespace? token) 'found-math-thing)
                ((math-thing? token) 'found-combine-candidate)
                (else 'searching))))))
  ; scanned contains the element which caused machine to enter an
  ; #:until state or leave an #:until-not state.
  ; the ith entry of history is the state the machine was in when it
  ; examined token i. Thus, the initial state is (list-ref 0 history),
  ; the state of the machine prior to scanning the 1st token. Thus
  ; there is one more state in the history than there are scanned
  ; tokens.
  (define (run-machine #:until [until-states #f]
                       #:until-not [until-not-states #f]
                       states
                       tokens
                       current-state)
    (define (get-state-func name)
      (cdr (assoc name states)))
    (define (helper scanned remaining history current-state)
      (cond 
        ((or (null? remaining)
             (and until-states
                  (member current-state until-states))
             (and until-not-states
                  (member current-state until-not-states)))
         (values scanned remaining history current-state))
        (else
          (let ((next-state 
                  ((get-state-func current-state) (first remaining))))
          (helper (append scanned (list (first remaining)))
                  (rest remaining)
                  (cons next-state history)
                  next-state)))))
    (let-values
      (((scanned remaining history last-state)
        (helper null tokens (list current-state) current-state)))
      (values scanned remaining (reverse history) last-state)))
  ; How to combine two math-things: math-stuff is a list of
  ; txexpr-elements? such that the first and last element are
  ; math-thing? and if there are other members in between them, they
  ; are whitespace.
  (define (combine elements)
    (cond 
      ((null? elements) elements)
      (else
        (define-values
          (scanned-elements the-rest history state-after-searching)
          (run-machine #:until (list 'found-combine-candidate)
                       states
                       elements
                       'searching))
        (cond
          ((eq? state-after-searching 'found-combine-candidate)
           (define-values
             (prefix math-stuff)
             (let ((last-non-math 
                     (index-where-last history (curry eq? 'searching))))
               (split-at scanned-elements last-non-math)))
           ; The last math element should be up for combination with
           ; the next math element, if there is one. This way
           ; something like
           ;    (math a) (math b) (math c) (math d)
           ; becomes
           ;    (math a b c d)
           ; instead of
           ;    (math a b) (math c d)
           (let-values
             (((all-but-last the-last)
               (split-at-right (combination math-stuff) 1)))
             (append prefix
                     all-but-last
                     (combine (cons (first the-last) the-rest)))))
           (else elements)))))
  (combine elements))

(module+ test
  (test-case
    "combine-math-things"
    (define tests
      `(()
        (a)
        (a b)
        ((math a))
        ((math a) (math b))
        ((math a) (ensure-math b))
        ((ensure-math a) (ensure-math b))
        ((math a) (ensure-math b) (ensure-math c))
        ((math a) (ensure-math b) ,(tex-symbol #:type "math" "symbol"))
        ((math a) " " (ensure-math b))
        ((math a) " " (ensure-math b) " " " " (ensure-math c))
        ((math a) " " (math b) (ensure-math c) (math d) e (ensure-math f) 
                  ,(tex-symbol #:type "math" "g"))
        ))
    (define expected-results
      `(()
        (a)
        (a b)
        ((math a))
        ((math a) (math b))
        ((math a (ensure-math b)))
        ((math (ensure-math a) (ensure-math b)))
        ((math a (ensure-math b) (ensure-math c)))
        ((math a (ensure-math b) ,(tex-symbol #:type "math" "symbol")))
        ((math a " " (ensure-math b)))
        ((math a " " (ensure-math b) " " " " (ensure-math c)))
        ((math a) " " (math b (ensure-math c)) (math d) e 
                  (math (ensure-math f) 
                        ,(tex-symbol #:type "math" "g")))
        ))
    (for ([test-case tests]
          [result expected-results])
      (check-equal? (combine-math-things test-case) result 
                    "combine-math-things failed"))))

(module+ test
  (test-case
    "combine-math-things"
    (define tests
      `((root a (b (math c) (math d)))
        (root a (b (ensure-math c) (ensure-math d)))
        (root (a (ensure-math b) (symbol ((type "math")) c)
                 (ensure-math d))
              (f (symbol ((type "math")) g) (symbol ((type "math")) h)))
        ))
    (define expected-results
      `((root a (b (math c) (math d)))
        (root a (b (math (ensure-math c) (ensure-math d))))
        (root (a (math (ensure-math b) (symbol ((type "math")) c)
                       (ensure-math d)))
              (f (math (symbol ((type "math")) g) 
                       (symbol ((type "math")) h))))
        ))
    (for ([test-case tests]
          [result expected-results])
      (check-equal? (decode test-case 
                            #:txexpr-elements-proc combine-math-things)
                    result 
                    "combine-math-things failed"))))

; (or/c string? symbol?) (listof core?)
;   #:before-args (listof core?)
;   #:args (listof (listof core?))
;   #:opt-args (listof core?)
;   #:after-args (listof core?)
; The first argument is the name of the environment. The second
; argument is the list of body matter. 
; The general format of the output is:
;       \begin{name} before-args [ opt-args ] args after-args
;       body
;       \end{name}
; args functions like the arguments to a macro.
; opt-args does not. You may only have one optional argument in a
; latex macro/environment.
(define (environment name body
                     #:args [args null] 
                     #:opt-args [optional null]
                     #:before-args [before null]
                     #:after-args [after null])
  (txexpr 'environment 
          `((name ,(~a name)))
          `( ,(txexpr 'args null (store-args args))
             ,(txexpr 'opt-args null optional)
             ,(txexpr 'before-args null before)
             ,(txexpr 'after-args null after)
             ,(txexpr 'body null body))))

(module+ test
  (test-case
    "macros and environments"
    (define cases `(
        ,(macro 'testmacro '("a" "b" "c"))
        ,(macro 'testmacro 
                '("arg1-1" " " "arg1-2")
                '("arg2-1" " " "arg2-2" " " "arg2-3"))
        ))
    (define expected-results `(
        (macro ((start "{") (end "}") (name "testmacro"))
               (arg "a" "b" "c"))
        (macro ((start "{") (end "}") (name "testmacro"))
               (arg "arg1-1" " " "arg1-2")
               (arg "arg2-1" " " "arg2-2" " " "arg2-3"))
        ))
    (for ([the-case cases]
          [result expected-results])
      (check-equal? 
        (apply-tags 
          the-case
          (cons '$ (λ (tx) (apply $ (get-attrs tx) (get-elements tx))))
          (cons '$$ (λ (tx) (apply $$ (get-attrs tx) (get-elements tx)))))
          result))))


; txexpr-elements? symbol? -> (listof (or/c txexpr? string?))
; Takes a list of txexpr-element? and the name of a tag to regard as a
; list-item. The result is a list of list items and strings containing
; whitespace. Collects elements until a newline? is encountered, and
; wraps those elements in a txexpr of name list-tag.
; splits a list of txexpr-elements? where there are newline? elements,
; or there are tags of a predefined type (typically 'i). The tags
; where splits happened are kept, the newlines are thrown away.
(define (split-newline lst list-tag)
  (split-where 
    lst 
    (λ (current . _) 
       (or (newline? current)
           (is-tag? current list-tag)))
    #:keep-where 
    (λ (split-elem . _) 
       (if (txexpr? split-elem) 'separate 'ignore))
    #:split-map
    (λ (current . _) 
       (txexpr list-tag null current))))

; txexpr-elements? regexp? symbol? -> (listof txexpr?)
; Takes a list of txexpr-element?, a regular expression representing
; the prefix of a string that represents an element of a list, and
; finally the name of the txexpr that should represent an element of a
; list (this allows you to intermix plain-text list items and
; list-items as txexprs).
; The result is a list of txexpr? with the name given by list-tag.
(define (split-bullets lst bullet-pattern list-tag)
  ; Keep looking through whitespace until you see a bullet. If you
  ; don't find this bullet, then return false. Otherwise, return the
  ; series of whitespace and the bullet item itself.
  (define (scan-until-next-bullet items bullet?)
    (let ([item (first items)])
      (cond 
        [(whitespace? item)
         (let ([up-to-bullet (scan-until-next-bullet (rest items) bullet?)])
           (if up-to-bullet 
             (cons item up-to-bullet) 
             up-to-bullet))]
        [(bullet? item) (list item)]
        [else #f])))
  (define (bullet? elem) 
    (and (string? elem) 
         (regexp-match bullet-pattern elem)))
  (split-where
    lst
    (λ (elem current-split remaining)
       (or (is-tag? elem list-tag)
           (and (newline? elem) 
                (scan-until-next-bullet remaining bullet?))))
    #:keep-where 
    (λ (current . _) 
       (if (newline? current)
         'ignore
         'separate))
    #:split-map
    (λ (current . _)
       (if (txexpr? current) ; Was already an item.
         current
         (let-values ([(up-to-bullet rst)
                       (splitf-at current (λ (v) (or (whitespace? v)
                                                     (bullet? v))))])
           (txexpr list-tag null 
                   (cons (string-join up-to-bullet "")
                         rst)))))))

(define (remove-empty-items items)
  (decode-elements items
                   #:txexpr-proc 
                   (λ (tx) 
                      (if (and (is-tag? tx 'i)
                               ((listof whitespace?) (get-elements tx)))
                               null
                               tx))))
(define (l . elems)
  (let* ([items (split-bullets elems #px"^\\s*-" 'i)]
         [cleaned (remove-empty-items items)])
    (txexpr 'list '((type "unordered")) cleaned)))

(define (ol . elems)
  (let* ([items (split-bullets elems #px"^\\s*-" 'i)]
        [cleaned (remove-empty-items items)])
    (txexpr 'list '((type "ordered")) cleaned)))

(define (eql . elems)
  (let* ([items (split-newline elems 'i)]
        [cleaned (remove-empty-items items)])
    (txexpr 'list '((type "math")) items)))

; txexpr? string? -> txexpr?
; Takes in a txexpr whose elements are 'i tags and newline? The 'i
; tags must still have their bullets as their first element. Returns a
; 'list tag whose children are 'i tags and 'list tags. 
; - Consecutive series of bullets at the same indentation level will
;   be turned into 'list tags containing those items.
; - If an 'i tag appears whose indentation level is greater than the
;   level of the current 'i tag, then a new 'list tag will be started.
; - If an 'i tag appears whose indentation level is less than the
;   level of the current 'i tag, then the current 'list tag is ended.
(define (level-lists list-txexpr list-type)
  ; TODO: Make this calculation account for tabs with a tab width,
  ; optionally. Take away the dividing, too. There's no need to guard
  ; against weird indentation levels. We don't have to set which
  ; indents we do. All this code depends on is whether or not the
  ; current indentation level is >, =, or < then the current
  ; indentation level.
  (define (get-list-level item)
    (let* ([bullet (first (get-elements item))]
           [indent (first (regexp-match #px"^\\s*" bullet))])
      (quotient (string-length indent) 4)))
  ; so-far will contain a list of items and lists.
  ; list? list? -> (cons list? list?)
  ; This function takes in a list of things processed so far (to be
  ; explained in a bit), and a list of remaining stuff to process, and
  ; adds on the result of processing the remaining stuff onto the
  ; stuff processed so far. It returns (cons processed-stuff
  ; unprocessed-tokens).
  ; so-far is a list of items and lists.
  (define (descender so-far remaining-tokens)
    (cond [(null? so-far)
           (descender (list (first remaining-tokens)) 
                      (rest remaining-tokens))]
          [(null? remaining-tokens) 
           ; This is correct because descender always returns 
           ; (cons processed-stuff unprocessed-tokens)
           (cons so-far null)]
          [(not (is-tag? (first remaining-tokens) 'i))
           (descender (cons (first remaining-tokens) so-far)
                      (rest remaining-tokens))]
          [else
            (let* 
              ([elem (first remaining-tokens)]
               [rst (rest remaining-tokens)]
               ; NOTE: The stuff in so-far is contained in stack
               ; order. So the car is the thing most recently
               ; encountered. Thus the 1st encountered item tag is the
               ; previous item.
               [previous-item (findf (λ (v) (is-tag? v 'i)) so-far)]
               ; TODO: Consider making this a parameter of descender.
               ; Faster than constantly searching backward for the
               ; previous item.
               [current-level (if previous-item
                                (get-list-level previous-item)
                                0)]
               [elem-level (get-list-level elem)])
              (cond [(< current-level elem-level)
                     (match (descender null remaining-tokens)
                       [(cons sublist leftover)
                        (descender (cons sublist so-far) leftover)])]
                    [(> current-level elem-level) 
                     (cons so-far remaining-tokens)]
                    [else (descender (cons elem so-far) rst)]))]))
  (define (item-list? val) (and (list? val) (not (txexpr? val))))
  (define (item-list->list-tag val)
    (if (item-list? val)
      (txexpr 'list `((type ,list-type)) (map item-list->list-tag val))
      val))
  (let* ([final-so-far (first (descender null (get-elements list-txexpr)))]
         [correct-order 
           (reverse* 
             final-so-far
             (λ (v) (or (not (list? v)) (txexpr? v))))])
    (item-list->list-tag correct-order)))

;(define-tag-function 
  ;(row attrs elems) 
  ;(txexpr 'row null (split-list-at-tag-or-newline elems)))


; (or/c txexpr? txexpr-elements?) -> (or/c txexpr? txexpr-elements?)
; This takes a txexpr? or txexpr-elements? which should contain only
; core? and convenience tags. It will return a txexpr? or
; txexpr-elements? which contains only core? tags. A txexpr? is
; returned if the argument was a txexpr?. Otherwise txexpr-elements?
; are returned.
(define (convenience->core elements)
  (define (user-lists->list-tags txpr)
    (apply-tags 
      txpr
      (cons 'l
            (lambda (tx) 
              (level-lists (apply l (get-elements tx)) "unordered")))
      (cons 'ol 
            (lambda (tx) 
              (level-lists (apply ol (get-elements tx)) "ordered")))
      (cons 'eql
            (lambda (tx) 
              (apply eql 
                     (flatten-tags (get-elements tx)
                                   #:only '(math ensure-math)))))))
  (define (remove-bullet item-tag bullet-pattern)
    (let* ([1st (first (get-elements item-tag))]
           [no-bullet (regexp-replace bullet-pattern 1st "")])
      (txexpr (get-tag item-tag)
              (get-attrs item-tag)
              (cons no-bullet (rest (get-elements item-tag))))))
  (define (insert-elements txpr . elements)
    (txexpr (get-tag txpr)
            (get-attrs txpr)
            (append elements (get-elements txpr))))
  ; txexpr? -> txexpr?
  ; Takes an 'i tag and removes the bullet, replacing it with an 'item
  ; macro.
  (define (transform-items txpr)
    (decode
      txpr
      #:txexpr-proc
      (λ (tx)
         (if (and (is-tag? tx 'list)
                  (member (attr-ref tx 'type)
                          '("unordered" "ordered")))
           (apply-tags-to-children 
             tx
             (cons 'i 
                   (λ (tx)
                      (insert-elements 
                        (remove-bullet tx #px"^\\s*-\\s*")
                        (macro 'item) " "))))
           tx))))
  (define (list-tags->core txpr)
    (apply-tags
      txpr
      (cons 'list
            (lambda (tx)
              (let* ([type (attr-ref tx 'type)]
                     [environment-name
                       (case type
                         [("unordered") 'itemize]
                         [("ordered") 'enumerate]
                         [("math") 'align*]
                         [else (~a type)])]
                     [items (get-elements (transform-items tx))]
                     [spaced-out-items
                       (case type 
                         [("unordered" "ordered")
                          (add-between items "\n")]
                         [("math")
                          (add-between items "\\\\\n")])]
                     [flattened-items
                       (flatten-tags spaced-out-items #:only '(i))])
                (environment environment-name flattened-items))))))
  (define (to-each-element txpr)
    (list-tags->core
      (user-lists->list-tags txpr)))
  (if (txexpr? elements)
    (to-each-element elements)
    (append-map to-each-element elements)))

; (or/c txexpr? txexpr-elements?) -> (or/c txexpr? txexpr-elements?)
; If this gets a txexpr? then it will return a txexpr? which contains
; entirely strings. The descendants of the txexpr? are expected to all
; be core?.
; If this gets txexpr-elements? then it will return (listof string?). 
; The txexpr-elements? are expected to be (listof core?).
(define (core->ltx elements)
  ; Like select*, but always returns a list.
  (define (diff-select* tag-name element)
    (let ([result (select* tag-name element)])
      (if result
        result
        null)))
  ; Turns a macro into a string, assuming that all of it's arguments
  ; are strings, too.
  ; When this function is used as part of to-each-element, this
  ; assumption will be true, since macro->string will complete first
  ; for macros whose arguments contain no macros.
  (define (macro->string macro-tag)
    (let* ([name (format "\\~a" (attr-ref macro-tag 'name))]
           [args 
             (map (λ (e) (string-join (get-elements e) "")) 
                  ;(report (get-elements macro-tag)))]
                  (get-elements macro-tag))]
           [formatted-args (if (null? args)
                             '("{}")
                             (macro-args args))])
      (string-append
        name
        (string-join formatted-args ""))))
  (define (to-each-element tx)
    (@-flatten
      (apply-tags
        (math-flatten 
          (decode #:txexpr-elements-proc combine-math-things tx))
        (cons 'macro macro->string)
        (cons
          'environment
          (λ (tx)
             (let ([name (attr-ref tx 'name)]
                   [opt-args (diff-select* 'opt-args tx)]
                   [before-args (diff-select* 'before-args tx)]
                   [after-args (diff-select* 'after-args tx)]
                   [args 
                     (map (λ (e) (string-join e "")) 
                          (diff-select* 'args tx))]
                   [body (diff-select* 'body tx)])
               (string-append
                 (report (macro->string (macro-1 'begin name)))
                 (string-join before-args)
                 (if (null? opt-args)
                   ""
                   (string-join `("[" ,@opt-args "]")))
                 (string-join (macro-args args))
                 "\n"
                 (string-join body "")
                 "\n"
                 (macro->string (macro-1 'end name))))))
        (cons 
          'math
          (λ (tx) 
             (let ([surround (if (has-attr? tx 'display) "$$" "$")])
               (string-join `( ,surround 
                               ,@(get-elements tx) 
                               ,surround) "")))))))
  (if (txexpr? elements)
    ; Should return txexpr? unless the root tag is @.
    (to-each-element elements)
    ; Should return txexpr-elements?.
    (append-map to-each-element elements)))

; txexpr-elements? -> (listof string?)
(define (->ltx elements)
  (core->ltx (convenience->core elements)))

(module+ test
  (test-case
    "core->ltx"
    (define cases `(
        (root (math "a"))
        (root (macro ((start "{") (end "}") (name "testmacro"))
                     (arg "a")))
        (root (math (ensure-math "a")) " " (ensure-math "b"))
        ))
    (define expected-results `(
        (root "$a$")
        (root "\\testmacro{a}")
        (root "$a b$")
        ))
    (for ([the-case cases]
          [result expected-results])
      (check-equal? (core->ltx the-case) result))))

; In reference to a comment in decode.rkt line 59 in the pollen
; repository:
;
; I think I understand what he means by avoiding recursive descent.
; The decode function could've gone more like:
; (cond [(tag? x) (tag-proc x)]
;       [(attrs? x) (attrs-proc x)]
;       [(txexpr? x) (txexpr-proc x)]
;       ...
; However, if we did things this way, then what would a value like
; '((p "thing")) be? Is it a list of attributes, or a list of
; txexpr?
; We know the answer to this if we work with the whole tag at the same
; time, so he breaks apart a txexpr, transforms the pieces, then puts
; them back together appropriately. He does recursive descent on the
; child elements, which is the correct thing to do.

; So, assuming that decode works correctly (I believe it), by the time
; any txexpr will get processed, it's children have already been
; processed. All the children get decoded before the current element
; gets decoded. So, for my manual traversal, all the children of an
; element have been traversed before the current element has been
; traversed. Thus, each tag function can safely assume that all of its
; children are strings.

; TODO:
; - Math behavior.
;   - Display math: If math is display math, then handle the output
;     correctly. Currently, only inline math is supported.
;   - Merging adjacent math elements
;   - Create the 'symbol tag. 
;       - The symbol tag should contain core? that should be
;         interpreted as a single character, such as "\\lambda" or
;         (macro 'lambda). These will result as a single character in
;         pdf output and thus should be treated this way. 
;       - Include an attribute for "math-only" or something like that.
;         For sure, such symbols should be wrapped in math-mode
;         automatically, or merged with adjacent math elements.
;   - Merge adjacent math elements and 'symbol tags that require
;     math mode.
;       - This behavior may be undesirable at times. So only merge
;         'ensure-math with other 'math tags ('ensure-math or 'math). Do
;         not merge 'math with 'math, as the intent of the 'math tag is
;         to be explictly used by the user, and otherwise 'ensure-math
;         is used to force math mode just in case.
; - tables.
; - Escaping special characters
;   - Escaping & outside of align and tables.
;   - Escaping % almost always (except in something like lstlisting
;     environments or lstlinline commands).
;   - Escaping lone backslashes.
;   - This is probably done easiest by escaping all special characters
;     early on, leaving special meaning characters to be created by
;     tags and macros and such.
; - Make the lists more flexible. Right now, a lot of the list logic
;   is split up amongst the logic of splitting up a list of items, and
;   different tag functions. Separate out the list stuff and make an
;   interface to it where you can decide how the raw txexpr-elements?
;   become list items and sublists, and what to do with each item in
;   the end, as well as what environment to use and such. This would
;   make it easier to create new lists on the fly, which would be
;   good.
;     - Try to make the current lists you have with some sort of
;       interface. Don't actually program it. It's wishful thinking
;       for now, just write some stuff out that looks like it ought to
;       create the same list functionality. Important things that can
;       change:
;         - How do you separate the raw stuff into items?
;         - How do you determine if an item should be in the same list
;           or go into a new list?
;         - Once the processing of items is done, how will you
;           transform the items into their final forms in the
;           document?
;         - What do you do with the list items as a whole once they've
;           been transformed?
;     - Make sure that all stuff related to "bullets" in a list are
;       contained in one place. Once a bullet is detected, it should
;       also be removed. The bullet should contain all list
;       information: the marker for a bullet and indentation level.
;       That should then be placed on an item in a uniform way that
;       has notthing to do with what the original list item looked
;       like.
; Make something where you can decide
;   if and how the list of items gets "levelled out" into sublists.
;   Give the 
