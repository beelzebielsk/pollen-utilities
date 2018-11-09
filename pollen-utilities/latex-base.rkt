#lang racket
(require 
  "utility.rkt" "utility/manual-traverse.rkt"
  txexpr pollen/decode pollen/core pollen/tag)
(provide macro environment group define-math-tag ->ltx $ $$ ensure-math)
(provide split-bullets store-args)
;(provide (all-defined-out))

(define (whitespace? val)
  (and (string? val) (regexp-match #px"^\\s*$" val)))
(define (newline? val)
  (and (whitespace? val) (string-contains? val "\n")))
(define (math-flatten xexpr)
  (cond [(is-tag? xexpr 'math)
         (decode xexpr
                 #:txexpr-proc (decode-flattener #:only '(ensure-math)))]
        ; We know that this is not contained within a math tag, for if
        ; it were, the above branch would have been taken already, and
        ; decode would've been applied to this tag.
        [(is-tag? xexpr 'ensure-math)
         (math-flatten (txexpr 'math null (get-elements xexpr)))]
        [(txexpr? xexpr)
         (map math-flatten xexpr)]
        [else xexpr]))

(define (group . _)
  (list-splice `("{" ,@_ "}")))
(define math (default-tag-function 'math))
(define ensure-math (default-tag-function 'ensure-math))
(define-tag-function ($ _ text)
  (apply math text))
(define-tag-function ($$ _ text)
  (apply math #:display "" text))

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
;   - list : Moving this here because there's a lot of necessary
;     transformations in going from a 'l 'ol or 'eql to a 'list. It's
;     best to keep the transformation as part of ->ltx.
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

(define (macro-args args)
  (add-between args '("}{")
               #:splice? #t
               #:before-first '("{")
               #:after-last '("}")))
(define (store-args args)
  (map (λ (a) (txexpr 'arg null a)) args))

; (or/c string? symbol?) (listof core?) ...
; This tag represents a latex macro. The first argument is the name of
; the macro.
; The remaining arguments are the arguments to the macro, where each
; argument is a list of (or/c string? core-tag?).
(define (macro name . args)
  (txexpr 'macro 
          `((start "{") (end "}") (name ,(~a name)))
          (store-args args)))

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

(define (split-bullets lst bullet-pattern list-tag)
  ; Keep looking through whitespace until you see a bullet. If you
  ; don't find this bullet, then return nothing. Otherwise, return the
  ; series of whitespace and the bullet item itself.
  (define (scan-until-next-bullet items bullet?)
    (let ([item (first items)])
      (cond [(whitespace? item)
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
                   (λ (tx) (if (and (is-tag? tx 'i)
                                    ((listof newline?) (get-elements tx)))
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
  (define (get-list-level item)
    (let* ([bullet (first (get-elements item))]
           [indent (first (regexp-match #px"^\\s*" bullet))])
      (quotient (string-length indent) 4)))
  ; so-far will contain a list of items and lists.
  ; list? list? -> (cons list? list?)
  ; This function takes in a list of things processed so far (to be
  ; explained in a bit), and a list of remaining stuff to process, and
  ; adds on the result of processing the remaining stuff onto the
  ; stuff processed so far.
  ; so-far is a list of items and lists.
  (define (descender so-far remaining-tokens)
    (cond [(null? so-far)
           (descender (list (first remaining-tokens)) 
                      (rest remaining-tokens))]
          [(null? remaining-tokens) 
           (cons so-far null)]
          [(not (is-tag? (first remaining-tokens) 'i))
           (descender (cons (first remaining-tokens) so-far)
                      (rest remaining-tokens))]
          [else
            (let* 
              ([elem (first remaining-tokens)]
               [rst (rest remaining-tokens)]
               [previous-item (findf (λ (v) (is-tag? v 'i)) so-far)]
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


; txexpr? -> txexpr?
; This takes one of the convenience layer tags and transforms it into
; core-level tags. All user-defined tags should already be strings by
; this point in time.
(define (convenience->core txpr)
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
              (apply eql (get-elements tx))))))
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
                         [("math") 'align*])]
                     [items (get-elements (transform-items tx))]
                     [spaced-out-items
                       (case type 
                         [("unordered" "ordered")
                          (add-between items "\n")]
                         [("math")
                          (add-between items "\\\\\n")])]
                     [flattened-items
                       (decode-elements 
                         spaced-out-items
                         #:txexpr-proc (decode-flattener #:only '(i)))])
                (environment environment-name flattened-items))))))
  (list-tags->core
    (user-lists->list-tags txpr)))

(define (core->ltx elements)
  ; Just redoing select* so that I always get back a list of stuff.
  (define (diff-select* tag-name element)
    (let ([result (select* tag-name element)])
      (if result
        result
        null)))
  ; THis is a misnomer. This transforms part of the macro to strings,
  ; but it does not transform the arguments to strings.
  ; However, if you apply this tag recursively to a macro:
  ; macros contain the following tags, which contain the following
  ; things:
  ; name : symbol?
  ; start : string?
  ; end : string?
  ; args : (listof arg)
  ; arg : (listof (or/c string? macro?))
  ; The base case for recursive application of macro->string (as if it
  ; were defined 
  ; (define (macro->string macro-tag)
  ;     ...
  ;     (let ([new-args 
  ;            (for/list ([i args]) (macro->string i))]) ...)
  ; The base case for this function is a macro whose args are all
  ; strings. In this case, macro->string does return a string.
  ; Suppose that for all macros with macro depth n or less,
  ; macro->string returns a string. Suppose also that we are calling
  ; macro->string on a macro of macro depth n + 1.
  ; Well, the return value of macro-> string is
  ; - it's formatted name, which is a string.
  ; - A list, which starts and ends with "{" and "}" respectively, and
  ;   in between is all the arguments of the macro with the string
  ;   "}{" in between them. 
  ; - The macro arguments have already been placed through
  ;   macro->string, and they must have macro depth of n or less, 
  ;   (strings have macro depth 0, a macro has macro depth 1 plus the
  ;   depth of the deepest argument).
  ; Applying this proof to what occurs below is straightforward, I
  ; think. Instead of "macro depth" it's "tag depth", where the only
  ; tags are the core tags. Strings have core tag depth 0, core tags
  ; have a depth of 1 plus the depth of the deepest core tag that they
  ; contain.
  ; From here on out the proof ought to be pretty much the same.
  ; This proof works assuming that the only tags present in the
  ; document are core tags (the tags handled by to-each-element).
  (define (macro->string macro-tag)
    (let* ([name (format "\\~a" (attr-ref macro-tag 'name))]
           [args 
             (map (λ (e) (string-join (get-elements e) "")) 
                  (report (get-elements macro-tag)))]
           [formatted-args (if (null? args)
                             '("{}")
                             (macro-args args))])
      (string-append
        name
        (string-join formatted-args ""))))
  (define (to-each-element tx)
    (@-flatten
      (apply-tags
        (math-flatten tx)
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
                 (report (macro->string (macro 'begin (list name))))
                 (string-join before-args)
                 (if (null? opt-args)
                   ""
                   (string-join `("[" ,@opt-args "]")))
                 (string-join (macro-args args))
                 "\n"
                 (string-join body "")
                 "\n"
                 (macro->string (macro 'end (list name)))))))
        (cons 
          'math
          (λ (tx) (string-join `("$" ,@(get-elements tx) "$") ""))))))
  (if (txexpr? elements)
    (to-each-element elements)
    (append-map to-each-element elements)))
(define (->ltx elements)
  (core->ltx (convenience->core elements)))


; I think I understand what he means by avoiding recursive descent.
; The decode function could've gone more like:
; (cond [(tag? x) (tag-proc x)]
;       [(attrs? x) (attrs-proc x)]
;       [(txexpr? x) (txexpr-proc x)]
;       ...
; However, if we did things this way, then what would a value like
; '((p "thing")) be? Is it a list of attributes, or a list of
; txexpr?
; We have that knowledge if we work with the whole tag at the same
; time, so he breaks apart a txexpr, transforms the pieces, then puts
; them back together appropriately. He does recursive descent on the
; child elements, which is the correct thing to do.

; So, assuming that decode works correctly (I believe it), by the time
; any txexpr will get processed, it's children have already been
; processed. All the children get decoded before the current element
; gets decoded. So, for my manual traversal, all the children of an
; element have been traversed before the current element has been
; traversed.

; TODO:
; - the macro and environment functions have to be available for the
;   pollen file. 
; - Math behavior.
; - tables.
