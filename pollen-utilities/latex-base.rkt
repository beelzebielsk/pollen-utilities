#lang racket
(require 
  "utility.rkt" "utility/manual-traverse.rkt"
  txexpr pollen/decode pollen/core pollen/tag)
(provide macro macro-1 environment group define-math-tag ->ltx $ $$
         ensure-math)
(provide split-bullets store-args)

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
(define (math-flatten xexpr)
  (cond [(is-tag? xexpr 'math)
         (flatten-tags xexpr #:only '(ensure-math))]
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
; symbol (character). Example use: (tex-symbol (macro 'pi)). That
; macro will become the greek lowercase letter pi.
; TODO: Handle non-math-mode symbols. I THINK that they exist, but I'm
; not sure.
(define (tex-symbol . args)
  (txexpr 'symbol
          `((requires-math "true"))
          args))

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


; txexpr-elements? -> txexpr?
; This takes one of the convenience layer tags and transforms it into
; core-level tags. All user-defined tags should already be strings by
; this point in time.
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
    (to-each-element elements)
    (append-map to-each-element elements)))

; txexpr-elements? -> (listof string?)
(define (->ltx elements)
  (core->ltx (convenience->core elements)))

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
; - tables.
; - Escaping special characters
;   - Escaping & outside of align and tables.
;   - Escaping % almost always (except in something like lstlisting
;     environments or lstlinline commands).
;   - Escaping lone backslashes.
