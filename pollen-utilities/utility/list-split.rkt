#lang racket
(require "../utility.rkt"
         txexpr
         pollen/decode)
(provide listify)
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

(define list-tag 'i)

; txexpr-elements? regexp? -> (listof txexpr?)
; This function takes the contents of a list tag---which should be a
; series of bullets---and prepares them for use in a list tag. It
; - Detects where bullet points end, separating all the bullet points
;   into individual txexpr. These txexpr will be 'list-tag.
; - Detects the bullets indentation levels and places that indentation
;   level as an attribute on each 'list-tag.
; Takes a regular expression representing the prefix of a string that
; represents an element of a list.
; The result is a list of txexpr? with the name given by 'list-tag.
(define (prepare-list lst bullet-pattern)
  ; Keep looking through whitespace until you see a bullet. If you
  ; don't find this bullet, then return false. Otherwise, return the
  ; series of whitespace and the bullet item itself.
  ; I use this to ask if items is of the form 
  ;   (list whitespace? ...* bullet? ...)
  ; If the list is like that, then returns the prefix 
  ;     (list whitespace? ...* bullet?)
  ; Otherwise returns #f.
  (define (scan-until-next-bullet items bullet?)
    (let loop ([items items])
      (define item (first items))
      (cond 
        [(whitespace? item)
         (define up-to-bullet (loop (rest items)))
         (if up-to-bullet 
           (cons item up-to-bullet) 
           up-to-bullet)]
        [(bullet? item) (list item)]
        [else #f])))
  (define (bullet? elem) 
    (and (string? elem) 
         (regexp-match bullet-pattern elem)))
  ; Given a 'list-tag txexpr, returns the appropriate nesting level of
  ; the item.
  (define (get-list-level bullet)
    (define indent (first (regexp-match #px"^\\s*" bullet)))
    (quotient (string-length indent) 4))


  ; This will be a list of 'list-tag and newline?
  (define list-of-items
    (split-where
      lst
      ; split at newlines which are followed by whitespace and a
      ; bullet.
      (λ (elem current-split remaining)
         (and (newline? elem) 
              (scan-until-next-bullet remaining bullet?)))
      #:keep-where 
      (λ _ 'ignore)
      #:split-map
      (λ (current . _)
         (define-values (indent bullet-and-rst)
           (splitf-at current whitespace?))
         ; TODO: Preceeding newlines would become part of indentation
         ; level. That's not right.
         (define content-only 
           (string-trim
             (regexp-replace bullet-pattern 
                             (first bullet-and-rst)
                             "")))
         (define bullet 
           (string-join (append indent (list (first bullet-and-rst))) ""))
         (define level (get-list-level bullet))
         (txexpr list-tag `((level ,(~a level))) 
                 (cons content-only (rest bullet-and-rst))))))
  (define (remove-empty-items items)
    (decode-elements 
      items
      #:txexpr-proc 
      (λ (tx) 
         (if (and (is-tag? tx 'i)
                  ((listof whitespace?) (get-elements tx)))
           null
           tx))))
  (remove-empty-items list-of-items))

; txexpr? string? -> txexpr?
; Takes in a txexpr whose elements are 'i tags. Returns a 'list-tag
; whose children are 'i tags and 'list tags. 
; - Consecutive series of bullets at the same indentation level will
;   be turned into 'list tags containing those items.
; - If an 'i tag appears whose indentation level is greater than the
;   level of the current 'i tag, then a new 'list tag will be started.
; - If an 'i tag appears whose indentation level is less than the
;   level of the current 'i tag, then the current 'list tag is ended.
(define (level-lists list-elements list-name)
  (define (make-lists)
    ; Appends element to the end of the last nested list being built.
    (define (stack-item-lst stack-item)
      (car stack-item))
    (define (stack-item-level stack-item)
      (cdr stack-item))
    (define (add-to-current-list new-list-stack elem)
      (define stack-bottom (rest new-list-stack))
      (define top (first new-list-stack))
      (define-values (lst level) 
        (values
          (stack-item-lst top)
          (stack-item-level top)))
      (define new-lst (cons elem lst))
      (define new-top (cons new-lst level))
      (cons new-top stack-bottom))
    (define (current-stack-level new-list-stack)
      (stack-item-level (first new-list-stack)))
    (define (current-stack-list new-list-stack)
      (stack-item-lst (first new-list-stack)))
    (define (prev-item new-list-stack)
      (car (car new-list-stack)))
    ; stop-at is the nesting level where we should continue building
    ; our list. All of the elements in a single element of
    ; new-list-stack should have the same level. Call that the level
    ; of the entire element on the stack. All elements of the stack
    ; have strictly increasing levels.
    ; We want to nest elements on the stack within the element beneath
    ; them. If an element have level higher than stop-at, then nest
    ; it. If it doesn't, then stop.
    (define (stack->list-tag new-list-stack stop-at)
      (define (stack-top-level stack)
        (define top (first stack))
        (string->number (attr-ref (first top) 'level)))
      (let loop ([stack new-list-stack])
        (define top-as-list
          (txexpr list-name null (reverse (current-stack-list stack))))
        (cond
          [(<= (current-stack-level stack) stop-at) 
           stack]
          [(= (length stack) 1) top-as-list]
          [else
            ; puts the top-as-list at the top of the second list
            ; (everything here is in stack order: the new-list-stack
            ; itself and the list item tags in each list).
            ;(define new-second 
              ;(cons top-as-list (second stack)))
            ;(loop (cons new-second (cddr stack)))])))
            (loop (add-to-current-list (rest stack) top-as-list))])))
    (define (make-final-list the-lists new-list-stack)
      (cond 
        [(null? new-list-stack)
         the-lists]
        [else
          ; There's no reason to stop nested. There are no more
          ; elements. So stop-at is -1.
          (define last-new-list (stack->list-tag new-list-stack -1))
          (reverse (cons last-new-list the-lists))]))
    ; - the-lists is a stack of the lists we've fully created so far.
    ; - new-list-stack will contain a list under construction. A list is
    ; under construction while we're finding elements that belong in
    ; the list, or within a list to be nested. The b
    ; - Once a list has been finished, 
    (for/fold [(new-list-stack null)
               #:result (stack->list-tag new-list-stack -1)]

      [(elem list-elements)]
      new-list-stack
      (define elem-level (string->number (attr-ref elem 'level)))
      (cond
        [(null? new-list-stack) 
         (cons (cons (list elem) elem-level) new-list-stack)]
        [else
          (define current-level (current-stack-level new-list-stack))
          current-level
          (cond
            ; nest lists. new-list-stack contains lists to be nested.
            [(< current-level elem-level)
             (cons 
               (cons (list elem) elem-level)
               new-list-stack)]
            ; stop nesting lists: turn the list stack into one nested
            ; list, now with all elements in the correct (non-stack)
            ; order, and make that the newest-list to add onto
            ; the-lists.
            [(> current-level elem-level)
             (define new-list (stack->list-tag new-list-stack elem-level))
             (add-to-current-list new-list elem)]
            [else (add-to-current-list new-list-stack elem)])])))

  (make-lists))

(define (listify list-elements bullet-pattern list-name)
  (define prepared-elements 
    (prepare-list list-elements bullet-pattern))
  (level-lists prepared-elements list-name))

(module+ test
  (define bullet? #px"^\\s*-")
  (test-case
    "one element and one level"
    (define the-case '(ul "- list text."))
    (define result `(ul (i ((level "0")) "list text.")))
    (check-equal? (listify (get-elements the-case) bullet? 'ul) result))
  (test-case
    "two elements and one level"
    (define the-case '(ul "- list text." "\n" "- more list text."))
    (define result `(ul (i ((level "0")) "list text.")
                        (i ((level "0")) "more list text.")))
    (check-equal? (listify (get-elements the-case) bullet? 'ul) result))
  (test-case
    "levels: one two"
    (define the-case '(ul "- list text." "\n" "    - more list text."))
    (define result `(ul (i ((level "0")) "list text.")
                        (ul (i ((level "1")) "more list text."))))
    (check-equal? (listify (get-elements the-case) bullet? 'ul) result))
  (test-case
    "levels: one two two one"
    (define the-case '(ul "- one:1 and text."
                          "\n" 
                          "    - two:1 and text."
                          "\n" 
                          "    - two:2 and text."
                          "\n" 
                          "- one:2 and text."))
    (define result `(ul (i ((level "0"))  "one:1 and text.")
                        (ul (i ((level "1")) "two:1 and text.")
                            (i ((level "1")) "two:2 and text."))
                        (i ((level "0"))  "one:2 and text.")))
    (check-equal? (listify (get-elements the-case) bullet? 'ul) result))
  (test-case
    "levels: one two three two one"
    (define the-case '(ul "- one:1 and text."
                          "\n" 
                          "    - two:1 and text."
                          "\n" 
                          "        - three:1 and text."
                          "\n" 
                          "    - two:2 and text."
                          "\n" 
                          "- one:2 and text."))
    (define result `(ul (i ((level "0"))  "one:1 and text.")
                        (ul (i ((level "1")) "two:1 and text.")
                            (ul (i ((level "2")) "three:1 and text."))
                            (i ((level "1")) "two:2 and text."))
                        (i ((level "0"))  "one:2 and text.")))
    (check-equal? (listify (get-elements the-case) bullet? 'ul) result))
  (test-case
    "levels: one two three one"
    (define the-case '(ul "- one:1 and text."
                          "\n" 
                          "    - two:1 and text."
                          "\n" 
                          "    - two:2 and text."
                          "\n" 
                          "        - three:1 and text."
                          "\n" 
                          "- one:2 and text."))
    (define result `(ul (i ((level "0"))  "one:1 and text.")
                        (ul (i ((level "1")) "two:1 and text.")
                            (i ((level "1")) "two:2 and text.")
                            (ul (i ((level "2")) "three:1 and text.")))
                        (i ((level "0"))  "one:2 and text.")))
    (check-equal? (listify (get-elements the-case) bullet? 'ul) result))
  (test-case
    "odd HTML thing"
    (define the-case '(ul "- Switch walls 2 and 3?"
                          "\n" 
                          (p 
                            "3|       " "\n"
                            "2|* w *  " "\n"
                            "1|* w * *" "\n"
                            "0|-------" "\n"
                            "  0 1 2 3" "\n"
                            )
                          "\n" 
                          "- Add height 0, 1, or 2 walls between walls 2 and 3? In the diagram"
                          "\n"
                          "position 6 is what used to be position 3."
                          (p
                            "3|             " "\n"
                            "2|* w       * *" "\n"
                            "1|* w * *   * *" "\n"
                            "0|-------------" "\n"
                            "  0 1 2 3 4 5 6" "\n"
                            )
                          "\n" 
                          "- Add height 0 or 1 walls between walls 0 and 1?"))
    (define result `(ul (i ((level "0"))
                           "Switch walls 2 and 3?"
                          "\n" 
                          (p 
                            "3|       " "\n"
                            "2|* w *  " "\n"
                            "1|* w * *" "\n"
                            "0|-------" "\n"
                            "  0 1 2 3" "\n"
                            ))
                        (i ((level "0"))
                          "Add height 0, 1, or 2 walls between walls 2 and 3? In the diagram"
                          "\n"
                          "position 6 is what used to be position 3."
                          (p
                            "3|             " "\n"
                            "2|* w       * *" "\n"
                            "1|* w * *   * *" "\n"
                            "0|-------------" "\n"
                            "  0 1 2 3 4 5 6" "\n"
                            ))
                        (i ((level "0")) "Add height 0 or 1 walls between walls 0 and 1?")))
    (check-equal? (listify (get-elements the-case) bullet? 'ul) result)))
