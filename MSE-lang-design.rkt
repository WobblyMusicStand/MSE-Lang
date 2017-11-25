#lang plai


;; ==========================================================
;;                     EBNF & DEFINE-TYPES
;; ==========================================================


;; Sample markov table

;; C B C D

; C | B D
; B | C
; D | C ; Implicitly first note follows the last note

; Example : (markov '(C B C D) 11) (C B C D C B C D C B C)


;; MSE = Music Writing Expression
;;
;; <MSE> ::= <num>
;;     | <id>
;;     | {note <num> <num> <num>}
;;     | {sequence <MSE>*}
;;     | {seqn-p <MSE>*}                            ; A list of given notes in letter format (e.g. A4 E5)
;;     | {seq-append <MSE> to <MSE>}                ; Appends the 2nd MSE Expression to
;;                                                    the end of the first
;;     | {with {<id> <MSE>} <MSE>}     
;;     | {fun {<id> <MSE>} <MSE>}
;;     | {<MSE> <MSE> }                             ; function calls, any time the first symbol is not a reserved word
;;     | {interleave <MSE> into <MSE>}              ; Takes first element of 1st MSE, appends to 1st element of 2nd MSE ... 
;;     | {insert <MSE> in <MSE> at <num>}        ; Inserts MSE in MSE at index number
;;     | {transpose <MSE> <num>}                 ; Takes a sequence of notes and a number, and
;;                                                    adds that number to the pitch of all notes in the sequence
;;     | {markov <MSE> <num> <MSE>?}             ; Runs markov chain (with a depth of 1) as shown in the example above

; Note Message is 3 bytes
; Pitch (0 - 127)
; Velocity (How hard you hit it) (0 - 127)
; Duration (Time in milliseconds)


;;;;;;;;;;;;;;;;   Type-Definitions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MSE, for desugar
(define-type MSE
  [num (n number?)]
  [id (name symbol?)]
  [note (pitch num?) (vel num?) (dur num?)] ; Pitch, Velocity, Duration
  [sequence (values (listof note?))]                  ; Distributions must not be empty and strictly evaluate to a (listof Note)
  [seqn-p (values (listof MSE?))]
  [seq-append (list1 MSE?) (list2 MSE?)]              
  [with (id symbol?) (expr MSE?) (body MSE?)]
  [fun (param symbol?) (body MSE?)]
  [app (function MSE?) (arg MSE?)]
  [interleave (list1 MSE?) (list2 MSE?)]
  [insert (list1 MSE?) (list2 MSE?) (index num?)]
  [transpose (list1 MSE?) (add-val num?)]
  [changeVelocity (list1 MSE?) (val num?)]
  [markov (seed MSE?) (length num?) (initial-note MSE?)] ; initial-note has to evaluate to a note
  )

;; D-MSE, for interpreter
;; exclude seq-append and with
(define-type D-MSE
  [i-num (n number?)]
  [i-id (name symbol?)]
  [i-note (pitch D-MSE?) (vel D-MSE?) (dur D-MSE?)]
  [i-sequence (values (listof i-note?))]
  [i-seqn-p (values (listof D-MSE?))]
  [i-fun (param symbol?) (body D-MSE?)]
  [i-app (function D-MSE?) (arg D-MSE?)]
  [i-interleave (list1 D-MSE?) (list2 D-MSE?)]
  [i-insert (list1 D-MSE?)(list2 D-MSE?)(index D-MSE?)]
  [i-transpose (list1 D-MSE?)(add-val i-num?)]
  [changeProp (list1 D-MSE?) (val i-num?) (pos i-num?)]
  [i-markov (seed D-MSE?)(length i-num?)(initial-note D-MSE?)]
  )

;; Interpreting a value returns a Value
(define-type MSE-Value
  ; Sequence of  Notes
  [pitchV (pit number?)]
  [velV (vel number?)]
  [durV (dur number?)]
  [noteV (p pitchV?) (vel velV?) (dur durV?)]
  [seqV (values (and/c (listof MSE-Value?) (not/c empty?)))]  
  [closureV (param symbol?)  ;;Closures wrap an unevaluated function body with its parameter and environment
            (body D-MSE?)
            (env Env?)])

;; Environments store values, instead of substitutions
(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value MSE-Value?) (env Env?)])

;; ID's which cannot be used by the user.
;TODO, update list to include all functions
(define *reserved-symbols* '(note
                             sequence
                             seqn-p
                             seq-append
                             with
                             fun
                             interleave
                             insert
                             transpose
                             changeVelocity
                             markov)) ; defining what the reserved symbols of the system are


;;;;;;;;;;;;;;;;   PARSER   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; valid-identifier? : symbol -> boolean
;; Returns whether the given input is a symbol and a valid identifier
(define (valid-id? sym)
  (and (symbol? sym)
       (and (match (symbol->string sym)
              [(regexp #rx"[A-G](#|b)*[0-9]+$") false]
              [else true])
            (not (member sym *reserved-symbols*)))))

  ;;parse s-exp -> MSE
  ;;Parses s-exp input and does valid-id checking for binding sites
  (define (parse sexp)
    (match sexp
      [(? number?) (num sexp)]
      [(? symbol?) (id sexp)]
      [(list 'note pitch vel dur) (note (parse pitch) (parse vel) (parse dur))]
      [(cons 'sequence notes) (sequence (map parse notes))]
      [(cons 'seqn-p notes) (seqn-p  (map parse notes))]
      [(list 'seq-append list1 list2) (seq-append (parse list1) (parse list2))]
      [(list 'with (list (? valid-id? id) value) body) (with id (parse value) (parse body))]
      [(list 'fun (? valid-id? param) body) (fun param (parse body))]
      ;TODO Explicitly cause invalid-id error for fun and with?
      [(list (and f-expr (? (lambda (s) (not (member s *reserved-symbols*))))) a-expr)
       (app (parse f-expr) (parse a-expr))]
      [(list 'interleave list1 list2) (interleave (parse list1) (parse list2))]
      [(list 'insert list1 list2 index) (insert (parse list1) (parse list2) (parse index))]
      [(list 'transpose list1 add-val) (transpose (parse list1) (parse add-val))]
      [(list 'changeVelocity list1 val) (changeVelocity (parse list1) (parse val))]
      [(list 'markov seed length initial-note) (markov (parse seed) (parse length) (parse initial-note))]
      [else (error "Illegal Expression")]))





  ;;;;;;;;;;;;;;;;   DESUGARER   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;desugar MSE -> D-MSE
  (define (desugar p-mse)
    (type-case MSE p-mse
      [num (n) (i-num n)]
      [id (val) (i-id val)]
      [note (p v d) (i-note (desugar p)
                            (desugar v)
                            (desugar d))]
      [sequence (vals) (i-sequence (map desugar vals))]
      [seqn-p (vals) (i-seqn-p (map desugar vals))]
      ;TODO seq-append support for IDs
      [seq-append (seq1 seq2)
                  (i-insert (desugar seq1)
                            (desugar seq2)
                            (type-case MSE seq1
                                             [sequence (s) (i-num (length (i-sequence-values (desugar seq1))))]
                                             [seqn-p (s) (i-num (length (i-seqn-p-values (desugar seq1))))]
                                             [id (var) (desugar seq1)]
                                             [else (error "need a sequence or seqn-p")]))] 
      [with (id named-expr body) (desugar (app (fun id body) named-expr))]
      [fun (param body) (i-fun param (desugar body))]
      [app (fn-exp arg-exp) (i-app (desugar fn-exp)
                                   (desugar arg-exp))]
      [interleave (lst1 lst2)
                  (i-interleave (desugar lst1)(desugar lst2))]
      [insert (lst1 lst2 index)
              (i-insert (desugar lst1)(desugar lst2)(desugar index))]
      [transpose (lst1 anum )
                 (i-transpose (desugar lst1)(desugar anum))]
      [changeVelocity (list1 val) (changeProp (desugar list1) (desugar val) (i-num 2))]
      [markov (s lth ini)
              (i-markov (desugar s)(desugar lth)(desugar ini))]
      ))


  ;;;;;;;;;;;;;;;;   INTERPRETER   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;decode-pitch symbol -> number
  ;;name must be castable and pass regex to get here
  (define (decode-pitch sym)
    (+ (match (regexp-match #rx"[A-G]" (symbol->string sym))
              ['("C") 0]
              ['("D") 2]
              ['("E") 4]
              ['("F") 5]
              ['("G") 7]
              ['("A") 9]
              ['("B") 11])
            (match (regexp-match #rx"b|#" (symbol->string sym))
              ;;TODO, cound number of b or # and multiply by +-1
              ['("b") -1]
              ['("#") 1]
              [else 0])
            (* (string->number (first (regexp-match #rx"[0-9]+" (symbol->string sym)))) 12)))
         

  ;;lookup symbol -> MSE
  (define (lookup name env)
    (local ([define (lookup-helper name env)
              (type-case Env env
                [mtEnv () (error 'lookup "free identifier ~a" name)]
                [anEnv (bound-name bound-value rest-env)
                       (if (symbol=? bound-name name)
                           bound-value
                           (lookup-helper name rest-env))])]
            [define (pitch-check name)
              (match (symbol->string name)
                [(regexp #rx"[A-G](#|b)*[0-9]+$") true]
                [else false])])
      (if (pitch-check name)
          (decode-pitch name) ;;name must be castable and pass regex to get here
          (lookup-helper name env))))


  ;;interp D-MSE -> MSE-Value
  (define (interp d-mse)
   (local [(define (transOne val m env)
            (type-case D-MSE m
              [i-note (p v d)  (i-note (i-num (+ (helper val env) (helper p env))) v d)]
              [else (transOne val
                              (type-case MSE-Value (helper m env)
                                [noteV (p v d) (i-note p v d)]
                                [else "need a note"]) env)]))
          (define (changeVol val m env)
            (type-case D-MSE m
              [i-note (p v d)  (i-note p val d)]
              [else (changeVol val
                              (type-case MSE-Value (helper m env)
                                [noteV (p v d) (i-note p v d)]
                                [else "need a note"]) env)]))
          (define (changePit val m env)
            (type-case D-MSE m
              [i-note (p v d)  (i-note val v d)]
              [else (changePit val
                              (type-case MSE-Value (helper m env)
                                [noteV (p v d) (i-note p v d)]
                                [else "need a note"]) env)]))
          (define (changeDur val m env)
            (type-case D-MSE m
              [i-note (p v d)  (i-note p v val)]
              [else (changeDur val
                              (type-case MSE-Value (helper m env)
                                [noteV (p v d) (i-note p v d)]
                                [else "need a note"]) env)]))
          (define (doInsert lis1 lis2 n)
            (cond [(> n (length lis1)) (append lis1 lis2)]
                  [(= n 0)(append lis1 lis2)]
                  [else
                   (cons (first lis1) (doInsert (rest lis1)lis2 (sub1 n)))]))
          (define (readIndex var env)
            (type-case D-MSE var
              [i-id (v) (type-case MSE-Value (lookup v env)
                          [seqV (l) (length l)]
                          [else (error "need a seqnV")])]
              [i-num (n) n]
              [else (error "need a num or an id")]))
          (define (tolist seq)
            (type-case D-MSE seq
              [i-sequence (l) l]
              [i-seqn-p (l) (map (lambda (sym) (i-note (i-num (interp sym))
                                               (i-num 10)  (i-num 10))) l)]
              [else (error "need a sequence")]))
          (define (tolist2 seq)
            (type-case MSE-Value seq
              [seqV (l) l]
              [else (error "need a seqV")]))
          (define (inter lis1 lis2)
            (cond [(empty? lis1) lis2]
                  [(empty? lis2) lis1]
                  [else (cons (first lis1)
                              (cons (first lis2)
                                    (inter (rest lis1) (rest lis2))))]))
          ;(define (markov lis 
          (define (helper expr env)
            (type-case D-MSE expr
              [i-num (n) n]
              [i-note (p v d) (noteV (pitchV (helper p env))
                                   (velV (helper v env) )
                                   (durV (helper d env)))]
              [i-id  (name)  (lookup name env)]
              [i-sequence (vals) (seqV (map (lambda (exp) (helper exp env)) vals))]
              [i-seqn-p (syms) (seqV (map (lambda (sym) (noteV (pitchV (helper sym env))
                                                           (velV  10)
                                                           (durV  10))) syms))]
              [i-fun (arg-name body) (closureV arg-name body env)]
              [i-app (fun-expr arg-expr)
                   (local ([define fun-val (helper fun-expr env)]
                           [define arg-val (helper arg-expr env)])
                     (helper (closureV-body fun-val)
                             (anEnv (closureV-param fun-val) arg-val (closureV-env fun-val))))]
              [i-interleave (l1 l2) (helper (i-sequence (inter (tolist l1) (tolist l2))) env)]
              [i-insert (l1 l2 index) (seqV (doInsert (tolist2  (helper l1 env) ) (tolist2  (helper l2 env)) (readIndex index env)))]
              [i-transpose (listN value)  (helper (i-sequence (map (lambda (m) (transOne value m env)) (tolist listN))) env) ]
              [changeProp (listN value pos) (cond [(= 1 (helper pos env)) (helper (i-sequence (map (lambda (m) (changePit value m env)) (tolist listN))) env)]
                                                [(= 2 (helper pos env)) (helper (i-sequence (map (lambda (m) (changeVol value m env)) (tolist listN))) env)]
                                                [(= 3 (helper pos env)) (helper (i-sequence (map (lambda (m) (transOne value m env)) (tolist listN))) env)])]
              [else "NO!!!"]))]
      (helper d-mse (mtEnv))))


  ;;Run MSE -> MSE-Value
  ;;Interprets the result of desugaring the parse s-expression
  (define (run mse)
    (interp (desugar (parse mse))))


  
