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



;; for desugar
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
  [markov (seed MSE?) (length num?) (initial-note MSE?)] ; initial-note has to evaluate to a note
  )

;; for interpreter
;; exclude seq-append and with
(define-type D-MSE
  [i-num (n number?)]
  [i-id (name symbol?)]
  [i-note (pitch num?) (vel num?) (dur num?)]
  [i-sequence (values (listof note?))]
  [i-fun (param symbol?) (body D-MSE?)]
  [i-app (function D-MSE?) (arg D-MSE?)]
  [i-interleave (list1 D-MSE?) (list2 D-MSE?)]
  [i-insert (list1 D-MSE?)(list2 D-MSE?)(index num?)]
  [i-transpose (list1 D-MSE?)(add-val num?)]
  [i-markov (seed D-MSE?)(length num?)(initial-note D-MSE?)]
  )

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
    [(list (and f-expr (? (lambda (s) (not (member s *reserved-symbols*))))) a-expr)
     (app (parse f-expr) (parse a-expr))]
    [(list 'interleave list1 list2) (interleave (parse list1) (parse list2))]
    [(list 'insert list1 list2 index) (insert (parse list1) (parse list2) (parse index))]
    [(list 'transpose list1 add-val) (transpose (parse list1) (parse add-val))]
    [(list 'markov seed length initial-note) (markov (parse seed) (parse length) (parse initial-note))]))


(define *reserved-symbols* '(note sequence seq-append with interleave insert transpose markov)) ; defining what the reserved symbols of the system are

;; valid-identifier? : symbol -> boolean
;; Returns whether the given input is a symbol and a valid identifier
(define (valid-id? sym)
  (and (symbol? sym)
       (not (member sym *reserved-symbols*))))

;; Environments store values, instead of substitutions
(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value MSE-Value?) (env Env?)])

;; Interpreting a value returns a Value
(define-type MSE-Value
  ; Sequence of  Notes
  [pitchV (pit num?)]
  [velV (vel num?)]
  [durV (dur num?)]
  [noteV (p pitchV?) (vel velV?) (dur durV?)]
  [seqV (values (and/c (listof MSE-Value?) (not/c empty?)))] ;;Distributions must not be empty
  [closureV (param symbol?)  ;;Closures wrap an unevaluated function body with its parameter and environment
            (body MSE?)
            (env Env?)])

(define (desugar p-mse)
  (type-case MSE p-mse
    [num (n) (num n)]
    [id (val) (id val)]
    [note (p v d) (note (desugar p)
                        (desugar v)
                        (desugar d))]
    [sequence (vals) (sequence (map desugar vals))]
    [seqn-p (vals) (seqn-p (map desugar vals))]
    [seq-append (seq1 seq2)
                (insert (desugar seq1)
                        (desugar seq2)
                        (num (length (sequence-values (desugar seq1)))))] 
    [with (id named-expr body) (desugar (app (fun id body) named-expr))]
    [fun (param body) (fun param (desugar body))]
    [app (fn-exp arg-exp) (app (desugar fn-exp)
                               (desugar arg-exp))]
    [interleave (lst1 lst2)
                (interleave (desugar lst1)(desugar lst2))]
    [insert (lst1 lst2 index)
            (insert (desugar lst1)(desugar lst2)(desugar index))]
    [transpose (lst1 anum)
               (transpose (desugar lst1)(desugar anum))]
    [markov (s lth ini)
            (markov (desugar s)(desugar lth)(desugar ini))]
    ))


        
(define (decode-pitch sym)
  (match sym
    ;[(? number?) (num sym)]
    [(? symbol?) (match (symbol->string sym)
                   [(regexp #rx"[A-G](#|b)*[0-9]+$")(num (+ (match (regexp-match #rx"[A-G]" (symbol->string sym))
                                                        ['("C") 0]
                                                        ['("D") 2]
                                                        ['("E") 4]
                                                        ['("F") 5]
                                                        ['("G") 7]
                                                        ['("A") 9]
                                                        ['("B") 11])
                                                      (match (regexp-match #rx"b|#" (symbol->string sym))
                                                        ['("b") -1]
                                                        ['("#") 1]
                                                        [else 0])
                                                      (* (string->number (first (regexp-match #rx"[0-9]+" (symbol->string sym)))) 12)))]
                    [else (error "Not a valid pitch: " sym)])]
    [else (error "Not a valid pitch: " sym)]
    ))
        
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
        (decode-pitch name) 
        (lookup-helper name env))))

(define (interp d-mse)
  (local [(define (transOne val m env)
            (type-case MSE m
              [note (p v d)  (note (num (+ (helper val env) (helper p env))) v d)]
              [else (transOne val
                              (type-case MSE-Value (helper m env)
                                [noteV (p v d) (note p v d)]
                                [else "need a note"]) env)]))
          (define (doInsert lis2 lis n)
            (cond [(> n (length lis)) (append lis lis2)]
                  [(= n 0)(append lis2 lis)]
                  [else
                   (cons (first lis) (doInsert lis2 (rest lis) (sub1 n)))]))
          (define (tolist seq)
            (type-case MSE seq
              [sequence (l) l]
              [else (error "need a sequence")]))
          (define (inter lis1 lis2)
            (cond [(empty? lis1) lis2]
                  [(empty? lis2) lis1]
                  [else (cons (first lis1)
                              (cons (first lis2)
                                    (inter (rest lis1) (rest lis2))))]))
          ;(define (markov lis 
          (define (helper expr env)
            (type-case MSE expr
              [num (n) n]
              [note (p v d) (noteV (pitchV p)
                                   (velV v)
                                   (durV d))]
              [id  (name)  (lookup name env)]
              [sequence (vals) (seqV (map (lambda (exp) (helper exp env)) vals))]
              [seqn-p (syms) (seqV (map (lambda (sym) (noteV (pitchV (helper sym env))
                                                           (velV (num 10))
                                                           (durV (num 10)))) syms))]
              [fun (arg-name body) (closureV arg-name body env)]
              [app (fun-expr arg-expr)
                   (local ([define fun-val (helper fun-expr env)]
                           [define arg-val (helper arg-expr env)])
                     (helper (closureV-body fun-val)
                             (anEnv (closureV-param fun-val) arg-val (closureV-env fun-val))))]
              [interleave (l1 l2) (helper (sequence (inter (tolist l1) (tolist l2))) env)]
              [insert (l1 l2 index) (helper (sequence (doInsert (tolist  l1 ) (tolist  l2) (helper index env))) env)]
              [transpose (listN value)  (helper (sequence (map (lambda (m) (transOne value m env)) (tolist listN))) env) ]
              [else "NO!!!"]))]
    (helper d-mse (mtEnv))))

(define (run mse)
  (interp (desugar (parse mse))))

