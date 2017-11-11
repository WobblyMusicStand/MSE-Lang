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




(define-type MSE
  [num (n number?)]
  [id (name symbol?)]
  [note (pitch num?) (vel num?) (dur num?)] ; Pitch, Velocity, Duration
  [sequence (values (listof note?))]                  ; Distributions must not be empty and strictly evaluate to a (listof Note)
  [seq-append (list1 MSE?) (list2 MSE?)]              
  [with (id symbol?) (expr MSE?) (body MSE?)]
  [fun (param symbol?) (body MSE?)]
  [app (function MSE?) (arg MSE?)]
  [interleave (list1 MSE?) (list2 MSE?)]
  [insert (list1 MSE?) (list2 MSE?) (index num?)]
  [transpose (list1 MSE?) (add-val num?)]
  [markov (seed MSE?) (length num?) (initial-note MSE?)] ; initial-note has to evaluate to a note
  )


(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list 'note pitch vel dur) (note (parse pitch) (parse vel) (parse dur))]
    [(cons 'sequence notes) (sequence (map parse notes))]
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
    [seq-append (seq1 seq2)
                (insert (desugar seq1)
                        (desugar seq2)
                        (num (length (sequence-values (desugar seq1)))))] 
    [with (id named-expr body) (desugar (app (fun id body) named-expr))]
    [fun (param body) (fun param (desugar body))]
    [app (fn-exp arg-exp) (app (desugar fn-exp)
                               (desugar arg-exp))]
    [else "NO!!!"]))

