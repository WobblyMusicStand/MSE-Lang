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
;;     | {note <MSE> <num> <num>}
;;     | {sequence <MSE>*}
;;     | {seqn-p <MSE>*}                            ; A list of given notes in letter format (e.g. A4 E5)
;;     | {seqn-v <MSE>*}                            ; A list of notes' velocities
;;     | {seqn-d <MSE>*}                            ; A list of notes' durations
;;     | {seq-append <MSE> to <MSE>}                ; Appends the 2nd MSE Expression to
;;                                                    the end of the first
;;     | {with {<id> <MSE>} <MSE>}     
;;     | {fun {<id> <MSE>} <MSE>}
;;     | {<MSE> <MSE> }                             ; function calls, any time the first symbol is not a reserved word
;;     | {interleave <MSE> into <MSE>}              ; Takes first element of 1st MSE, appends to 1st element of 2nd MSE ... 
;;     | {insert <MSE> in <MSE> at <num>}           ; Inserts MSE in MSE at index number
;;     | {transpose <MSE> <num>}                    ; Takes a sequence of notes and a number, and
;;                                                    adds that number to the pitch of all notes in the sequence
;;     | {changePits <MSE> <num>}                   ; change pitches of all notes in the sequence to the given number
;;     | {changeVels <MSE> <num>}                   ; change velocities of all notes in the sequence to the given number
;;     | {changeDurs <MSE> <num>}                   ; change durations of all notes in the sequence to the given number
;;     | {zip <MSE> <MSE> <MSE>}                    ; creates a new sequence using the pitches of the 1st, velocity of the 2nd, and duration of the 3rd lists in order of occurance
;;     | {markov <MSE> <num> <MSE>?}                ; Runs markov chain (with a depth of 1) as shown in the example above

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
  [note (pitch MSE?) (vel MSE?) (dur MSE?)] ; Pitch, Velocity, Duration can be notes or IDs
  [sequence (values (listof MSE?))]                  ; Distributions must not be empty and strictly evaluate to a (listof Note)
  [seqn-p (values (listof MSE?))]
  [seqn-v (values (listof MSE?))]
  [seqn-d (values (listof MSE?))]
  [seq-append (list1 MSE?) (list2 MSE?)]              
  [with (id symbol?) (expr MSE?) (body MSE?)]
  [fun (param symbol?) (body MSE?)]
  [app (function MSE?) (arg MSE?)]
  [interleave (list1 MSE?) (list2 MSE?)]
  [insert (list1 MSE?) (list2 MSE?) (index MSE?)]
  [transpose (list1 MSE?) (add-val num?)]
  [changePits (list1 MSE?) (val num?)]
  [changeVels (list1 MSE?) (val num?)]
  [changeDurs (list1 MSE?) (val num?)]
  [zip (pList MSE?) (vList MSE?) (dList MSE?)]
  [markov (seed MSE?) (length MSE?) (initial-note MSE?)] ; initial-note has to evaluate to a note
  )

;; D-MSE, for interpreter
;; exclude seq-append and with
(define-type D-MSE
  [i-num (n number?)]
  [i-id (name symbol?)]
  [i-note (pitch D-MSE?) (vel D-MSE?) (dur D-MSE?)]
  [i-sequence (values (listof D-MSE?))]
  [i-seqn (prop symbol?) (values (listof D-MSE?))] ;prop field selects between pitch, vel, and dur targets
  [i-fun (param symbol?) (body D-MSE?)]
  [i-app (function D-MSE?) (arg D-MSE?)]
  [i-interleave (list1 D-MSE?) (list2 D-MSE?)]
  [i-insert (list1 D-MSE?)(list2 D-MSE?)(index D-MSE?)]
  [i-transpose (list1 D-MSE?)(add-val i-num?)]
  [changeProp (prop symbol?) (list1 D-MSE?) (val i-num?)]
  [i-zip (pList D-MSE?) (vList D-MSE?) (dList D-MSE?)]
  [i-markov (seed D-MSE?)(length D-MSE?)(initial-note D-MSE?)]
  )

;; Interpreting a value returns a Value
(define-type MSE-Value
  ; Sequence of  Notes
  [pitV (pit number?)]
  [velV (vel number?)]
  [durV (dur number?)]
  [noteV (pit pitV?) (vel velV?) (dur durV?)]
  [seqV (values (and/c (listof noteV?) (not/c empty?)))] ;no nested sequences
  [closureV (param symbol?)  ;;Closures wrap an unevaluated function body with its parameter and environment
            (body D-MSE?)
            (env Env?)])

;; Environments store values, instead of substitutions
(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value (or/c number? MSE-Value?)) (env Env?)])

;; ID's which cannot be used by the user.
;TODO, update list to include all functions
(define *reserved-symbols* '(note
                             sequence
                             seqn-p
                             seqn-v
                             seqn-d
                             seq-append
                             with
                             fun
                             interleave
                             insert
                             transpose
                             changePits
                             changeVels
                             changeDurs
                             zip
                             markov)) ; defining what the reserved symbols of the system are


;;;;;;;;;;;;;;;;   PARSER   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; valid-identifier? : symbol -> boolean
;; Returns whether the given input is a symbol and a valid identifier
(define (valid-id? sym)
  (and (symbol? sym)
       (if (and (match (symbol->string sym)
                  [(regexp #rx"[A-G](#|b)*[0-9]+$") (error "Illegal reserved PitchID in binding:" sym)]
                  [else true])
                (not (member sym *reserved-symbols*)))
           true
           (error "Illegal reserved ID in binding:" sym))))

;;parse s-exp -> MSE
;;Parses s-exp input and does valid-id checking for binding sites
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list 'note pitch vel dur) (note (parse pitch) (parse vel) (parse dur))]
    [(cons 'sequence notes) (sequence (map parse notes))]
    [(cons 'seqn-p pitches) (seqn-p  (map parse pitches))]
    [(cons 'seqn-v vels) (seqn-v  (map parse vels))]
    [(cons 'seqn-d durs) (seqn-d  (map parse durs))]
    [(list 'seq-append list1 list2) (seq-append (parse list1) (parse list2))]
    [(list 'with (list (? valid-id? id) value) body) (with id (parse value) (parse body))]
    [(list 'fun (? valid-id? param) body) (fun param (parse body))]
    [(list (and f-expr (? (lambda (s) (not (member s *reserved-symbols*))))) a-expr)   ;TODO Explicitly cause invalid-id error for fun and with?
     (app (parse f-expr) (parse a-expr))]
    [(list 'interleave list1 list2) (interleave (parse list1) (parse list2))]
    [(list 'interleave list1 into list2) (interleave (parse list1) (parse list2))] ;explicit version of interleave
    [(list 'insert list1 list2 index) (insert (parse list1) (parse list2) (parse index))]
    [(list 'insert list1 into list2 at index) (insert (parse list1) (parse list2) (parse index))] ;explicit version of insert
    [(list 'transpose list1 (? number? add-val)) (transpose (parse list1) (parse add-val))]
    [(list 'changePits list1 val) (changePits (parse list1) (parse val))]
    [(list 'changeVels list1 val) (changeVels (parse list1) (parse val))]
    [(list 'changeDurs list1 val) (changeDurs (parse list1) (parse val))]
    [(list 'zip pL vL dL) (zip (parse pL) (parse vL) (parse dL))]
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
    [seqn-p (vals) (i-seqn 'p (map desugar vals))]
    [seqn-v (vals) (i-seqn 'v (map desugar vals))]
    [seqn-d (vals) (i-seqn 'd (map desugar vals))]
    [seq-append (seq1 seq2) ;performs an insert with a negative index
                (i-insert (desugar seq1)
                          (desugar seq2)
                          (i-num -1))]     
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
    [changePits (list1 val) (changeProp 'p (desugar list1) (desugar val))]
    [changeVels (list1 val) (changeProp 'v (desugar list1) (desugar val))]
    [changeDurs (list1 val) (changeProp 'd (desugar list1) (desugar val))]
    [zip (pL vL dL) (i-zip  (desugar pL) (desugar vL) (desugar dL))]
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
     (+ (* -1 (length (indexes-of (regexp-split #rx" *" (symbol->string sym)) "b")))
        (length (indexes-of (regexp-split #rx" *" (symbol->string sym)) "#")))
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
            (type-case MSE-Value m
              [noteV (p v d)  (noteV (pitV (+ (helper val env) (type-case MSE-Value p
                                                                 [pitV (n) n]
                                                                 [else (error "Transpose requires a pitch, given: " p)]))) v d)]
              [else (transOne val
                              (type-case MSE-Value (helper m env)
                                [noteV (p v d) (noteV p v d)]
                                [else "need a note"]) env)]))
          ;changeProperty variants
          (define (changeVol val m env)
            (type-case MSE-Value m
              [noteV (p v d)  (noteV p (velV (helper val env)) d)]
              [else (changeVol val
                               (type-case MSE-Value (helper m env)
                                 [noteV (p v d) (noteV p v d)]
                                 [else "need a note"]) env)]))
          (define (changePit val m env)
            (type-case MSE-Value m
              [noteV (p v d)  (noteV (pitV (helper val env)) v d)]
              [else (changePit val
                               (type-case MSE-Value (helper m env)
                                 [noteV (p v d) (noteV p v d)]
                                 [else "need a note"]) env)]))
          (define (changeDur val m env)
            (type-case MSE-Value m
              [noteV (p v d)  (noteV p v (durV (helper val env)))]
              [else (changeDur val
                               (type-case MSE-Value (helper m env)
                                 [noteV (p v d) (noteV p v d)]
                                 [else "need a note"]) env)]))
          ;insert helper
          (define (doInsert lisS lisD n)
            (cond [(> n (length lisD)) (append lisD lisS)] ;if desination length is smaller then desired index append source to the end of dest
                  [(<= n 0)(append lisS lisD)] ;if index is <0, append destination to the end of source 
                  [else
                   (cons (first lisD) (doInsert lisS (rest lisD) (sub1 n)))])) ;else iterate over the elements in destination until index is reached
          
          (define (tolist seq)
            (type-case MSE-Value seq
              [seqV (l) l]
              [else (error "Expected a sequence given:" seq)])) ;TODO, replace with false, error handle at call site for more specific errors
          
          ;interleave helper
          (define (interl lis1 lis2)
            (cond [(empty? lis1) lis2]
                  [(empty? lis2) lis1]
                  [else (cons (first lis1)
                              (cons (first lis2)
                                    (interl (rest lis1) (rest lis2))))]))
          
          ;zip should call map on m elements of each list, where m is the length of the shortest list.
          ;zip helper
          (define (shorten pL vL dL) (local [(define-values (lpL lvL ldL) (values (length pL) (length vL) (length dL)))
                                             (define-values (shortestL) (if (< lpL lvL) ;pitch < vel?
                                                                            (if (< lpL ldL) ;pitch < dur?
                                                                                lpL ;then pitch
                                                                                ldL) ;else dur
                                                                            (if (< lvL ldL) ;vel < dur?
                                                                                lvL ;vel
                                                                                ldL)))] ;else dur
                                       (if (= lpL lvL ldL)
                                           (values pL vL dL)
                                           (values (take pL shortestL)
                                                   (take vL shortestL)
                                                   (take dL shortestL)))))
          
          (define (markovWithNote seed n init)
            (local [(define-values (seed-hash) (make-hash)) ;the mutable strong-keyed hash table

                    ;gets a pair at the current index, key is the current value, value is the next value (or an empty pair if current/next are out of bounds)
                    (define (getpair lst n) (cons (noteV-pit (list-ref lst n)) ;key, accesses noteV-pit field of key note
                                                  (if (>= (add1 n) (length lst)) ;value
                                                      (list (first lst)) ;last element implicitly follows first (option?)
                                                      (list (list-ref lst (add1 n))))))    
                    ;prepares a list of key-value pairs to be put into the seed-hash
                    (define (prepEntries lst) (local [(define (helper lst n)
                                                        (if (>= n (length lst))
                                                            empty
                                                            (local [(define-values (current-pair) (getpair lst n))]
                                                              (cons current-pair (helper lst (add1 n))))))]
                                                (helper lst 0)))
                    ;Fills the seed-hash with key-values pairs found in the seed list.
                    ;each pair is the key followed by a list of values, each values is found immediatly after the key value in the seed list.
                    (define (sethash entries) (for-each (lambda (k-v-pair)
                                                          (local [(define-values (current-vals) (hash-ref seed-hash (car k-v-pair) false))] ;if the current key has a value, append new value to old and re-set, otherwise create new entry
                                                            (if current-vals
                                                                (hash-set! seed-hash (car k-v-pair) (append current-vals (cdr k-v-pair)))
                                                                (hash-set! seed-hash (car k-v-pair) (cdr k-v-pair)))))
                                                        entries))
          
          
                    ;selects a values from the list of values stored with a key. will return false if the list does not exist, or is empty
                    (define (select lst) (if (and lst (positive? (length lst))) ;;add support for 0 length lists, DONE                                   
                                             (list-ref lst (random (length lst))) ;TODO, seed random to set values by default, desable with optional flag
                                             false))
                    ;selects the next id from the list of values for a particular key id
                    (define (getnext id) (select (hash-ref seed-hash id false)))          
                    ;recurssively builds a list of values using an initial key found in the seed-hash and returns it
                    (define (buildlist n id) (local [(define-values (next) (getnext (noteV-pit id)))]
                                               (if (and next (> n 0))
                                                   (cons next
                                                         (buildlist (sub1 n) next))
                                                   empty)))] ;;not (empty) because that is a procedure application.
    
              ;Create the hash table for the markov chain
              (sethash (prepEntries seed))
    
              ;return the seed-hash and the resulting list
              (buildlist n init)))

          ;inter helper, the BBEG itself
          (define (helper expr env)
            (type-case D-MSE expr
              [i-num (n) n]
              [i-note (p v d) (local [(define-values (pit) (helper p env))
                                      (define-values (vel) (helper v env))
                                      (define-values (dur) (helper d env))]
                                (if (and (number? pit) (number? vel) (number? dur))
                                    (noteV (pitV pit)
                                           (velV vel)
                                           (durV dur))
                                    (error "note requires numeric pit, vel, dur; given:" pit vel dur)))]
              [i-id  (name)  (lookup name env)]
              [i-sequence (vals) (seqV (map (lambda (exp) (local [(define-values (pnote) (helper exp env))]
                                                            (if (noteV? pnote)
                                                                pnote
                                                                (error "Sequence requires note, given:" pnote)))) vals))]
              ;prop selects the field to bind the values into
              [i-seqn (prop syms) (cond [(eq? prop 'p) (seqV (map (lambda (sym) (noteV (pitV (helper sym env))
                                                                                       (velV (helper (i-id 'DVEL) env))
                                                                                       (durV (helper (i-id 'DDUR) env)))) syms))]
                                        [(eq? prop 'v) (seqV (map (lambda (sym) (noteV (pitV (helper (i-id 'DPIT) env))
                                                                                       (velV (helper sym env))
                                                                                       (durV (helper (i-id 'DDUR) env)))) syms))]
                                        [(eq? prop 'd) (seqV (map (lambda (sym) (noteV (pitV (helper (i-id 'DPIT) env))
                                                                                       (velV (helper (i-id 'DVEL) env))
                                                                                       (durV (helper sym env)))) syms))])]
              [i-fun (arg-name body) (closureV arg-name body env)]
              [i-app (fun-expr arg-expr)
                     (local ([define fun-val (helper fun-expr env)]
                             [define arg-val (helper arg-expr env)])
                       (helper (closureV-body fun-val)
                               (anEnv (closureV-param fun-val) arg-val (closureV-env fun-val))))]
              [i-interleave (l1 l2) (seqV (interl (tolist (helper l1 env)) (tolist (helper l2 env))))]
              [i-insert (l1 l2 index) (seqV (doInsert (tolist  (helper l1 env) ) (tolist  (helper l2 env)) (helper index env)))]
              [i-transpose (listN value)  (seqV (map (lambda (m) (transOne value m env)) (tolist (helper listN env)))) ]
              [changeProp (prop listN value) (cond [(eq? prop 'p) (seqV (map (lambda (m) (changePit value m env)) (tolist (helper listN env))))]
                                                   [(eq? prop 'v) (seqV (map (lambda (m) (changeVol value m env)) (tolist (helper listN env))))]
                                                   [(eq? prop 'd) (seqV (map (lambda (m) (changeDur value m env)) (tolist (helper listN env))))])]
              [i-zip (pL vL dL) (local [(define-values (spL svL sdL) ;Get the first m elements of each list, where m is the length of the shortest list
                                          (shorten (tolist (helper pL env))
                                                   (tolist (helper vL env))
                                                   (tolist (helper dL env))))]
                                  (seqV (map (lambda (p v d)
                                               (noteV (noteV-pit p) ;pitches from the 1st list
                                                      (noteV-vel v) ;vels from the 2nd
                                                      (noteV-dur d))) ;durs from the 3rd
                                             spL svL sdL)))]
              [i-markov (seed length inote)(local [(define seedlist (tolist (helper seed env)))
                                                   (define init (noteV (pitV (helper inote env))
                                                                       (velV (helper (i-id 'DVEL) env))
                                                                       (durV (helper (i-id 'DVEL) env))))                                                                             
                                                   (define n (helper length env))]
                                             (if (and seedlist init (number? n))
                                                 (local [(define result (markovWithNote seedlist n init))]
                                                   (if (empty? result)
                                                       (error "Bad starting note in markov, no mappings" inote)
                                                       (seqV result)))
                                                 (error "Malformed markov with length:" n)))]
              ;return a list of notes, each note is a note from seed.
              ;the result is length notes long and starts at inote.
              
              ;[else "NO!!!"]
              ))]
    ;Trampoline into interp, set the defualt Pitch, Velocity and Duration to O (unless overridden)
    (helper d-mse (anEnv 'DPIT 0 (anEnv 'DVEL 0 (anEnv 'DDUR 0 (mtEnv)))))))


;;Run MSE -> MSE-Value
;;Interprets the result of desugaring the parse s-expression
(define (run mse)
  (interp (desugar (parse mse))))


  
