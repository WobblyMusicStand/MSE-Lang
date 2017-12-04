#lang plai



(printf "hash testing: \n")
(hash 1 1 2 (list 2 2 2))
(hash)
(make-hash (list (cons 1 2)))
(make-hash '())


(printf "hash-set! testing: \n")
(define testhash (make-hash))
testhash
(hash-set! testhash 'a 1)
testhash
(for-each (lambda (cp) (hash-set! testhash (car cp) (cdr cp))) (list (cons 'a 1) (cons 'b 2) )) ;discards result, maintain only side-effects
testhash

;hash-ref
(printf "hash-ref testing: \n")
(hash-ref (hash 1 2 1 1) 1 0)
(hash-ref (hash) 1 0)
(hash-ref (make-hash (list (cons 1 2) (cons 1 1))) 1 0)
(hash-ref (make-hash) 1 0)

;map hash-ref
(map (lambda (id) (hash-ref (hash 'a 1 'b 2 'c 3 'd 4) id false)) (list 'a 'b 'c 'd))

(printf "list building testing: \n")
;build list, recursively hash-ref results of hash-ref
(local [(define (getnext id) (hash-ref (hash 'a 'b 'b 'c 'c 'd 'd 4) id false))
        (define (buildlist id n) (if (> n 0)
                                     (cons (getnext id)
                                           (if (and (getnext id) (> n 1))
                                               (buildlist (getnext id) (sub1 n))
                                               empty))
                                     empty))] ;;not (empty) because that is a procedure application.
  (buildlist 'a 1))


;access elemenets of lists returned by hash-ref
(local [(define (getnext id) (first (hash-ref (hash 'a (list 'b 'c) 'b (list 'c) 'c (list 'd 4) 'd (list 4)) id empty)))
        (define (buildlist id n) (if (> n 0)
                                     (cons (getnext id)
                                           (if (and (getnext id) (> n 1))
                                               (buildlist (getnext id) (sub1 n))
                                               empty))
                                     empty))] ;;not (empty) because that is a procedure application.
  (buildlist 'a 2))

;hash-ref once per cycle.
(local [(define (getnext id) (first (hash-ref (hash 'a (list 'b 'c) 'b (list 'c) 'c (list 'd 4) 'd (list 'a 4)) id empty)))
        (define (buildlist id n) (if (> n 0)
                                     (local [(define-values (next) (getnext id))]
                                       (cons next
                                             (if (and next (> n 1))
                                                 (buildlist next (sub1 n))
                                                 empty)))
                                     empty))] ;;not (empty) because that is a procedure application.
  (buildlist 'a 10))

;hash-ref once per cycle, select last element from list to use, falses are turned into empty elements (base case)
(local [(define (select lst) (if lst
                                 (list-ref lst (sub1 (length lst)))
                                 false))
        (define (getnext id) (select (hash-ref (hash 'a (list 'b 'c) 'b (list 'c) 'c (list 'd 4) 'd (list 'a 4)) id false)))
        (define (buildlist id n) (local [(define-values (next) (getnext id))]
                                   (if (and next (> n 0))
                                       (cons next
                                             (buildlist next (sub1 n))
                                             #;(if (> n 1) ;redundant
                                                   (buildlist next (sub1 n))
                                                   empty))
                                       empty)))] ;;not (empty) because that is a procedure application.
  (buildlist 'a 3))

;hash-ref once per cycle, select random element from list to use, falses are turned into empty elements (base case)
;;Markov number symbol -> listof (or/c number? symbol?)
(define (m1 n init)
  (local [(define (select lst) (if lst
                                   (list-ref lst (random (length lst)))
                                   false))
          (define (getnext id) (select (hash-ref (hash 'a (list 'a 'c) 'b (list 'c) 'c (list 'd 4) 'd (list 'a 4)) id false)))
          (define (buildlist n id) (local [(define-values (next) (getnext id))]
                                     (if (and next (> n 0))
                                         (cons next
                                               (buildlist (sub1 n) next))
                                         empty)))] ;;not (empty) because that is a procedure application.
    (buildlist n init)))

(printf "m1 testing: \n")
(m1 5 'a)
(m1 5 'a)
(m1 5 'a)
(m1 5 'a)


;hash-ref once per cycle, select random element from list to use, falses are turned into empty elements (base case)
;;Markov number symbol -> listof (or/c number? symbol?)
(define (Markov1 seed n init)
  (local [(define (select lst) (if (and lst (positive? (length lst))) ;;add support for 0 length lists, DONE                                   
                                   (list-ref lst (random (length lst)))
                                   false))
          (define (getnext id) (select (hash-ref seed id false)))
          (define (buildlist n id) (local [(define-values (next) (getnext id))]
                                     (if (and next (> n 0))
                                         (cons next
                                               (buildlist (sub1 n) next))
                                         empty)))] ;;not (empty) because that is a procedure application.
    (buildlist n init)))

(printf "Markov with seed \n")
(Markov1 (hash 'a (list 'a 'c) 'b (list 'c) 'c (list 'd 4) 'd (list 'a 4)) 5 'a)
(Markov1 (hash 'a (list 'a 'c) 'b (list 'c) 'c (list 'd 4) 'd (list 'a 4)) 5 'a)
(Markov1 (hash 'a (list 'a 'c) 'b (list 'c) 'c (list 'd 4) 'd (list 'a 4)) 5 'a)
(Markov1 (hash 'a (list 'a 'c) 'b (list 'c) 'c (list 'd 4) 'd (list 'a 4)) 5 'a)

(Markov1 (hash 'a (list 'b 'c 'd 'e) 'b (list 'a) 'c (list 'a) 'd (list 'a) 'e (list 'a)) 20 'a)

(Markov1 (hash 'a (list 'a 'c) 'b (list 'c) 'c (list 'd 4) 'd (list)) 5 'a)


;hash-ref once per cycle, select random element from list to use, falses are turned into empty elements (base case)
;;Markov number symbol -> listof (or/c number? symbol?)
;buildhash creates a hash table and for each element in seed, makes it's entry the succedding element
(define (Markov2 seed n init)
  (local [(define (getpair lst n) (cons (list-ref lst n)
                                        (if (>= (add1 n) (length lst))
                                            empty
                                            (list (list-ref lst (add1 n)))))) ;return the current and next elements in the list, provided the list is not empty
          (define (loadhash lst n) (if (>= n (length lst))
                                       empty
                                       (local [(define-values (current-pair) (getpair lst n))]
                                         (cons current-pair (loadhash lst (add1 n))))))
          (define-values (hashtable) (make-hash (loadhash seed 0)))

          (define (select lst) (if (and lst (positive? (length lst))) ;;add support for 0 length lists, DONE                                   
                                   (list-ref lst (random (length lst)))
                                   false))
          (define (getnext id) (select (hash-ref hashtable id false)))
          (define (buildlist n id) (local [(define-values (next) (getnext id))]
                                     (if (and next (> n 0))
                                         (cons next
                                               (buildlist (sub1 n) next))
                                         empty)))] ;;not (empty) because that is a procedure application.
    (values hashtable
            (buildlist n init))))

(printf "Markov2 with build hash \n")
(Markov2 (list 'a 'b 'a 'c 'b) 5 'a)
(Markov2 (list) 5 'a)


;;Markov number symbol -> listof (or/c number? symbol?) 
;buildhash creates a hash table and for each element in seed, update it's entry the succedding element
;uses hash-set! to place entries into a hash where the new entrees for a key are the exisiting list appended to the newly found value
(define (Markov3 seed n init)
  (local [(define-values (seed-hash) (make-hash))

          ;gets a pair at the current index, key is the current value, value is the next value (or an empty pair if current/next are out of bounds)
          (define (getpair lst n) (cons (list-ref lst n)
                                        (if (>= (add1 n) (length lst))
                                            empty
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
                                   (list-ref lst (random (length lst)))
                                   false))
          ;selects the next id from the list of values for a particular key id
          (define (getnext id) (select (hash-ref seed-hash id false)))          
          ;recurssively builds a list of values using an initial key found in the seed-hash and returns it
          (define (buildlist n id) (local [(define-values (next) (getnext id))]
                                     (if (and next (> n 0))
                                         (cons next
                                               (buildlist (sub1 n) next))
                                         empty)))] ;;not (empty) because that is a procedure application.
    
    (sethash (prepEntries seed))
    
    ;return the seed-hash and the resulting list
    (values seed-hash
            (buildlist n init))))

(printf "Markov3 with build hash and all values following keys in hash list\n")
(Markov3 (list 'a 'b 'a 'c 'b) 5 'a)
(Markov3 (list) 5 'a)
(Markov3 (list 'a 'b 'a 'c 'b) 10 'a)
(Markov3 (list 'a 'b 'a 'c 'b 'a 'b 'a 'a 'a) 10 'a)

;almost complete implementation, once finished. convert keys to the pitV from a note, and entries to lists of noteVs

(define-type MSE-Value
  ; Sequence of  Notes
  [pitV (pit number?)]
  [velV (vel number?)]
  [durV (dur number?)]
  [noteV (pit pitV?) (vel velV?) (dur durV?)])
  

;;MarkovWithNote listof (noteV) number noteV -> listof (or/c noteV) 
;buildhash creates a hash table and for each element in seed, update it's entry the succedding element
;uses hash-set! to place entries into a hash where the new entrees for a key are the exisiting list appended to the newly found value
(define (MarkovWithNote seed n init)
  (local [(define-values (seed-hash) (make-hash))

          ;gets a pair at the current index, key is the current value, value is the next value (or an empty pair if current/next are out of bounds)
          (define (getpair lst n) (cons (noteV-pit (list-ref lst n)) ;key, accesses noteV-pit field of key note
                                        (if (>= (add1 n) (length lst)) ;value
                                            empty
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
                                   (list-ref lst (random (length lst)))
                                   false))
          ;selects the next id from the list of values for a particular key id
          (define (getnext id) (select (hash-ref seed-hash id false)))          
          ;recurssively builds a list of values using an initial key found in the seed-hash and returns it
          (define (buildlist n id) (local [(define-values (next) (getnext (noteV-pit id)))]
                                     (if (and next (> n 0))
                                         (cons next
                                               (buildlist (sub1 n) next))
                                         empty)))] ;;not (empty) because that is a procedure application.
    
    (sethash (prepEntries seed))
    
    ;return the seed-hash and the resulting list
   (values seed-hash
            (buildlist n init))))

(printf "MarkovWithNote\n")
(MarkovWithNote (list (noteV (pitV 10) (velV 10) (durV 10))
                      (noteV (pitV 10) (velV 20) (durV 10))
                      (noteV (pitV 10) (velV 30) (durV 10))) 1 (noteV (pitV 10) (velV 10) (durV 10)))

;;MarkovWithNote listof (noteV) number noteV -> listof (or/c noteV) 
;buildhash creates a hash table and for each element in seed, update it's entry the succedding element
;uses hash-set! to place entries into a hash where the new entrees for a key are the exisiting list appended to the newly found value
(define (MarkovRapAroundWithNote seed n init)
  (local [(define-values (seed-hash) (make-hash))

          ;gets a pair at the current index, key is the current value, value is the next value (or an empty pair if current/next are out of bounds)
          (define (getpair lst n) (cons (noteV-pit (list-ref lst n)) ;key, accesses noteV-pit field of key note
                                        (if (>= (add1 n) (length lst)) ;value
                                            (list (first lst))
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
                                   (list-ref lst (random (length lst)))
                                   false))
          ;selects the next id from the list of values for a particular key id
          (define (getnext id) (select (hash-ref seed-hash id false)))          
          ;recurssively builds a list of values using an initial key found in the seed-hash and returns it
          (define (buildlist n id) (local [(define-values (next) (getnext (noteV-pit id)))]
                                     (if (and next (> n 0))
                                         (cons next
                                               (buildlist (sub1 n) next))
                                         empty)))] ;;not (empty) because that is a procedure application.
    
    (sethash (prepEntries seed))
    
    ;return the seed-hash and the resulting list
   (values seed-hash
            (buildlist n init))))

(printf "MarkovRapAroundWithNote\n")
(MarkovRapAroundWithNote (list (noteV (pitV 10) (velV 10) (durV 10))
                      (noteV (pitV 10) (velV 20) (durV 10))
                      (noteV (pitV 10) (velV 30) (durV 10))) 1 (noteV (pitV 10) (velV 10) (durV 10)))
