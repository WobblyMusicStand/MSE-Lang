#lang plai


;(print-only-errors)

(require "MSE-lang-design.rkt")


;;Testing for MSE lang

;;;;;;;;;;;;;;;;   define-type tests   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;   PARSER tests   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;TODO!

;;;;;;;;;;;;;;;;   DESUGARER tests   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sequence
(test (desugar (parse '{sequence {note 40 50 60}}))
      (sequence (list (note (num 40) (num 50) (num 60)))))
(test (desugar (parse '{sequence {note 40 50 60}
                                 {note 50 60 70}}))
      (sequence (list (note (num 40) (num 50) (num 60))
                      (note (num 50) (num 60) (num 70)))))
;; seq-append
(test (desugar (parse '{seq-append
                        {sequence {note 30 40 50}
                                  {note 20 30 40}}
                        {sequence {note 40 50 60}}}))
      (insert
       (sequence
         (list (note (num 30) (num 40) (num 50))
               (note (num 20) (num 30) (num 40))))
       (sequence (list (note (num 40) (num 50) (num 60))))
       (num 2)))

;; with
(test (desugar (parse '{with {c
                              {sequence {note 10 20 30}}}
                             c}))
      (app (fun 'c (id 'c))
           (sequence (list (note (num 10) (num 20) (num 30))))))
;; interleave
(test (desugar (parse '{interleave {sequence {note 30 40 50}}
                                   {sequence {note 40 50 60}}}))
      (interleave
       (sequence (list (note (num 30) (num 40) (num 50))))
       (sequence (list (note (num 40) (num 50) (num 60))))))
;; insert
(test (desugar (parse '{insert {sequence {note 20 30 40}}
                               {sequence {note 30 40 50}
                                         {note 40 50 60}
                                         {note 10 20 30}}
                               2}))
      (insert
       (sequence (list (note (num 20) (num 30) (num 40))))
       (sequence
         (list
          (note (num 30) (num 40) (num 50))
          (note (num 40) (num 50) (num 60))
          (note (num 10) (num 20) (num 30))))
       (num 2)))
;; transpose
(test (desugar (parse '{transpose {sequence {note 30 40 50}} 14}))
      (transpose (sequence (list (note (num 30) (num 40) (num 50)))) (num 14)))
;; markov
(test  (desugar (parse '{markov
                         {sequence {note 20 30 40}
                                   {note 30 40 50}
                                   {note 40 50 60}}
                         20
                         {sequence {note 20 30 40}}}))
       (markov
        (sequence
          (list
           (note (num 20) (num 30) (num 40))
           (note (num 30) (num 40) (num 50))
           (note (num 40) (num 50) (num 60))))
        (num 20)
        (sequence (list (note (num 20) (num 30) (num 40))))))






;;;;;;;;;;;;;;;;   INTERPRETER tests   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (run '10) (numV 10))

(test (run '{note 10 20 30})
      (noteV (pitchV 10) (velV  20) (durV 30)))


(test (run '{note C4 20 30})
      (noteV (pitchV 48) (velV  20) (durV 30)))

(test (run '{with {c 10} c}) (numV 10))

(test/exn (run '{with {C4 10} C4}) "") ;restricted pitch identifier
      
(test (run '{with {c {sequence {note 10 20 30}}}
                             c})
      (seqV (list (noteV (pitchV 10) (velV 20) (durV  30)))))

(test (run '(interleave (sequence (note 10 20 30) (note 20 20 20)) (sequence (note 30 20 10) (note 30 30 30))))
(seqV
 (list
  (noteV (pitchV 10) (velV 20) (durV 30))
  (noteV (pitchV 30) (velV 20) (durV 10))
  (noteV (pitchV 20) (velV 20) (durV 20))
  (noteV (pitchV 30) (velV 30) (durV 30)))))

(test (run '(interleave (sequence (note 10 20 30)) (sequence (note 30 20 10) (note 30 30 30))))
(seqV (list (noteV (pitchV 10) (velV 20) (durV 30))
            (noteV (pitchV 30) (velV 20) (durV 10))
            (noteV (pitchV 30) (velV 30) (durV 30)))))

(test (run '(seq-append (sequence (note 10 20 30) (note 20 20 20)) (sequence (note 30 20 10) (note 30 30 30))))
(seqV
 (list
  (noteV (pitchV 30) (velV 20) (durV 10))
  (noteV (pitchV 30) (velV 30) (durV 30))
  (noteV (pitchV 10) (velV 20) (durV 30))
  (noteV (pitchV 20) (velV 20) (durV 20)))))
