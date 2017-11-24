#lang plai


(print-only-errors)

(require "MSE-lang-design.rkt")


;;Testing for MSE lang

;;;;;;;;;;;;;;;;   define-type tests   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;   PARSER tests   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (parse '1) (num 1))
(test (parse 'c) (id 'c))

;restricted symbols
;references
(test (parse 'note) (id 'note))
(test (parse 'C4) (id 'C4))
(test (parse 'Cb4) (id 'Cb4))
(test (parse 'C#4) (id 'C#4))
(test (parse 'C100) (id 'C100))


;binding:
(test/exn (parse '{with {note 1} 1}) "")
(test/exn (parse '{with {sequence 1} 1}) "")
(test/exn (parse '{with {seqn-p 1} 1}) "")
(test/exn (parse '{with {seq-append 1} 1}) "")
(test/exn (parse '{with {with 1} 1}) "")
(test/exn (parse '{with {fun 1} 1}) "")
(test/exn (parse '{with {interleave 1} 1}) "")
(test/exn (parse '{with {insert 1} 1}) "")
(test/exn (parse '{with {transpose 1} 1}) "")
(test/exn (parse '{with {changeVelocity 1} 1}) "")
(test/exn (parse '{with {markov 1} 1}) "")
;pitches
(test/exn (parse '{with {A4 1} 1}) "")
(test/exn (parse '{with {B4 1} 1}) "")
(test/exn (parse '{with {C4 1} 1}) "")
(test/exn (parse '{with {D4 1} 1}) "")
(test/exn (parse '{with {E4 1} 1}) "")
(test/exn (parse '{with {F4 1} 1}) "")
(test/exn (parse '{with {G4 1} 1}) "")
(test/exn (parse '{with {Cb4 1} 1}) "")
(test/exn (parse '{with {C#4 1} 1}) "")
(test/exn (parse '{with {Cbb4 1} 1}) "")
(test/exn (parse '{with {C##4 1} 1}) "")
(test/exn (parse '{with {C10000 1} 1}) "")
(test (parse '{with {Dg4 1} 1}) (with 'Dg4 (num 1) (num 1))) ;go right ahead with your malformed ids


;;;;;;;;;;;;;;;;   DESUGARER tests   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;ID testing
;TODO, append cannot be desugared to support ids?
;or, not desugared when first arg is ID?
(test (desugar (parse '{with {c {seqn-p C4 C5 C6}}
                             {seq-append c c}}))
      "")


;; sequence
(test (desugar (parse '{sequence {note 40 50 60}}))
      (i-sequence (list (i-note (i-num 40) (i-num 50) (i-num 60)))))
(test (desugar (parse '{sequence {note 40 50 60}
                                 {note 50 60 70}}))
      (i-sequence (list (i-note (i-num 40) (i-num 50) (i-num 60))
                        (i-note (i-num 50) (i-num 60) (i-num 70)))))
;; seqn-p
;; happy birthday
(test (desugar (parse '(seqn-p A4 A4 B4 A4 D5 C#5 A4 A4 B4 A4 E5 D5 A4 A4 A5 F#5 D5 C#5 B4 G5 G5 F#5 D5 E5 D5 )))
      (i-seqn-p
       (list
        (i-id 'A4)
        (i-id 'A4)
        (i-id 'B4)
        (i-id 'A4)
        (i-id 'D5)
        (i-id 'C#5)
        (i-id 'A4)
        (i-id 'A4)
        (i-id 'B4)
        (i-id 'A4)
        (i-id 'E5)
        (i-id 'D5)
        (i-id 'A4)
        (i-id 'A4)
        (i-id 'A5)
        (i-id 'F#5)
        (i-id 'D5)
        (i-id 'C#5)
        (i-id 'B4)
        (i-id 'G5)
        (i-id 'G5)
        (i-id 'F#5)
        (i-id 'D5)
        (i-id 'E5)
        (i-id 'D5))))

;; seq-append
(test (desugar (parse '{seq-append
                        {sequence {note 30 40 50}
                                  {note 20 30 40}}
                        {sequence {note 40 50 60}}}))
      (i-insert
       (i-sequence (list (i-note (i-num 30) (i-num 40) (i-num 50)) (i-note (i-num 20) (i-num 30) (i-num 40))))
       (i-sequence (list (i-note (i-num 40) (i-num 50) (i-num 60))))
       (i-num 2)))
(test (desugar (parse '{seq-append {seqn-p C4 C4} {seqn-p G4 G4 A4 A4 G4}}))
      (i-insert (i-seqn-p (list (i-id 'C4) (i-id 'C4)))
                (i-seqn-p (list (i-id 'G4) (i-id 'G4) (i-id 'A4) (i-id 'A4) (i-id 'G4)))
                (i-num 2)))

;; with
(test (desugar (parse '{with {c
                              {sequence {note 10 20 30}}}
                             c}))
      (i-app (i-fun 'c (i-id 'c)) (i-sequence (list (i-note (i-num 10) (i-num 20) (i-num 30))))))
;; interleave
(test (desugar (parse '{interleave {sequence {note 30 40 50}}
                                   {sequence {note 40 50 60}}}))
      (i-interleave (i-sequence (list (i-note (i-num 30) (i-num 40) (i-num 50))))
                    (i-sequence (list (i-note (i-num 40) (i-num 50) (i-num 60))))))
(test (desugar (parse '{interleave {seqn-p A4}{seqn-p A4}}))
      (i-interleave (i-seqn-p (list (i-id 'A4))) (i-seqn-p (list (i-id 'A4)))))
;; insert
(test (desugar (parse '{insert {sequence {note 20 30 40}}
                               {sequence {note 30 40 50}
                                         {note 40 50 60}
                                         {note 10 20 30}}
                               2}))
      (i-insert
       (i-sequence (list (i-note (i-num 20) (i-num 30) (i-num 40))))
       (i-sequence (list (i-note (i-num 30) (i-num 40) (i-num 50))
                         (i-note (i-num 40) (i-num 50) (i-num 60))
                         (i-note (i-num 10) (i-num 20) (i-num 30))))
       (i-num 2)))
(test (desugar (parse '{insert {seqn-p A4 A4} {seqn-p B4 B4} 1}))
      (i-insert (i-seqn-p (list (i-id 'A4) (i-id 'A4)))
                (i-seqn-p (list (i-id 'B4) (i-id 'B4)))
                (i-num 1)))
;; transpose
(test (desugar (parse '{transpose {sequence {note 30 40 50}} 14}))
      (i-transpose (i-sequence (list (i-note (i-num 30) (i-num 40) (i-num 50)))) (i-num 14)))
(test (desugar (parse '{transpose {seqn-p A4} 23}))
      (i-transpose (i-seqn-p (list (i-id 'A4))) (i-num 23)))
;; changeVelocity
(test (desugar (parse '{changeVelocity {sequence {note 10 20 30}} 40}))
      (changeProp (i-sequence (list (i-note (i-num 10) (i-num 20) (i-num 30)))) (i-num 40) (i-num 2)))
(test (desugar (parse '{changeVelocity {seqn-p C4 C4 G4 G4} 7}))
      (changeProp (i-seqn-p (list (i-id 'C4) (i-id 'C4) (i-id 'G4) (i-id 'G4))) (i-num 7) (i-num 2)))
;; markov
(test  (desugar (parse '{markov
                         {sequence {note 20 30 40}
                                   {note 30 40 50}
                                   {note 40 50 60}}
                         20
                         {sequence {note 20 30 40}}}))
       (i-markov
        (i-sequence (list (i-note (i-num 20) (i-num 30) (i-num 40))
                          (i-note (i-num 30) (i-num 40) (i-num 50))
                          (i-note (i-num 40) (i-num 50) (i-num 60))))
        (i-num 20)
        (i-sequence (list (i-note (i-num 20) (i-num 30) (i-num 40))))))
(test (desugar (parse '{markov {seqn-p C4 D4 E4 F4 G4 A4 B4} 10 {seqn-p E4}}))
      (i-markov (i-seqn-p (list (i-id 'C4) (i-id 'D4) (i-id 'E4) (i-id 'F4) (i-id 'G4) (i-id 'A4) (i-id 'B4)))
                (i-num 10)
                (i-seqn-p (list (i-id 'E4)))))



;;;;;;;;;;;;;;;;   INTERPRETER tests   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test pitch-ids
(test (run 'A3) 45)
(test (run 'B3) 47)
(test (run 'Cb4) 47)
(test (run 'B#3) 48)
(test (run 'C4) 48)
(test (run 'C#4) 49)
(test (run 'D4) 50)
(test (run 'E4) 52)
(test (run 'F4) 53)
(test (run 'G4) 55)
(test (run 'A4) 57)
(test (run 'B4) 59)
(test (run 'C5) 60)

(test (run 'C0) 0)
(test (run 'C10) 120)

(test (run 'Cbb4) 46)
(test (run 'C##4) 50)
(test (run 'Cb#4) 48)
(test (run 'C#b4) 48)

(test/exn (run 'B-1) "") ;out of range
(test/exn (run 'C) "")   ;no octave
(test/exn (run 'Cg5) "") ;incorrect b/# symbol
(test/exn (run 'h) "")   ;no
(test (run '0) 0)        ;duh




(test (run '{with {c {sequence {note 10 20 30}}}
                  c})
      (seqV (list (noteV (pitchV 10) (velV 20) (durV 30)))))

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
        (noteV (pitchV 10) (velV 20) (durV 30))
        (noteV (pitchV 20) (velV 20) (durV 20))
        (noteV (pitchV 30) (velV 20) (durV 10))
        (noteV (pitchV 30) (velV 30) (durV 30)))))

