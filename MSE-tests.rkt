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
(test/exn (parse '{with {seqn-v 1} 1}) "")
(test/exn (parse '{with {seqn-d 1} 1}) "")
(test/exn (parse '{with {seq-append 1} 1}) "")
(test/exn (parse '{with {with 1} 1}) "")
(test/exn (parse '{with {fun 1} 1}) "")
(test/exn (parse '{with {interleave 1} 1}) "")
(test/exn (parse '{with {insert 1} 1}) "")
(test/exn (parse '{with {transpose 1} 1}) "")
(test/exn (parse '{with {changePits 1} 1}) "")
(test/exn (parse '{with {changeVels 1} 1}) "")
(test/exn (parse '{with {changeDurs 1} 1}) "")
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
(test (desugar (parse '{with {c {seqn-p C4 C5 C6}}
                             {seq-append c c}}))
      (i-app
       (i-fun 'c (i-insert (i-id 'c) (i-id 'c) (i-num -1)))
       (i-seqn 'p (list (i-id 'C4) (i-id 'C5) (i-id 'C6)))))


;; sequence
(test (desugar (parse '{sequence {note 40 50 60}}))
      (i-sequence (list (i-note (i-num 40) (i-num 50) (i-num 60)))))
(test (desugar (parse '{sequence {note 40 50 60}
                                 {note 50 60 70}}))
      (i-sequence (list (i-note (i-num 40) (i-num 50) (i-num 60))
                        (i-note (i-num 50) (i-num 60) (i-num 70)))))

;; seqn-x s
(test (desugar (parse '(seqn-p A4 A5 A6)))
      (i-seqn 'p (list (i-id 'A4) (i-id 'A5) (i-id 'A6))))
(test (desugar (parse '(seqn-v 60 62 73)))
      (i-seqn 'v (list (i-num 60) (i-num 62) (i-num 73))))
(test (desugar (parse '(seqn-d 1000 0 100000)))
      (i-seqn 'd (list (i-num 1000) (i-num 0) (i-num 100000))))

;; happy birthday pitch sequence
(test (desugar (parse '(seqn-p A4 A4 B4 A4 D5 C#5 A4 A4 B4 A4 E5 D5 A4 A4 A5 F#5 D5 C#5 B4 G5 G5 F#5 D5 E5 D5 )))
      (i-seqn 'p
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
       (i-num -1)))
(test (desugar (parse '{seq-append {seqn-p C4 C4} {seqn-p G4 G4 A4 A4 G4}}))
      (i-insert (i-seqn 'p (list (i-id 'C4) (i-id 'C4)))
                (i-seqn 'p (list (i-id 'G4) (i-id 'G4) (i-id 'A4) (i-id 'A4) (i-id 'G4)))
                (i-num -1)))

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
      (i-interleave (i-seqn 'p (list (i-id 'A4))) (i-seqn 'p (list (i-id 'A4)))))
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
      (i-insert (i-seqn 'p (list (i-id 'A4) (i-id 'A4)))
                (i-seqn 'p (list (i-id 'B4) (i-id 'B4)))
                (i-num 1)))
;; transpose
(test (desugar (parse '{transpose {sequence {note 30 40 50}} 14}))
      (i-transpose (i-sequence (list (i-note (i-num 30) (i-num 40) (i-num 50)))) (i-num 14)))
(test (desugar (parse '{transpose {seqn-p A4} 23}))
      (i-transpose (i-seqn 'p (list (i-id 'A4))) (i-num 23)))
;; changeVelocity
(test (desugar (parse '{changePits {sequence {note 10 20 30}} 40}))
      (changeProp 'p (i-sequence (list (i-note (i-num 10) (i-num 20) (i-num 30)))) (i-num 40)))
(test (desugar (parse '{changePits {seqn-p C4 C4 G4 G4} 7}))
      (changeProp 'p (i-seqn 'p (list (i-id 'C4) (i-id 'C4) (i-id 'G4) (i-id 'G4))) (i-num 7)))

(test (desugar (parse '{changeVels {sequence {note 10 20 30}} 40}))
      (changeProp 'v (i-sequence (list (i-note (i-num 10) (i-num 20) (i-num 30)))) (i-num 40)))
(test (desugar (parse '{changeVels {seqn-v 50 60 70 80} 7}))
      (changeProp 'v (i-seqn 'v (list (i-num 50) (i-num 60) (i-num 70) (i-num 80))) (i-num 7)))

(test (desugar (parse '{changeDurs {sequence {note 10 20 30}} 40}))
      (changeProp 'd (i-sequence (list (i-note (i-num 10) (i-num 20) (i-num 30)))) (i-num 40)))
(test (desugar (parse '{changeDurs {seqn-d 0 10 100 1000} 7}))
      (changeProp 'd (i-seqn 'd (list (i-num 0) (i-num 10) (i-num 100) (i-num 1000))) (i-num 7)))

;;zip
(test (desugar (parse '{zip {sequence {note 1 1 1}} {sequence {note 2 2 2}} {sequence {note 3 3 3}}}))
      (i-zip (i-sequence (list (i-note (i-num 1) (i-num 1) (i-num 1))))
             (i-sequence (list (i-note (i-num 2) (i-num 2) (i-num 2))))
             (i-sequence (list (i-note (i-num 3) (i-num 3) (i-num 3))))))

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
      (i-markov (i-seqn 'p (list (i-id 'C4) (i-id 'D4) (i-id 'E4) (i-id 'F4) (i-id 'G4) (i-id 'A4) (i-id 'B4)))
                (i-num 10)
                (i-seqn 'p (list (i-id 'E4)))))





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

;(test (run 'Cbb4) 46)
;(test (run 'C##4) 50)
;(test (run 'Cb#4) 48)
;(test (run 'C#b4) 48)

(test/exn (run 'B-1) "") ;out of range
(test/exn (run 'C) "")   ;no octave
(test/exn (run 'Cg5) "") ;incorrect b/# symbol
(test/exn (run 'h) "")   ;no
(test (run '0) 0)        ;duh


;;Notes
(test (run '{note 1 2 3})
      (noteV (pitchV 1) (velV 2) (durV 3)))

#;(test (run '{note A1 2 3}) ;note must expect MSE
        (noteV (pitchV 21) (velV 2) (durV 3)))



#;(test/exn (run '{note {note 1 2 3} A1 3})
            "") ;pitchV expects numbers

;; seqn-xs
(test (run '(seqn-p C4 C5 C6))
      (seqV (list
             (noteV (pitchV 48) (velV 10) (durV 10))
             (noteV (pitchV 60) (velV 10) (durV 10))
             (noteV (pitchV 72) (velV 10) (durV 10)))))
(test (run '(seqn-v 60 62 73))
      (seqV (list
             (noteV (pitchV 0) (velV 60) (durV 10))
             (noteV (pitchV 0) (velV 62) (durV 10))
             (noteV (pitchV 0) (velV 73) (durV 10)))))
(test (run '(seqn-d 1000 0 100000))
      (seqV (list
             (noteV (pitchV 0) (velV 10) (durV 1000))
             (noteV (pitchV 0) (velV 10) (durV 0))
             (noteV (pitchV 0) (velV 10) (durV 100000)))))

;;changeProps
(test (run '{changePits {sequence {note 10 20 30}} 40})
      (seqV (list (noteV (pitchV 40) (velV 20) (durV 30)))))

(test (run '{changeVels {sequence {note 10 20 30}} 40})
      (seqV (list (noteV (pitchV 10) (velV 40) (durV 30)))))

(test (run '{changeDurs {sequence {note 10 20 30}} 40})
      (seqV (list (noteV (pitchV 10) (velV 20) (durV 40)))))


(test (run '{with {c {sequence {note 10 20 30}}}
                  c})
      (seqV (list (noteV (pitchV 10) (velV 20) (durV 30)))))


;;interleave
(test (run '{interleave {sequence {note 10 20 30} {note 20 20 20}} {sequence {note 30 20 10} {note 30 30 30}}})
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

;;append
(test (run '(seq-append (sequence (note 10 20 30) (note 20 20 20)) (sequence (note 30 20 10) (note 30 30 30))))
      (seqV (list
             (noteV (pitchV 10) (velV 20) (durV 30))
             (noteV (pitchV 20) (velV 20) (durV 20))
             (noteV (pitchV 30) (velV 20) (durV 10))
             (noteV (pitchV 30) (velV 30) (durV 30)))))

;;insert
(test (run '{insert {seqn-p C4 C4} {seqn-p C5 C5} 0})
      (seqV (list
             (noteV (pitchV 48) (velV 10) (durV 10))
             (noteV (pitchV 48) (velV 10) (durV 10))
             (noteV (pitchV 60) (velV 10) (durV 10))
             (noteV (pitchV 60) (velV 10) (durV 10)))))

(test (run '{insert {seqn-p C4 C4} into {seqn-p C5 C5} at 1})
      (seqV (list
             (noteV (pitchV 60) (velV 10) (durV 10))
             (noteV (pitchV 48) (velV 10) (durV 10))
             (noteV (pitchV 48) (velV 10) (durV 10))
             (noteV (pitchV 60) (velV 10) (durV 10)))))

(test (run '{insert {seqn-p C4 C4} {seqn-p C5 C5} 2})
      (seqV (list
             (noteV (pitchV 60) (velV 10) (durV 10))
             (noteV (pitchV 60) (velV 10) (durV 10))
             (noteV (pitchV 48) (velV 10) (durV 10))
             (noteV (pitchV 48) (velV 10) (durV 10)))))

(test/exn (run '{with {i 2} {insert {seqn-p C4 C4} {seqn-p C5 C5} i}})
          "") ;indexes must be provided as numbers, not variables

;;zip
(test (run '{zip {sequence {note 1 1 1}} {sequence {note 2 2 2}} {sequence {note 3 3 3}}})
      (seqV (list
             (noteV (pitchV 1) (velV 2) (durV 3)))))

(test (run '{zip {sequence {note 1 1 1} {note 1 1 1}} {sequence {note 2 2 2} {note 2 2 2}} {sequence {note 3 3 3} {note 3 3 3}}})
      (seqV (list
             (noteV (pitchV 1) (velV 2) (durV 3))
             (noteV (pitchV 1) (velV 2) (durV 3)))))

;;TODO
(test (run '{zip {sequence {note 1 1 1}} {sequence {note 2 2 2} {note 2 2 2}} {sequence {note 3 3 3} {note 3 3 3}}})
      (seqV (list
             (noteV (pitchV 1) (velV 2) (durV 3)))))

(test (run '{zip {seqn-p C4 D4 E4 F4 G4 A4 B4 C5} {seqn-v 40 45 50 55 60 65 70 75} {seqn-d 1000 1000 1000 1000 1000 1000 1000 1000}})
      (seqV (list
             (noteV (pitchV 48) (velV 40) (durV 1000))
             (noteV (pitchV 50) (velV 45) (durV 1000))
             (noteV (pitchV 52) (velV 50) (durV 1000))
             (noteV (pitchV 53) (velV 55) (durV 1000))
             (noteV (pitchV 55) (velV 60) (durV 1000))
             (noteV (pitchV 57) (velV 65) (durV 1000))
             (noteV (pitchV 59) (velV 70) (durV 1000))
             (noteV (pitchV 60) (velV 75) (durV 1000)))))

;;A sequence zipped with itself returns a new equivalent sequence
(test (run '{with {s {seqn-p C4 D4 E4 F4 G4 A4 B4 C5}}
                  {zip s s s}})
      (run '{seqn-p C4 D4 E4 F4 G4 A4 B4 C5}))



