#lang plai
;Proof of concept

(require "../MSE-lang-design.rkt")
(require "osc.rkt")

;Using racket osc.rkt by Hogeschool voor de Kunsten Utrecht
;https://github.com/marcdinkum/racket-music

;post parser -> takes MSE-values and sends osc messages
;FOR maxMSP demo
(osc-open "localhost" 8000)

(define (post-eval val)
  (type-case MSE-Value val
    [noteV (pit vel dur)(local [(define p (pitV-pit pit))
                                (define v (velV-vel vel))
                                (define d (durV-dur dur))]
                          (osc-send-msg "noteV" "iii" (list p v d)))]
    [seqV (vals) (osc-send-msg "seqV" "i" '(1)) ;sending...
          (for-each (lambda (note) (post-eval note)) vals)
          (osc-send-msg "seqV" "i" '(0))] ;done!
    [closureV (param body env) (error "post-parse does not handle closureVs")]
    [else (error ("post-parse expects MSE-Values"))]))

(post-eval (seqV
            (list
             (noteV (pitV 47) (velV 60) (durV 1000))
             (noteV (pitV 48) (velV 60) (durV 1000))
             (noteV (pitV 47) (velV 60) (durV 1000))
             (noteV (pitV 48) (velV 60) (durV 1000))
             (noteV (pitV 50) (velV 60) (durV 1000))
             (noteV (pitV 48) (velV 60) (durV 1000))
             (noteV (pitV 47) (velV 60) (durV 1000))
             (noteV (pitV 48) (velV 60) (durV 1000))
             (noteV (pitV 50) (velV 60) (durV 1000))
             (noteV (pitV 48) (velV 60) (durV 1000)))))

;Markov fun!
#;(post-eval (run '{with {DVEL 60} {with {DDUR 1000} {markov {seqn-p C4 C4 C4 C4 C5} 10 C4}}}))

;Twinkle Twinkle (as seen in poster example)
#;(post-eval (run '{with {DVEL 60}
                         {with {DDUR 1000}
                               {seqn-p C4 C4 G4 G4 A4 A4 G4 F4 F4 E4 E4 D4 D4 C4 G4 G4 F4 F4 E4 E4 D4 G4 G4 F4 F4 E4 E4 D4 C4 C4 G4 G4 A4 A4 G4 F4 F4 E4 E4 D4 G4 C4}}}))
                                       
;Happy birthday! Rhythym and all!
#;(post-eval (run '{with {DVEL 60 }
                         {with {melody {seqn-p A4 A4 B4 A4 D5 C#5 A4 A4 B4 A4 E5 D5 A4 A4 A5 F#5 D5 C#5 B4 G5 G5 F#5 D5 E5 D5}}
                               {with {rhythym {seqn-d 750 250 1000 1000 1000 2000 750 250 1000 1000 1000 2000 750 250 1000 1000 1000 1000 1000 750 250 1000 1000 1000 2000}}
                                     {zip melody melody rhythym}}}}))

;Happy birthday! Up a 5th?!
#;(post-eval (run '{transpose {with {DVEL 60 }
                                    {with {melody {seqn-p A4 A4 B4 A4 D5 C#5 A4 A4 B4 A4 E5 D5 A4 A4 A5 F#5 D5 C#5 B4 G5 G5 F#5 D5 E5 D5}}
                                          {with {rhythym {seqn-d 750 250 1000 1000 1000 2000 750 250 1000 1000 1000 2000 750 250 1000 1000 1000 1000 1000 750 250 1000 1000 1000 2000}}
                                                {zip melody melody rhythym}}}}
                              7}))



