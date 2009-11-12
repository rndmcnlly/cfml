
;; Copyright (c) 2009 Adam M. Smith
;; 
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.


;;;;        __           _ 
;;;;   ___ / _|_ __ ___ | |
;;;;  / __| |_| '_ ` _ \| |
;;;; | (__|  _| | | | | | |
;;;;  \___|_| |_| |_| |_|_| context-free music language
;;;; by Adam M. Smith (adam@adamsmith.as)

;; cfml is a library-based language within impromptu for
;; livecoding context-free music grammar (inspired by
;; cfdg for visual art).

;; Terminals: (literal l '((o c p v d)..))) ; play a list of notes
;;                                          ; l = total duration (used for the after combinator)
;;                                          ; o = relative onset time (beats)
;;                                          ; c = midi channel
;;                                          ; p = relative pitch number (in key)
;;                                          ; v = relative volume (1 is normal)
;;                                          ; d = relative duration (beats)

;; Combinators: (after comp1 comp2)  ; play comp2 after comp1's duration ends
;;              (during comp1 comp2) ; play comp1 and comp2 at the same time
;;              (choose comp1 comp2) ; randomly choose bewteen comp1 and comp2
;; -- all combinators take 2 or more parameters

;; Modifiers: (tra shift comp1) ; transpose comp1 by shift steps in the key
;;            (dur ratio comp1) ; scale the duration of comp1 by a ratio
;;            (vol ratio comp1) ; change the volume of comp1 by a ratio

;; Abstractions: (def (loopy-rule)
;;                 (after another-rule
;;                        (choose epic-finale
;;                                (tra +2 loopy-rule))))

;; Performance: (perform loopy-rule ; some rule or in-line composition expression
;;                       tempo  ; a tempo in beats-per-minute
;;                       pitch) ; a pitch class from andrew's pc-lib

;; see the end of the file for an example grammar

;; PANIC!!!
;; If you have a recursive rule that won't stop playing,
;; redefine the rule with no body to halt the recursion.

;;;;
;;;; au setup
;;;; 

(au:clear-graph)
(define *synth* (au:make-node "aumu" "dls " "appl"))
(au:connect-node *synth* 0 *au:output-node* 0)
(au:update-graph)

(au:set-param (now) *synth* 1 *au:global-scope* 0 -3)
(au:set-param (now) *synth* 2 *au:global-scope* 0 +8)

;;;;
;;;; general midi instruments
;;;;

(au:midi-out (now) *synth* *io:midi-pc* 0 117 0)
(au:midi-out (now) *synth* *io:midi-pc* 1 115 0)
(au:midi-out (now) *synth* *io:midi-pc* 2 33 0)
(au:midi-out (now) *synth* *io:midi-pc* 3 24 0)
(au:midi-out (now) *synth* *io:midi-pc* 4 41 0)

; primitives: piano, string, drum ?

; rule: () => one of the following
;              (composition total-duration list-of-notes)
;              (choose rule1 rule2)
;              (after rule1 rule2)
;              (during rule1 rule2)
; literal: (r-ontime chan r-pitch r-volume r-duration)

;; must be true of performance implementation functions:
;;  they return as soon all left-most literals are scheduled
;;  AND
;;  the continuation is called with the total subtree duration once known

(define (anticipate time) (- time 4410))
(define (sps tempo) (/ (* 44100 60) tempo))


(define (literal . args) (cons 'op-literal args))
(define (choose  . args) (cons 'op-choose args))
(define (during . args)
   (let ((len (length args)))
      (if (= len 2)
          (cons 'op-during args)
          (if (> len 2)
              `(op-during ,(car args) ,(apply during (cdr args)))))))
(define (after . args)
   (let ((len (length args)))
      (if (= len 2)
       (cons 'op-after args)
       (if (> len 2)
           `(op-after ,(car args) ,(apply after (cdr args)))))))

(define (perform score tempo scale)
   (print '~)
   (print 'starting-composition)
   (let ((composition-result
          (call/cc (lambda (k)
                      (perform-dispatch score
                                        tempo
                                        scale
                                        0
                                        (+ (now) 4410)
                                        64
                                 k)))))
      (print 'composition-result composition-result)
      ))
                      

(define (perform-dispatch score tempo scale root time volume k)
   ;(print 'perform-dispatch score)
   (if (procedure? score)
       (perform-dispatch (score) tempo scale root time volume k)
       (let ((op   (car score))
             (args (cdr score)))
          (case op
                ((op-choose)  (perform-choose args tempo scale root time volume k))
                ((op-after)   (perform-after args tempo scale root time volume k))
                ((op-during)  (perform-during args tempo scale root time volume k))
                ((op-literal) (perform-literal args tempo scale root time volume k))
                ((op-tweak)   (perform-tweak args tempo scale root time volume k))))))


(define (perform-literal args tempo scale root time volume k)
   ;(print 'perform-literal args)
   (let ((duration (car args))
         (notes    (cadr args)))
      (map (lambda (n) (perform-note n tempo scale root time volume)) notes)
      (callback time k duration)))

(define (perform-note note tempo scale root time ref-volume)
   (let ((rel-ontime   (list-ref note 0))
         (channel      (list-ref note 1))
         (rel-pitch    (list-ref note 2))
         (rel-volume   (list-ref note 3))
         (rel-duration (list-ref note 4)))
      (let ((ontime (+ time (* rel-ontime (sps tempo))))
            (pitch (pc:relative 60 (+ root rel-pitch) scale))
            (volume (min 127 (* rel-volume ref-volume)))
            (duration (* rel-duration (sps tempo))))
         (play-note ontime *synth* pitch volume duration channel))))

(define (perform-choose args tempo scale root time volume k)
   ;(print 'perform-choose args)
   (let ((chosen-one (list-ref args (random (length args)))))
      (perform-dispatch chosen-one tempo scale root time volume k)))

(define (perform-during args tempo scale root time volume k)
   ;(print 'perform-during args)
   (let ((first-res #f))
      (let ((this-res (call/cc (lambda (k2)
                                  (perform-dispatch (car args)  tempo scale root time volume k2)
                                  (perform-dispatch (cadr args) tempo scale root time volume k2)))))
         ; at this point we don't know who returned first, but, wlog, someone sure did
         (if (number? this-res)
             (if (number? first-res)
                 (k (max first-res this-res))
                 (begin (set! first-res this-res)
                        'during-leaf-a))
             'during-leaf-b))))

(define (perform-after args tempo scale root time volume k)
   ;(print 'perform-after args)
   (let ((left-res (call/cc (lambda (k2)
                               (perform-dispatch (car args) tempo scale root time volume k2)))))
      (if (number? left-res)
          (let ((right-res (call/cc (lambda (k2)
                                       (callback (anticipate (+ time (* left-res (sps tempo))))
                                                 perform-dispatch
                                                 (cadr args) tempo scale root (+ time (* left-res (sps tempo))) volume k2)))))
             (if (number? right-res)
                 (k (+ left-res right-res))
                 'after-leaf-a))
          'after-leaf-b)))

(define (perform-tweak args tempo scale root time volume k)
   ;(print 'perform-tweak args)
   (let* ((comp (car args))
          (pre  (cadr args))
          (post (caddr args))
          (tweaked (append (list comp) (pre tempo scale root time volume) (list k)))
          (res (apply perform-dispatch tweaked)))
      (post res)))

(define (dur factor comp)
   (list 'op-tweak
         comp
         (lambda (tempo scale root time volume)
            (list (* tempo factor) scale root time volume))
         (lambda (dur) (/ dur factor))))

(define (tra shift comp)
   (list 'op-tweak
         comp
         (lambda (tempo scale root time volume)
            (list tempo scale (+ shift root) time volume))
         (lambda (dur) dur)))

(define (vol ratio comp)
   (list 'op-tweak
         comp
         (lambda (tempo scale root time volume)
            (list tempo scale root time (* ratio volume)))
         (lambda (dur) dur)))

;;;;
;;;; top-level-style defintions (try redefining them while the song is playing)
;;;;


(define (bump)
   (literal 1/2 '((0 3 0 1 1/2))))

(define (lump)
   (after bump
          (tra 2 bump)
          (tra 4 bump)
          (tra 5 bump)))

(define (string-step)
   (literal 2 '((0 4 0 1 2)(1 4 7 1 1)(0 4 2 1 2))))

(define (string-end)
   (literal 2 '((0 4 4 1 1)(1 4 -1 1 1))))

(define (song)
   (choose (vol 2/3 string-end)
           (tra +2 (after (during string-step
                                 (after lump (tra -4 lump)))
                         song))))

(perform song 120 (pc:scale 0 'dorian))

