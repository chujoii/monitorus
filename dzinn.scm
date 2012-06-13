#!/usr/bin/guile-2.0 -s
!#
; coding: utf-8

;;;; dzinn.scm ---  generate text graph for dzen



;;; Copyright (C) 2012 Roman V. Prikhodchenko



;;; Author: Roman V. Prikhodchenko <chujoii@gmail.com>



;;;    This file is part of dzinn.
;;;
;;;    dzinn is free software: you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.
;;;
;;;    dzinn is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with dzinn.  If not, see <http://www.gnu.org/licenses/>.



;;; Keywords: graph plot dzen monitoring system parameter cpu net disk



;;; Usage:

;; ./mycpubar.scm | dzen2 -x 1170 -y 30 -w 100 -h 10 -ta l -bg black -dock
;;  while true; do cat graph.txt ; sleep 1; done | dzen2 -x 1150 -y 30 -w 100 -h 100 -bg black -ta l -dock
;; ^ib(1)^p(-98)^fg(blue)^r(20x10)^fg(orange)^p(3)^r(40x10-50+10)^p(4)
;; gcpubar -s g -w 100 -h 10 -gs 1 -gw 2  | dzen2 -x 0 -y 700 -w 120 -h 30 -ta l -bg black



;;; History:

;; Version 0.1 was created at 2012.february.03



;;; Code:








(use-modules (ice-9 format))
(use-modules (ice-9 regex)) ;; for match:substring
(load "../battery-scheme/file-contents.scm")


(define *stdout* (current-output-port))
(define *stderr* (current-error-port))


(define *graph-height* 20) ;; fixme     real-hight=40
(define *graph-width* 100) ;; fixme     real-hight=40
(define *cpu-max-val* 100) ;; fixme     cpu=2 => max-cpu-use=200%
(define *cpu-quantity* 2) ;; fixme
(define *bar-width* 2) ;; fixme
(define *bar-vertical-space* 0) ;; fixme with numbers more than zero -> error with height
(define *bar-horizontal-space* 1) ;; fixme with 0 generate error
(define *refresh-time* 1) ;; fixme



(define *color-0* #xEFBC69) ;; orange
(define *color-1* #x02FAFA) ;; aqua
(define *color-2* #x338433) ;; green


(define (scale x in-min in-max out-min out-max) ;; fixme move to battary-scheme
  (/ (* (- x in-min) (- out-max out-min))
     (+ (- in-max in-min) out-min)))

(define (constrain x a b) ;; fixme move to battary-scheme
  (cond ((< x a) a)
	((> x b) b)
	(else    x)))






(define (proc-stat)
  ;;(let ((ps-user (list-ref proc-stat 0)) ;; user: normal processes executing in user mode
  ;;      (ps-nice (list-ref proc-stat 1)) ;; nice: niced processes executing in user mode
  ;;      (ps-system (list-ref proc-stat 2)) ;; system: processes executing in kernel mode
  ;;      (ps-idle (list-ref proc-stat 3)) ;; idle: twiddling thumbs
  ;;      (ps-iowait (list-ref proc-stat 4)) ;; iowait: waiting for I/O to complete
  ;;      (ps-irq (list-ref proc-stat 5)) ;; irq: servicing interrupts
  ;;      (ps-softirq (list-ref proc-stat 6))) ;; softirq: servicing softirqs)))
  
  ;; return (sys user nice)
  (let ((p-s (list-head (map string->number (map match:substring (list-matches "[0-9]+" (first-line-of-file "/proc/stat"))))
			3)))
    (list (list-ref p-s 2)    ;; sys
	  (list-ref p-s 0)    ;; user
	  (list-ref p-s 1)))) ;; nice



(define (cpu-value new-val old-val max-val)
  (let* ((cpu-val (map (lambda (x) (truncate (scale x 0 (* *cpu-max-val* *cpu-quantity* *refresh-time*) 0 *graph-height*)))
		       (map - new-val old-val)))
	 (cpu-max (apply + cpu-val)))
    
    (if (> cpu-max *cpu-max-val*) ;; very strange: (+ sys user nice) > 100% -> need correction: change nice value
	(let* ((cpu-nice-index (- (length cpu-val) 1))
	       (cpu-nice       (list-ref cpu-val cpu-nice-index)))
	  (list-set! cpu-val cpu-nice-index (- cpu-nice (- cpu-max *cpu-max-val*)))))
    cpu-val))







(define (eth0-stat) ;; fixme fixme fixme need wlan0, ...
  ;; return (in out)
  (list (car (map string->number (map match:substring (list-matches "[0-9]+" (first-line-of-file "/sys/class/net/eth0/statistics/rx_bytes")))))   ;; rx
	(car (map string->number (map match:substring (list-matches "[0-9]+" (first-line-of-file "/sys/class/net/eth0/statistics/tx_bytes"))))))) ;; tx



(define (eth0-value new-val old-val max-val)
  (let* ((eth0-val (map (lambda (x) (truncate (scale x 0 (* *cpu-max-val* *cpu-quantity* *refresh-time*) 0 *graph-height*)))
			(map - new-val old-val)))
	 (cpu-max (apply + cpu-val)))
    
    net-val))




(define (generate-simple-bar x-list)
  ;; 
  ;;^fg(white)^p(1)^r(3x5+0-6)
  ;;    color  position
  ;;             1   --> gcpubar -s g -w 100 -h 10 -gs <<1>> -gw 2
  ;;                rectangle WIDTHxHEIGHT±X±Y
  (let* ((y (apply + x-list))
	 (y-round (if (> y *graph-height*) *graph-height* y))
	 (y-shift (truncate (- (/ *graph-height* 2) (/ y-round 2)))))
    
    (format #f "^fg(#~6,'0x)^p(~d)^r(~dx~d~@d~@d)"
	    (cond ((< y (* *graph-height* (/ 1 3))) #x777777)
		  ((< y (* *graph-height* (/ 2 3))) #xAAAAAA)
		  (else #xFFFFFF))
	    *bar-horizontal-space*
	    *bar-width*
	    y-round
	    0
	    y-shift)))


(define (generate-incremental-particle-bar y prev-height num)
  ;; 
  ;;^fg(white)^p(1)^r(3x5+0-6)
  ;;    color  position
  ;;             1   --> gcpubar -s g -w 100 -h 10 -gs <<1>> -gw 2
  ;;                rectangle WIDTHxHEIGHT±X±Y
  (if (and (= y 0) (> num 0)) ;; fixme
      ""
      (let* ((y-round (if (> y *graph-height*) *graph-height* y))
	     (y-prev-round (if (> prev-height *graph-height*) *graph-height* prev-height))
	     (y-shift (- (ceiling (- (/ *graph-height* 2) (/ y-round 2)))
			 y-prev-round
			 *bar-vertical-space*))
	     (y-overgrown (- (- y-round y-shift) *graph-height*))
	     (y-result (if (> 0 y-overgrown) y-round (- y-round y-overgrown))))
	
	(format #f "^fg(#~6,'0x)^p(~d)^r(~dx~d~@d~@d)"
		;;(cond ((< y (* *graph-height* (/ 1 3))) #x777777)
		;;  ((< y (* *graph-height* (/ 2 3))) #xAAAAAA)
		;;(else #xFFFFFF))
		(cond ((= num 0) *color-0*)  ;; system 
		      ((= num 1) *color-1*)  ;; user
		      ((= num 2) *color-2*)  ;; nice
		      (else      *color-0*)) ;; system        ;;(ash color 8)
		(if (= num 0) *bar-horizontal-space* (- *bar-width*))
		*bar-width*
		y-round
		0
		y-shift))))



(define (generate-incremental-multi-bar x-list)
  ;; 
  ;;^fg(white)^p(1)^r(3x5+0-6)
  ;;    color  position
  ;;             1   --> gcpubar -s g -w 100 -h 10 -gs <<1>> -gw 2
  ;;                rectangle WIDTHxHEIGHT±X±Y
  (define (multi-bar lst previous num)
    (if (null? lst)
	'()
	(cons (generate-incremental-particle-bar (car lst) previous num) (multi-bar (cdr lst) (car lst) (+ num 1)))))
  (string-join (multi-bar x-list 0 0) ""))















(define (mygraph func-generate func-scale func-draw old history-list incremental? tsleep)
  (let* ((new (func-generate))
	 (max-val (if incremental?
		      (max (apply max (map (lambda (x) (apply + x)) history-list)) (apply + new)) ;; max inccrement
		      (max (apply max (map (lambda (x) (apply max x)) history-list)) (apply max new)))) ;; max from all
	 (new-history-list (append (cdr history-list) (list (func-scale new old max-val)))))
    (display "^ib(1)")(map display (map func-draw new-history-list))(display "^fg()")(newline)(force-output *stdout*)
    
    (sleep tsleep)
    (mygraph func-generate func-scale func-draw new new-history-list incremental? tsleep)))








(mygraph proc-stat cpu-value generate-incremental-multi-bar (proc-stat) (make-list (truncate (/ *graph-width* (+ *bar-width* *bar-horizontal-space*))) (list 0 0 0)) #t *refresh-time*)

;(mygraph eth0-stat eth0-value generate-incremental-multi-bar (proc-stat) (make-list (truncate (/ *graph-width* (+ *bar-width* *bar-horizontal-space*))) (list 0 0 0)) #f *refresh-time*)



