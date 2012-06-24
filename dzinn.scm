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


(define *graph-height* 20) ;; fixme
(define *graph-width* 100) ;; fixme
(define *cpu-max-val* 100) ;; fixme     cpu=2 => max-cpu-use=200%
(define *cpu-quantity* 2) ;; fixme
(define *bar-width* 2) ;; fixme
(define *bar-vertical-space* 0) ;; fixme with numbers more than zero -> error with height
(define *bar-horizontal-space* 1) ;; fixme with 0 generate error
(define *refresh-time* 1) ;; fixme



(define *color-0* #xEFBC69) ;; orange system out
(define *color-1* #x02FAFA) ;; aqua   user   in
(define *color-2* #x338433) ;; green  nice


(define (scale x in-min in-max out-min out-max) ;; fixme move to battary-scheme
  (let ((dividend (* (- x in-min) (- out-max out-min)))
	(divisor (+ (- in-max in-min) out-min)))
  (/ dividend
     (if (= divisor 0) 1 divisor))))

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



(define (cpu-prepare new-val old-val)
  (map - new-val old-val))


(define (cpu-scale x  in-min in-max out-min out-max)
  ;;                                                     ignore in-max
  (let* ((cpu-val (map (lambda (element) (truncate (scale element in-min (* *cpu-max-val* *cpu-quantity* *refresh-time*) out-min out-max))) x))
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



(define (eth0-prepare new-val old-val)
  (map - new-val old-val))


(define (eth0-scale x in-min in-max out-min out-max)
  (map (lambda (element) (truncate (scale element in-min in-max out-min out-max))) x))




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
      (let* ((y-round (if (> y *graph-height*) *graph-height* y)) ;; strange after scale
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







(define (generate-splitted-particle-bar y prev-height num)
  ;; 
  ;;^fg(white)^p(1)^r(3x5+0-6)
  ;;    color  position
  ;;             1   --> gcpubar -s g -w 100 -h 10 -gs <<1>> -gw 2
  ;;                rectangle WIDTHxHEIGHT±X±Y
  ;;
  ;; num - index number of element in list
  ;; 
  (if (and (= y 0) (> num 0)) ;; fixme
      ""
      (let* ((y-round (if (> y (/ *graph-height* 2)) (/ *graph-height* 2) y)) ;; strange after scale
	     (y-prev-round (if (> prev-height *graph-height*) *graph-height* prev-height))
	     (y-shift (if (= num 0)
			  (- 0 (ceiling (/ y-round 2)) *bar-vertical-space*)    ;; in
			  (+ 0 (ceiling (/ y-round 2)) *bar-vertical-space*)))) ;; out

	(format #f "^fg(#~6,'0x)^p(~d)^r(~dx~d~@d~@d)"
		;;(cond ((< y (* *graph-height* (/ 1 3))) #x777777)
		;;  ((< y (* *graph-height* (/ 2 3))) #xAAAAAA)
		;;(else #xFFFFFF))
		(cond ((= num 0) *color-1*)  ;; in   ;; fixme: color1 for 0; and color0 for 1;
		      ((= num 1) *color-0*)  ;; out
		      (else      *color-1*)) ;; in        ;;(ash color 8)
		(if (= num 0) *bar-horizontal-space* (- *bar-width*))
		*bar-width*
		y-round
		0
		y-shift))))



(define (generate-splitted-multi-bar x-list)
  ;; x-list only pair list: (a b)
  ;;^fg(white)^p(1)^r(3x5+0-6)
  ;;    color  position
  ;;             1   --> gcpubar -s g -w 100 -h 10 -gs <<1>> -gw 2
  ;;                rectangle WIDTHxHEIGHT±X±Y
  (define (multi-bar lst previous num)
    (if (null? lst)
	'()
	(cons (generate-splitted-particle-bar (car lst) previous num) (multi-bar (cdr lst) (car lst) (+ num 1)))))
  (string-join (multi-bar x-list 0 0) ""))















(define (mygraph func-read-raw func-prepare func-scale func-draw old-raw history-list incremental? tsleep)
  ;; func-read-raw return list of numbers
  ;;               (1073962454 45394130)
  ;; func-prepare  return difference between current and old value (proc-stat) or return not modified value (temperature)
  ;;               (input: old value for calculate difference)
  ;;               (3000 1000)
  ;; func-scale    convert numbers from real value to value for graph
  ;;               (3000 1000) => (3 1)
  ;; func-draw     draw graph
  ;;               (input value = (3 1))
  ;;               "^fg(#efbc69)^p(1)^r(2x3+0+10)^fg(#efbc69)^p(1)^r(2x1+0+10)"
  ;; old           previous list of value before delay
  ;;               (6927 3174)
  ;; history-list  history list contains not scalled value
  ;;               ((6927 3174) (3000 1000))
  ;; incremental?  how to draw graph
  ;;               #t or #f
  ;; tsleep        delay

  (let* ((new-raw (func-read-raw))
	 (new (func-prepare new-raw old-raw))
	 (max-val (if incremental?
		      (max (apply max (map (lambda (x) (apply + x)) history-list)) (apply + new)) ;; max inccrement
		      (max (apply max (map (lambda (x) (apply max x)) history-list)) (apply max new)))) ;; max from all
	 (new-history-list (append (cdr history-list) (list new))))
    (display "^ib(1)")
    
    (if incremental?
	(map display (map func-draw (map (lambda (x) (func-scale x 0 max-val 0 *graph-height*)) new-history-list)))
	(map display (map func-draw (map (lambda (x) (func-scale x 0 max-val 0 (/ *graph-height* 2))) new-history-list))))
    (display "^fg()")
    (newline)
    (force-output *stdout*)
    
    (sleep tsleep)
    (mygraph func-read-raw func-prepare func-scale func-draw new-raw new-history-list incremental? tsleep)))




;(mygraph eth0-stat     eth0-prepare eth0-scale generate-splitted-multi-bar (eth0-stat) (make-list (truncate (/ *graph-width* (+ *bar-width* *bar-horizontal-space*))) (list 0 0)) #f *refresh-time*)

(mygraph proc-stat       cpu-prepare  cpu-scale  generate-incremental-multi-bar (proc-stat) (make-list (truncate (/ *graph-width* (+ *bar-width* *bar-horizontal-space*))) (list 0 0 0)) #t *refresh-time*)





