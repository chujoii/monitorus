#!/usr/bin/guile -s
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

;; nice -n 19 project/dzinn/dzinn.scm | dzen2 -x 1000 -y 0 -w 300 -h 20 -ta l -bg black -dock -fn snap:size=8 &
;;  while true; do cat graph.txt ; sleep 1; done | dzen2 -x 1150 -y 30 -w 100 -h 100 -bg black -ta l -dock
;; cat graph.txt | dzen2 -p
;; ^ib(1)^p(-98)^fg(blue)^r(20x10)^fg(orange)^p(3)^r(40x10-50+10)^p(4)
;; gcpubar -s g -w 100 -h 10 -gs 1 -gw 2  | dzen2 -x 0 -y 700 -w 120 -h 30 -ta l -bg black



;;; History:

;; Version 0.1 was created at 2012.february.03



;;; Code:







;; gkrellm
;; /proc/diskstats
;; /proc/2976/net/dev
;; /proc/vmstat
;; /proc/stat
;; /proc/2976/net/route

(use-modules (ice-9 format))
(use-modules (ice-9 regex)) ;; for match:substring
(load "../battery-scheme/file-contents.scm")
(load "../battery-scheme/string.scm")
(load "../battery-scheme/system-cmd.scm")

(define *stdout* (current-output-port))
(define *stderr* (current-error-port))




;; ---------------------- start config ----------------------

(define *refresh-time* 1)

(define *graph-width* 50) ;; (* (/ 60secondsinminute *refresh-time*) (+ *bar-width* *bar-horizontal-space*)) = 60
(define *graph-height* 19) ;; check *bar-vertical-space*
(define *bar-width* 2)
(define *bar-horizontal-space* 1)
(define *bar-vertical-space* 1) ;; check *graph-height*

;; you can set *graph-height* less that real graph height: real_dzen_height=20 then set *graph-height*=18 for 1 pixel border
;; (define *horizontal-border* 0)
;; (define *vertical-border* 0)

;; if *graph-height* is odd, then set *bar-vertical-space* to odd
;; if *graph-height* is even, then set *bar-vertical-space* to even
;; because:
;;
;; for splitted graph with *bar-vertical-space* = even (0):
;;
;; if *graph-height* is even (8)     |    if odd (7)
;;      A                            |         B          C          D
;; 1 HHHHHHH                         |    1 ???????    HHHHHHH    HHHHHHH
;; 2 HHHHHHH                         |    2 HHHHHHH    HHHHHHH    HHHHHHH
;; 3 HHHHHHH                         |    3 HHHHHHH    HHHHHHH    HHHHHHH
;; 4 HHHHHHH                         |    4 HHHHHHH OR LLLLLLL OR HH???LL
;; 5 LLLLLLL                         |    5 LLLLLLL    LLLLLLL    LLLLLLL
;; 6 LLLLLLL                         |    6 LLLLLLL    LLLLLLL    LLLLLLL
;; 7 LLLLLLL                         |    7 LLLLLLL    ???????    LLLLLLL
;; 8 LLLLLLL                         |
;; normal graph                      |    strange graph ??????? (D - *bar-vertical-space*===1 not 0)
;;
;;
;; for splitted graph with *bar-vertical-space* = odd (1):
;;
;; if  *graph-height*  is even (8)   |    if odd (7)
;;     A           B          C      |         D
;; 1 HHHHHHH    ???????    HHHHHHH   |    1 HHHHHHH
;; 2 HHHHHHH    HHHHHHH    HHHHHHH   |    2 HHHHHHH
;; 3 HHHHHHH    HHHHHHH    HHHHHHH   |    3 HHHHHHH
;; 4 ~~~~~~~ OR HHHHHHH OR ???~~~~   |    4 ~~~~~~~
;; 5 LLLLLLL    ~~~~~~~    ~~~~???   |    5 LLLLLLL
;; 6 LLLLLLL    LLLLLLL    LLLLLLL   |    6 LLLLLLL
;; 7 LLLLLLL    LLLLLLL    LLLLLLL   |    7 LLLLLLL
;; 8 ???????    LLLLLLL    LLLLLLL   |
;;                                   |
;; strange graph ???????             |    normal graph
;; (C *bar-vertical-space* = 2 not 1)



(define *ps-cpu-show* 1) ;; number to show ;; fixme
(define *ps-cpu-above* 3) ;; show only processes above (in %) ;; fixme

(define *cpu-quantity* 2) ;; fixme

(define *color-0* #xb17e37) ;; orange system out
(define *color-1* #x439595) ;; aqua   user   in
(define *color-2* #x357d35) ;; green  nice
(define *color-3* #x999999) ;; gray 


;; ---------------------- end config ----------------------








(define *number-of-bar* (truncate (/ (- *graph-width* *bar-horizontal-space*) (+ *bar-width* *bar-horizontal-space*)))) ;; ideally if (truncate fubar)===(fubar) else you can see half-bar width space

(define *cpu-max-val* (* 100 *cpu-quantity* *refresh-time*)) ;; if num-of-cpu=2 => max-cpu-use=200%









(define (scale x in-min in-max out-min out-max) ;; fixme move to battary-scheme
  (let ((dividend (* (- x in-min) (- out-max out-min)))
	(divisor (+ (- in-max in-min) out-min)))
  (/ dividend
     (if (= divisor 0) 1 divisor))))

(define (constrain x a b) ;; fixme move to battary-scheme
  (cond ((< x a) a)
	((> x b) b)
	(else    x)))



(define (search-first-string-in-list lst str) ;; fixme move to battary-scheme
  (if (null? lst)
      '()
      (let ((str-ind (string-contains-ci (car lst) str)))
	(if str-ind
	    (car lst)
	    (search-first-string-in-list (cdr lst) str)))))




(define (diff-prepare new-val old-val)
  (map - new-val old-val))


(define (index-of-max lst)
  (define (indmax im mm lst counter)
    (if (null? lst)
	im
	(if (> (car lst) mm)
	    (indmax counter (car lst) (cdr lst) (+ counter 1))
	    (indmax im mm (cdr lst) (+ counter 1)))))
  (indmax 0 (car lst) lst 0))



(define (cpu-diff-prepare new-val old-val)
  ;; fixme need store real-refresh-time: (+ sys user nice) > 100% (because real-refresh-time === sleep+processing =/= *refresh-time*) -> need correction: change one of cpu value: if for example decrease "nice==0" => -1   need decrease max of cpu value
  (let* ((cpu-val (map - new-val old-val))
	 (cpu-sum (apply + cpu-val))) ;; out-max already iclude sum of *bar-vertical-space*
    
    (if (> cpu-sum *cpu-max-val*)
	(let* ((cpu-overfilled-index (index-of-max cpu-val))
	       (cpu-overfilled       (list-ref cpu-val cpu-overfilled-index)))
	  (list-set! cpu-val cpu-overfilled-index (- cpu-overfilled (- cpu-sum *cpu-max-val*)))))
    
    cpu-val))




(define (simple-scale x in-min in-max out-min out-max)
  (map (lambda (element) (truncate (scale element in-min in-max out-min out-max))) x))




(define (top-stat)
  ;; fixme need use *ps-cpu-above*
  ;; fixme maybe need use another behavior http://forums.anandtech.com/showthread.php?t=297729
  (map (lambda (x) (string-pad-right (string-cut x 5 15) 10))
       (list-head (string-split (system-with-output-to-string (string-append "ps --no-headers -A -o pcpu,comm --sort=-pcpu | head -n " (number->string *ps-cpu-show*))) #\newline) *ps-cpu-show*)))


(define (cpu-stat)
  ;;(let ((ps-user (list-ref cpu-stat 0)) ;; user: normal processes executing in user mode
  ;;      (ps-nice (list-ref cpu-stat 1)) ;; nice: niced processes executing in user mode
  ;;      (ps-system (list-ref cpu-stat 2)) ;; system: processes executing in kernel mode
  ;;      (ps-idle (list-ref cpu-stat 3)) ;; idle: twiddling thumbs
  ;;      (ps-iowait (list-ref cpu-stat 4)) ;; iowait: waiting for I/O to complete
  ;;      (ps-irq (list-ref cpu-stat 5)) ;; irq: servicing interrupts
  ;;      (ps-softirq (list-ref cpu-stat 6))) ;; softirq: servicing softirqs)))
  
  ;; return (sys user nice)
  (let ((p-s (list-head (map string->number (map match:substring (list-matches "[0-9]+" (first-line-of-file "/proc/stat"))))
			3)))
    (list (list-ref p-s 2)    ;; sys
	  (list-ref p-s 0)    ;; user
	  (list-ref p-s 1)))) ;; nice





(define (cpu-scale x in-min in-max out-min out-max)
  ;;                                                     ignore in-max
  (map (lambda (element) (round (scale element in-min *cpu-max-val* out-min out-max))) x))







(define (eth0-stat) ;; fixme fixme fixme need wlan0, ...
  ;; return (in out)
  (list (car (map string->number (map match:substring (list-matches "[0-9]+" (first-line-of-file "/sys/class/net/eth0/statistics/rx_bytes")))))   ;; rx
	(car (map string->number (map match:substring (list-matches "[0-9]+" (first-line-of-file "/sys/class/net/eth0/statistics/tx_bytes"))))))) ;; tx











(define (sda-stat) ;; fixme fixme fixme need sda, sdb, ...
  (let ((sda (map string->number (map match:substring (list-matches "[^ ]+" (search-first-string-in-list (read-lines-list "/proc/diskstats") "sda"))))))
    (list (list-ref sda 3) (list-ref sda 7))))


(define (swap-stat) ;; fixme fixme fixme need sda, sdb, ...
  (let ((swap (map string->number (map match:substring (list-matches "[^ ]+" (search-first-string-in-list (read-lines-list "/proc/diskstats") "sda2"))))))
    (list (list-ref swap 5) (list-ref swap 9))))




(define (generate-simple-bar x-list)
  ;; 
  ;;^fg(white)^p(1)^r(3x5+0-6)
  ;;    color  position
  ;;             1   --> gcpubar -s g -w 100 -h 10 -gs <<1>> -gw 2
  ;;                rectangle WIDTHxHEIGHT±X±Y
  (let* ((y (apply + x-list))
	 (y (if (> y *graph-height*) *graph-height* y))
	 (y-shift (truncate (- (/ *graph-height* 2) (/ y 2)))))
    
    (format #f "^fg(#~6,'0x)^p(~d)^r(~dx~d~@d~@d)"
	    (cond ((< y (* *graph-height* (/ 1 3))) #x777777)
		  ((< y (* *graph-height* (/ 2 3))) #xAAAAAA)
		  (else #xFFFFFF))
	    *bar-horizontal-space*
	    *bar-width*
	    y
	    0
	    y-shift)))


(define (generate-incremental-particle-bar y prev-height num)
  ;; 
  ;;^fg(white)^p(1)^r(3x5+0-6)
  ;;    color  position
  ;;             1   --> gcpubar -s g -w 100 -h 10 -gs <<1>> -gw 2
  ;;                rectangle WIDTHxHEIGHT±X±Y
  (if (and (= y 0) (> num 0)) ;; fixme
      (list "" prev-height)
      (let* ((y-shift (- (ceiling (- (/ *graph-height* 2) (/ y 2)))
			 prev-height)))
	
	(list (format #f "^fg(#~6,'0x)^p(~d)^r(~dx~d~@d~@d)"
		      ;;(cond ((< y (* *graph-height* (/ 1 3))) #x777777)
		      ;;  ((< y (* *graph-height* (/ 2 3))) #xAAAAAA)
		      ;;(else #xFFFFFF))
		      (cond ((= num 0) *color-0*)  ;; system 
			    ((= num 1) *color-1*)  ;; user
			    ((= num 2) *color-2*)  ;; nice
			    (else      *color-0*)) ;; system        ;;(ash color 8)
		      (if (= num 0) *bar-horizontal-space* (- *bar-width*))
		      *bar-width*
		      y
		      0
		      y-shift)
	      (if (= y 0) 0 (+ prev-height y *bar-vertical-space*))))))



(define (generate-incremental-multi-bar x-list)
  ;; 
  ;;^fg(white)^p(1)^r(3x5+0-6)
  ;;    color  position
  ;;             1   --> gcpubar -s g -w 100 -h 10 -gs <<1>> -gw 2
  ;;                rectangle WIDTHxHEIGHT±X±Y
  (define (multi-bar lst previous-high num)
    (if (null? lst)
	'()
	(begin
	  (let ((res-txt-shift (generate-incremental-particle-bar (car lst) previous-high num)))
	    (cons (car res-txt-shift) (multi-bar (cdr lst) (cadr res-txt-shift) (+ num 1)))))))
  

  (if (< (apply + x-list) 1)
      (format #f "^p(~d)" (+ *bar-width* *bar-horizontal-space*)) ;; skip if zero hight of all bar
      (string-join (multi-bar x-list 0 0) "")))







(define (generate-splitted-particle-bar y num)
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
      (let* ((y-shift (if (= num 0)
			  (- 0 (floor (/ (+ y *bar-vertical-space*) 2)))    ;; in
			  (+ 0 (ceiling (/ (+ y *bar-vertical-space*) 2)))))) ;; out
	
	(format #f "^fg(#~6,'0x)^p(~d)^r(~dx~d~@d~@d)"
		;;(cond ((< y (* *graph-height* (/ 1 3))) #x777777)
		;;  ((< y (* *graph-height* (/ 2 3))) #xAAAAAA)
		;;(else #xFFFFFF))
		(cond ((= num 0) *color-1*)  ;; in   ;; fixme: color1 for 0; and color0 for 1;
		      ((= num 1) *color-0*)  ;; out
		      (else      *color-1*)) ;; in        ;;(ash color 8)
		(if (= num 0) *bar-horizontal-space* (- *bar-width*))
		*bar-width*
		y
		0
		y-shift))))



(define (generate-splitted-multi-bar x-list)
  ;; x-list only pair list: (a b)
  ;;^fg(white)^p(1)^r(3x5+0-6)
  ;;    color  position
  ;;             1   --> gcpubar -s g -w 100 -h 10 -gs <<1>> -gw 2
  ;;                rectangle WIDTHxHEIGHT±X±Y
  (define (multi-bar lst num)
    (if (null? lst)
	'()
	(cons (generate-splitted-particle-bar (car lst) num) (multi-bar (cdr lst) (+ num 1)))))
  (if (< (apply + x-list) 1)
      (format #f "^p(~d)" (+ *bar-width* *bar-horizontal-space*)) ;; skip if zero hight of all bar
      (string-join (multi-bar x-list 0) "")))















(define (mygraph func-read-raw func-prepare func-scale func-draw old-raw history-list incremental?)
  ;; func-read-raw return list of numbers
  ;;               (1073962454 45394130)
  ;; func-prepare  return difference between current and old value (cpu-stat) or return not modified value (temperature)
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

  (let* ((new-raw (func-read-raw))
	 (new (func-prepare new-raw old-raw))
	 (max-val (if incremental?
		      (max (apply max (map (lambda (x) (apply + x)) history-list)) (apply + new)) ;; max inccrement
		      (max (apply max (map (lambda (x) (apply max x)) history-list)) (apply max new)))) ;; max from all
	 (real-height (if incremental?
			  (- *graph-height* (* *bar-vertical-space* (- (length old-raw) 1))) ;; 1#2#3 number of "#" === (- length 1) === 2 ;; fixme: need only count the distance between the non-zero columns, but the resultant height of the columns (especially the small columns), depends on the distance between the columns. Otherwise, the top will often be an empty space, even when wholly loaded, due to rounding error
			  (truncate (/ (- *graph-height* *bar-vertical-space*) 2))))
	 (new-history-list (append (cdr history-list) (list new))))
    
    (map display (map func-draw (map (lambda (x) (func-scale x 0 max-val 0 real-height)) new-history-list)))

    (list mygraph func-read-raw func-prepare func-scale func-draw new-raw new-history-list incremental?)))






(define (mytext color str)
  (format #t "^fg(#~6,'0x)~a" color str)
  (list mytext color str))





(define (call-func-from-list lst)
  (if (null? lst)
      '()
      (begin
	(cons (apply (caar lst) (cdar lst)) (call-func-from-list (cdr lst)))))) ;; strange: (apply (caar lst) (cdar lst)) why not work: (car lst)?



(define (dzinn old-lst)
  (display "^ib(1)")
  (let ((new-lst (call-func-from-list old-lst)))
    (display "^fg()")
    (newline)
    (force-output *stdout*)

    (sleep *refresh-time*)
    (dzinn new-lst)))


(dzinn (list (list mytext *color-3* " sda[")
	     (list mygraph sda-stat diff-prepare simple-scale generate-splitted-multi-bar    (sda-stat) (make-list *number-of-bar* (list 0 0))   #f)
	     (list mytext *color-3* "] eth0[")
	     (list mygraph eth0-stat diff-prepare simple-scale generate-splitted-multi-bar    (eth0-stat) (make-list *number-of-bar* (list 0 0))   #f)
	     (list mytext *color-3* "] cpu[")
	     (list mygraph cpu-stat cpu-diff-prepare  cpu-scale  generate-incremental-multi-bar (cpu-stat) (make-list *number-of-bar* (list 0 0 0)) #t)
	     (list mytext *color-3* "]")
	     ))

