; coding: utf-8

;;;; dzinn.scm --- provides a way for data to dzen



;;; Copyright (C) 2012 Roman V. Prikhodchenko



;;; Author: Roman V. Prikhodchenko <chujoii@gmail.com>



;;;    This file is part of monitorus.
;;;
;;;    monitorus is free software: you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.
;;;
;;;    monitorus is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with monitorus.  If not, see <http://www.gnu.org/licenses/>.



;;; Keywords: graph plot dzen monitoring system cpu net disk top process temperature sensor dzinn monutorus



;;; Usage:

;; nice -n 19 ./monitorus.scm | dzen2 -x 705 -y 0 -w 575 -h 21 -ta l -bg black -dock -fn "Mono:size=7" &
;; while true; do cat graph.txt ; sleep 1; done | dzen2 -x 1150 -y 30 -w 100 -h 100 -bg black -ta l -dock
;; cat graph.txt | dzen2 -p



;;; History:

;; Version 0.1 was created at 2012.may.30



;;; Code:



(use-modules (ice-9 format))
(use-modules (ice-9 regex)) ;; for match:substring
(load "../battery-scheme/file-contents.scm")
(load "../battery-scheme/string.scm")
(load "../battery-scheme/system-cmd.scm")
(load "../battery-scheme/netcat.scm")
(load "../battery-scheme/minimax.scm")
(load "../battery-scheme/time.scm")


(define *stdout* (current-output-port))
(define *stderr* (current-error-port))










(define *number-of-bar* (truncate (/ (- *graph-width* *bar-horizontal-space*) (+ *bar-width* *bar-horizontal-space*)))) ;; Ideally, if (truncate fubar)===(fubar), otherwise, due to rounding at the end of the schedule will see the empty space

(define *cpu-max-val* (* 100 *cpu-quantity*)) ;; if num-of-cpu=2 => max-cpu-val=200%     the final value is also dependent on the time of accumulation (see cpu-diff-prepare)















    







(define (diff-prepare new-val old-val)
  (map - (cdr new-val) (cdr old-val))) ;; "cdr" because for simple prepare function not necessary "runtime" element



(define (cpu-diff-prepare new-val old-val)
  (let* ((cpu-val (map - new-val old-val)))
    (map (lambda (x) (/ x (car cpu-val)));; (car cpu-val) === accumulation time
	 (cdr cpu-val))))




(define (simple-scale x in-min in-max out-min out-max)
  (map (lambda (element) (truncate (scale element in-min in-max out-min out-max))) x))




(define (top-stat)
  ;; fixme: probably better to use a different algorithm http://forums.anandtech.com/showthread.php?t=297729
  (define (top-list)
    (list-head (map match:substring (list-matches "[^ \n]+" (system-with-output-to-string "ps --no-headers -A -o pcpu,comm --sort=-pcpu" ))) (* 2 *ps-top-show*)))
  

  (define (top-generator lst result)
    (if (null? lst)
	result
	(let* ((pcpu (inexact->exact (string->number (car lst))))
	       (pcpu-scalled (truncate (scale pcpu 0 100 0 (* *ps-top-name-length* *font-horizontal-size*))))
	       (comm (cadr lst)))
	  (top-generator (cddr lst)
			 (string-append result
					(if (< pcpu *ps-top-above*)
					    (format #f "^p(~d)~a" *bar-horizontal-space* (make-string *ps-top-name-length* #\space)) ;; ^p(+1) because (generate-grounded-bar *bar-horizontal-space* ...) also add space
					    (string-append 
					     (generate-grounded-bar *bar-horizontal-space* 0 pcpu-scalled *bar-height* *color-3*)
					     (format #f "^p(-~d)~a" pcpu-scalled (string-pad-right (string-cut comm 0 *ps-top-name-length*) *ps-top-name-length*)))))))))
  
  (top-generator (top-list) ""))
  



(define (mem-stat)
  (let* ((mem-info (read-lines-list "/proc/meminfo"))
	 (mem-names (list "MemTotal"
			  "MemFree"
			  "Buffers"
			  "Cached"
			  "SwapTotal"
			  "SwapFree"))
	 (mem-numbers (map (lambda (x) (string->number (car (map match:substring (list-matches "[0-9]+" (search-first-string-in-list mem-info x))))))
			   mem-names))
	 
	 (mem-total   (list-ref mem-numbers 0))
	 (mem-free    (list-ref mem-numbers 1))
	 (mem-buffers (list-ref mem-numbers 2))
	 (mem-cached  (list-ref mem-numbers 3))
	 (swap-total  (list-ref mem-numbers 4))
	 (swap-free   (list-ref mem-numbers 5))
	 (mem-graph-height (- *graph-height* (* 3 *bar-vertical-space*)))
	 (swap-graph-height (- *graph-height* (* 2 *bar-vertical-space*))))
    
    (string-append
     (generate-incremental-multi-bar (list (truncate (scale (- mem-total mem-free mem-buffers mem-cached) 0 mem-total  0 mem-graph-height))
					   (truncate (scale mem-buffers                                   0 mem-total  0 mem-graph-height))
					   (truncate (scale mem-cached                                    0 mem-total  0 mem-graph-height))
					   (truncate (scale mem-free                                      0 mem-total  0 mem-graph-height))))
     (generate-incremental-multi-bar (list (truncate (scale (- swap-total swap-free)   0 swap-total  0 swap-graph-height))
					   (truncate (scale swap-free                  0 swap-total  0 swap-graph-height)))))))
  


(define (pulseaudio-stat)
  ;; fixme: need rewrite because the function is full of circles and so eats the whole 1% of processor time (most likely it's because calling shell, pacmd and regexp), and rounding is not consistent with other design elements
  (define (draw-circle diameter color-f)
    (format #f "^ib(1)^fg(#~6,'0x)^c(~d~@d)"
	    color-f
	    diameter 360))
  
  (define (draw-circle-withsegment diameter degree color-b color-f)
    ;; ^ib(1)^c(26+360)^p(-26)^fg(#aecf96)^c(26-120)^p(-26)^fg(#000000)^c(26-60)
    ;;          circle  shift   color      segment   shift  color       segment
    (format #f "^ib(1)^fg(#~6,'0x)^c(~d~@d)^p(-~d)^fg(#~6,'0x)^c(~d~@d)^p(-~d)^fg(#~6,'0x)^c(~d~@d)" ;; text degree  "^p(-~d)^fg(#~6,'0x)~3,' d"
	    color-b ;; background color
	    diameter 360 ;; background full circle
	    
	    diameter ;; shift to left
	    color-f ;; degree color
	    diameter (- -90 degree) ;; degree segment
	    
	    diameter ;; shift to left
	    (if (> degree 100) color-f color-b) ;;  degree or hide segment
	    diameter (+ -90 degree) ;; hide segment
	    ;;diameter ;; shift to left
	    ;;color-b
	    ;;degree
	    ))
  
  ;; fixme need to use dB?
  (let ((snd (string-split (system-with-output-to-string "pacmd list-sinks 0") #\newline)))
    (if (null? (search-first-string-in-list snd "muted: no"))
	"x" ;; mute
	(let* ((volume (round (scale (string->number (list-ref (map match:substring (list-matches "[^ %]+" (search-first-string-in-list snd "volume: 0:"))) 2)) 0 100 0 90)))
	       (shift (* 2 *bar-horizontal-space*))
	       (d0 *graph-height*)
	       (d1 (- d0 (* 2 shift)))
	       (d2 (- d1 (* 2 shift)))
	       (d3 (- d2 (* 2 shift)))
	       (d4 (- d3 (* 2 shift))))
	  (string-append (draw-circle-withsegment d0 volume *color-4* *color-0*)
			 (format #f "^p(-~d)" (+ d1 shift))
			 (draw-circle d1 *color-4*)
			 (format #f "^p(-~d)" (+ d2 shift))
			 (draw-circle-withsegment d2 volume *color-4* *color-0*)
			 (format #f "^p(-~d)" (+ d3 shift))
			 (draw-circle d3 *color-4*)
			 (format #f "^p(-~d)" (+ d4 shift))
			 (draw-circle-withsegment d4 volume *color-4* *color-0*)
			 (format #f "^p(~d)" (+ d3 shift))))))) ;; fixme d3 or ? for shift from center of circle


(define (thermo-stat)
  ;;  (string-split (system-with-output-to-string "cat /sys/devices/platform/coretemp.*/temp*_input /sys/class/thermal/thermal_zone*/temp") #\newline));; this command call "shell" and "cat" evry refresh
  ;; (system-with-output-to-string "sensors") This command does not show all of the thermal sensors?
  (string-join
   (map (lambda (x) (let ((thermo
			    (round (/ (string->number (if (number? (car x))
							  (list-ref (string-split (car (nc "127.0.0.1" 7634)) #\|) (car x))
							  (first-line-of-file (car x))))
				      (cadr x)))))
		      (format #f "~a~2,'0d"
			      (generate-grounded-bar *bar-horizontal-space*
						     0
						     *bar-width*
						     (round (scale thermo 0 100 0 *graph-height*))
						     (cond ((< thermo (caddr x))  *color-1*)   ;; permissible_limit
							   ((> thermo (cadddr x)) *color-5*)   ;; maximum_limit
							   (else                  *color-0*))) ;; normal
			      thermo)))
	*list-thermal*)
   ""))

  




(define (cpu-stat)
  ;;(let ((ps-user (list-ref cpu-stat 0)) ;; user: normal processes executing in user mode
  ;;      (ps-nice (list-ref cpu-stat 1)) ;; nice: niced processes executing in user mode
  ;;      (ps-system (list-ref cpu-stat 2)) ;; system: processes executing in kernel mode
  ;;      (ps-idle (list-ref cpu-stat 3)) ;; idle: twiddling thumbs
  ;;      (ps-iowait (list-ref cpu-stat 4)) ;; iowait: waiting for I/O to complete
  ;;      (ps-irq (list-ref cpu-stat 5)) ;; irq: servicing interrupts
  ;;      (ps-softirq (list-ref cpu-stat 6))) ;; softirq: servicing softirqs)))
  
  ;; return (runtime sys user nice)
  (let ((p-s (list-head (map string->number (map match:substring (list-matches "[0-9]+" (first-line-of-file "/proc/stat"))))
			3)))
    (list (runtime)           ;; time
	  (list-ref p-s 2)    ;; sys
	  (list-ref p-s 0)    ;; user
	  (list-ref p-s 1)))) ;; nice





(define (cpu-scale x in-min in-max out-min out-max)
  ;;                                                     ignore in-max
  (map (lambda (element) (round (scale element in-min *cpu-max-val* out-min out-max))) x))







(define (eth0-stat) ;; fixme need to use wlan0, ...
  ;; return (in out)
  (list (runtime)
	(car (map string->number (map match:substring (list-matches "[0-9]+" (first-line-of-file "/sys/class/net/eth0/statistics/rx_bytes")))))   ;; rx
	(car (map string->number (map match:substring (list-matches "[0-9]+" (first-line-of-file "/sys/class/net/eth0/statistics/tx_bytes"))))))) ;; tx











(define (sda-stat) ;; fixme need to use ALL disk
  ;; /usr/src/linux/Documentation/ABI/testing/procfs-diskstats
  (let ((sda (map string->number (map match:substring (list-matches "[^ ]+" (search-first-string-in-list (read-lines-list "/proc/diskstats") *disk-sda*))))))
    (list (runtime) (list-ref sda 3) (list-ref sda 7))))


(define (swap-stat) ;; fixme: to automatically determine the partition (file?) swap
  (let ((swap (map string->number (map match:substring (list-matches "[^ ]+" (search-first-string-in-list (read-lines-list "/proc/diskstats") *disk-swap*))))))
    (list (runtime) (list-ref swap 5) (list-ref swap 9))))




(define (fs-stat)
  ;; /proc/partitions ?
  ;; /proc/mounts ?
  (let* ((fs-info (string-split (system-with-output-to-string "df -h") #\newline))
	 (column-avail 3)
	 (column-use 4)
	 (fs-new-list (map (lambda (x)
			     (let ((fs-current (map match:substring (list-matches "[^ %]+" (search-first-string-in-list fs-info (car x))))))
			       (append x
				       (list (string->number (list-ref fs-current column-use))
					     (list-ref fs-current column-avail)))))
			   *list-fs*)))
    ;; (fs-new-list) === list of list ("filesystem" "symbol" 42 "32M")

    (string-join
     (map (lambda (x)
	    (string-append
	     (format "^fg(#~6,'0x) ~a~a"
		     (if (> (caddr x) 80) *color-5* *color-3*)
		     (cadr x)
		     (string-pad (cadddr x) 5))
	     (generate-incremental-multi-bar (list (truncate (scale (caddr x)         0 100  0 *graph-height*))
						   (truncate (scale (- 100 (caddr x)) 0 100  0 *graph-height*))))))
	  fs-new-list)
     "")))
  
     


(define (generate-grounded-bar x y w h color)
  ;; 
  ;;^fg(white)^p(1)^r(3x5+0-6)
  ;;    color  position
  ;;             1   --> gcpubar -s g -w 100 -h 10 -gs <<1>> -gw 2
  ;;                rectangle WIDTHxHEIGHT±X±Y
  (format #f "^fg(#~6,'0x)^p(~@d)^r(~dx~d~@d~@d)"
	  color
	  x
	  w
	  h
	  0
	  (- (ceiling (- (/ *graph-height* 2) (/ h 2))) y)))

(define (canvas width height color)
  (string-append (generate-grounded-bar 0 0 width height color)
		 (format #f "^p(-~d)" width)))

(define (img filename)
  (format #f "^i(~a)" filename))

(define (generate-incremental-particle-bar y prev-height num)
  ;;
  ;;^fg(white)^p(1)^r(3x5+0-6)
  ;;    color  position
  ;;             1   --> gcpubar -s g -w 100 -h 10 -gs <<1>> -gw 2
  ;;                rectangle WIDTHxHEIGHT±X±Y
  (if (and (= y 0) (> num 0))
      
      (list "" prev-height) ;; do not draw small column
      
      (let* ((y-shift (- (ceiling (- (/ *graph-height* 2) (/ y 2)))
			 prev-height)))
	(list (format #f "^fg(#~6,'0x)^p(~d)^r(~dx~d~@d~@d)"
		      ;;(cond ((< y (* *graph-height* (/ 1 3))) #x777777)
		      ;;  ((< y (* *graph-height* (/ 2 3))) #xAAAAAA)
		      ;;(else #xFFFFFF))
		      (cond ((= num 0) *color-0*)  ;; system 
			    ((= num 1) *color-1*)  ;; user
			    ((= num 2) *color-2*)  ;; nice
			    (else      *color-3*)) ;; system        ;;(ash color 8)
		      (if (= num 0) *bar-horizontal-space* (- *bar-width*))
		      *bar-width*
		      y
		      0
		      y-shift)
;	      (if (= y 0) 0 (+ prev-height y *bar-vertical-space*))))))
	      (+ prev-height y *bar-vertical-space*)))))


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
  (if (and (= y 0) (> num 0))
      ""  ;; do not draw small column
      (let* ((y-shift (if (= num 0)
			  (- 0 (floor (/ (+ y *bar-vertical-space*) 2)))    ;; in
			  (+ 0 (ceiling (/ (+ y *bar-vertical-space*) 2)))))) ;; out
	
	(format #f "^fg(#~6,'0x)^p(~d)^r(~dx~d~@d~@d)"
		;;(cond ((< y (* *graph-height* (/ 1 3))) #x777777)
		;;  ((< y (* *graph-height* (/ 2 3))) #xAAAAAA)
		;;(else #xFFFFFF))
		(cond ((= num 0) *color-1*)  ;; in   ;; fixme: strange color1 for 0; and color0 for 1;
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














(define (make-graph func-read-raw func-prepare func-scale func-draw old-raw history-list incremental?)
  ;; func-read-raw function returns a time stamp and a list of values
  ;;
  ;; func-prepare  function takes two lists with the raw data (previous list and current list).
  ;;               function returns the difference between the previous and the current value
  ;;               for the sensors using the accumulated values (cpu),
  ;;               or if the sensor shows the current value only returns the current value (temperature).
  ;;
  ;; func-scale    function converts a real value to a number suitable for display on the chart.
  ;;               (3000 1000) => (3 1)
  ;;
  ;; func-draw     function that generates a description of the graph for dzen
  ;;               (input value = (3 1))
  ;;               "^fg(#efbc69)^p(1)^r(2x3+0+10)^fg(#efbc69)^p(1)^r(2x1+0+10)"
  ;;
  ;; old           the previous value (time stamp, and values)
  ;;
  ;; history-list  historical list of unscaled values
  ;;               ((6927 3174) (3000 1000))
  ;;
  ;; incremental?  mode: incremental graph (#t) or split graph (#f)
  ;;

  (let* ((new-raw (func-read-raw))
	 (new (func-prepare new-raw old-raw))
	 (max-val (if incremental?
		      (max (apply max (map (lambda (x) (apply + x)) history-list)) (apply + new)) ;; max inccrement
		      (max (apply max (map (lambda (x) (apply max x)) history-list)) (apply max new)))) ;; max from all
	 (real-height (if incremental?
			  (- *graph-height* (* *bar-vertical-space* (- (length old-raw) 1))) ;; 1#2#3 number of "#" === (- length 1) === 2 ;; fixme: need only count the distance between the non-zero columns (short columns are not shown), but the resultant height of the columns (especially the small columns), depends on the distance between the columns. Otherwise, the top will often be an empty space, even when wholly loaded, due to rounding error
			  (truncate (/ (- *graph-height* *bar-vertical-space*) 2))))
	 (new-history-list (append (cdr history-list) (list new))))
    
    (map display (map func-draw (map (lambda (x) (func-scale x 0 max-val 0 real-height)) new-history-list)))

    (list make-graph func-read-raw func-prepare func-scale func-draw new-raw new-history-list incremental?)))






(define (make-text color str)
  (format #t "^fg(#~6,'0x)~a" color str)
  (list make-text color str))


(define (make-dynamic-text color func)
  (format #t "^fg(#~6,'0x)~a" color (func))
  (list make-dynamic-text color func))




(define (call-func-from-list lst)
  (if (null? lst)
      '()
      (begin
	(cons (apply (caar lst) (cdar lst)) (call-func-from-list (cdr lst)))))) ;; strange: (apply (caar lst) (cdar lst)) I wonder why such an algorithm does not work: (car lst)? eval?



(define (dzinn old-lst)
  (display "^ib(1)")
  (let ((new-lst (call-func-from-list old-lst)))
    (display "^fg()")
    (newline)
    (force-output *stdout*)

    (sleep *refresh-time*)
    (dzinn new-lst)))




