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

(define *real-height-splitted-bar* (truncate (/ (- *graph-height* *bar-vertical-space*) 2)))
(define *half-graph-with-border-height* (+ *real-height-splitted-bar* *vertical-border*))



(define (generate-grounded-bar x y w h color)
  ;; 
  ;;^fg(white)^p(1)^r(3x5+0-6)
  ;;    color  position
  ;;             1   --> gcpubar -s g -w 100 -h 10 -gs <<1>> -gw 2
  ;;                rectangle WIDTHxHEIGHT±X±Y
  ;; first need to install the y-axis on the bottom edge  
  (format #f "^fg(#~6,'0x)^r(~dx~d~@d~@d)"
	  color
	  w
	  h
	  x
	  (- 0 y h)));;



(define (canvas width height color)
  (string-append (set-y-zero-to-bottom)
		 (generate-grounded-bar 0 *vertical-border* width height color)
		 (format #f "^p(-~d)" width)))

(define (set-y-zero-to-bottom) ;; fixme need example
  ;; command ^p(_BOTTOM) adds 3 pixels between the elements
  ;; example: between red and green rectangle 3 pixels, between green and blue rectangle 0 pixels
  ;;  echo "^p()^fg(#FF0000)^r(10x19+0+0)^p(_BOTTOM)^fg(#00FF00)^r(10x19+0-20)^p()^fg(#0000FF)^r(10x19+0+0)"| dzen2 -h 21 -p
  "^p(_BOTTOM)^p(-3)")


(define (set-y-zero-to-center)
  "^p()")

(define (set-lock-x)
  "^p(_LOCK_X)")

(define (set-unlock-x)
  "^p(_UNLOCK_X)")


(define (create-space width)
     (format #f "^p(~d)" width))

(define (img filename)
  (format #f "^i(~a)" filename))


(define *empty-bar* (create-space (+ *bar-horizontal-space* *bar-width* )))








    






(define (top-stat)
  ;; fixme: probably better to use a different algorithm http://forums.anandtech.com/showthread.php?t=297729
  ;; fixme (if (< pcpu *ps-top-above*) (format #f "^p(~d)~a" *bar-horizontal-space* (make-string *ps-top-name-length* #\space)) ;; ^p(+1) because (generate-grounded-bar *bar-horizontal-space* ...) also add space
  (cons (runtime)
	(list-head (map match:substring (list-matches "[^ \n]+" (system-with-output-to-string "ps --no-headers -A -o pcpu,comm --sort=-pcpu" ))) (* 2 *ps-top-show*))))


(define (top-horizontal-draw func-scale x-list)
  (define (top-generator lst result)
    (if (null? lst)
	result
	(let* ((pcpu (inexact->exact (string->number (car lst))))
	       (pcpu-scalled (truncate (scale-proportional pcpu 100 (* *ps-top-name-length* *font-horizontal-size*))))
	       (comm (cadr lst)))
	  (top-generator (cddr lst)
			 (cons
			  (string-append 
			   (set-y-zero-to-bottom)
			   (generate-grounded-bar *bar-horizontal-space* *vertical-border* pcpu-scalled *bar-height* *color-0*)
			   (set-y-zero-to-center)
			   (add-color *color-3* "")
			   (format #f "^p(-~d)~a" pcpu-scalled (string-pad-right (string-cut comm 0 *ps-top-name-length*) *ps-top-name-length*)))
			  result)))))
  
  (reverse (top-generator (car x-list) '())))




(define (top-vertical-draw func-scale x-list)
  (define (top-generator lst result)
    (if (null? lst)
	result
	(let* ((pcpu (inexact->exact (string->number (car lst))))
	       (pcpu-scalled (truncate (scale-proportional pcpu 100 (* *ps-top-name-length* *font-horizontal-size*))))
	       (comm (cadr lst)))
	  (top-generator (cddr lst)
			 (cons
			  (string-append 
			   (generate-grounded-bar 0 *element-vertical-space* pcpu-scalled *bar-height* *color-0*)
			   (format #f "^p(;~@d)" (- 0 *bar-height* *font-vertical-size* *element-vertical-space*))
			   (add-color *color-3* (string-pad-right (string-cut comm 0 *ps-top-name-length*) *ps-top-name-length*)))
			  result)))))
  (append (list
	   (set-y-zero-to-bottom)
	   (set-lock-x))
	  (top-generator (car x-list) '())
	  (list (set-unlock-x)
		(create-space (+ (* *font-horizontal-size* *ps-top-name-length*) (* *bar-horizontal-space* *ps-top-show*) *bar-horizontal-space*)))))




(define (mem-stat)
  (let ((mem-info (read-lines-list "/proc/meminfo"))
	(mem-names (list "MemTotal"
			 "MemFree"
			 "Buffers"
			 "Cached"
			 "SwapTotal"
			 "SwapFree")))
    (cons (runtime)
	  (map (lambda (x) (string->number (car (map match:substring (list-matches "[0-9]+" (search-first-string-in-list mem-info x))))))
	       mem-names))))
	 
(define (mem-draw func-scale mem-info)
  (let* ((mem-list (car mem-info))
	 (mem-total   (list-ref mem-list 0))
	 (mem-free    (list-ref mem-list 1))
	 (mem-buffers (list-ref mem-list 2))
	 (mem-cached  (list-ref mem-list 3))
	 (swap-total  (list-ref mem-list 4))
	 (swap-free   (list-ref mem-list 5)))
	    
    (list
     (create-space *element-horizontal-space*)
     (car (generate-incremental-multi-bar func-scale
				     (list (list (- mem-total mem-free mem-buffers mem-cached)
					   mem-buffers
					   mem-cached
					   mem-free))))
     (create-space *element-horizontal-space*)
     (car (generate-incremental-multi-bar func-scale
				     (list (list (- swap-total swap-free)
					   swap-free))))
     (create-space *element-horizontal-space*))))

  




(define (pulseaudio-stat)
  ;; fixme: need rewrite because the function is full of circles and so eats the whole 1% of processor time (most likely it's because calling shell, pacmd and regexp), and rounding is not consistent with other design elements
  (let ((snd (string-split (system-with-output-to-string "pacmd list-sinks 0") #\newline)))
    (if (null? (search-first-string-in-list snd "muted: no"))
	(list (runtime) -1) ;; mute
	(list (runtime) (string->number (list-ref (map match:substring (list-matches "[^ %]+" (search-first-string-in-list snd "volume: 0:"))) 2)) 0 100 0 90))));; fixme need to use dB?







(define (pulseaudio-draw func-scale volume)
  
  (define (draw-circle diameter color-f)
    (format #f "^ib(1)^fg(#~6,'0x)^c(~d)"
	    color-f
	    diameter))
  
  (define (draw-circle-withsegment diameter degree color-b color-f)
    ;; ^ib(1)^c(26+360)^p(-26)^fg(#aecf96)^c(26-120)^p(-26)^fg(#000000)^c(26-60)
    ;;          circle  shift   color      segment   shift  color       segment
    (format #f "^ib(1)^fg(#~6,'0x)^co(~d)^p(-~d)^fg(#~6,'0x)^c(~d~@d)^p(-~d)^fg(#~6,'0x)^c(~d~@d)" ;; text degree  "^p(-~d)^fg(#~6,'0x)~3,' d"
	    color-b ;; background color
	    diameter ;; background full circle
	    
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
  (if (< (caar volume) 0)
      (list (add-color *color-3* " >X<")) ;; mute
      (let* ((new-volume (func-scale (caar volume) 100 90))
	     (shift (* 2 *bar-horizontal-space*))
	     (d0 *graph-height*)
	     (d1 (- d0 (* 2 shift)))
	     (d2 (- d1 (* 2 shift)))
	     (d3 (- d2 (* 2 shift)))
	     (d4 (- d3 (* 2 shift))))
	(list (draw-circle-withsegment d0 new-volume *color-4* *color-0*)
	      (format #f "^p(-~d)" (+ d1 shift))
	      (draw-circle d1 *color-4*)
	      (format #f "^p(-~d)" (+ d2 shift))
	      (draw-circle-withsegment d2 new-volume *color-4* *color-0*)
	      (format #f "^p(-~d)" (+ d3 shift))
	      (draw-circle d3 *color-4*)
	      (format #f "^p(-~d)" (+ d4 shift))
	      (draw-circle-withsegment d4 new-volume *color-4* *color-0*)
	      (format #f "^p(~d)" (+ d3 shift)))))) ;; fixme d3 or ? for shift from center of circle






(define (thermo-stat)
  ;;  (string-split (system-with-output-to-string "cat /sys/devices/platform/coretemp.*/temp*_input /sys/class/thermal/thermal_zone*/temp") #\newline));; this command call "shell" and "cat" evry refresh
  ;; (system-with-output-to-string "sensors") This command does not show all of the thermal sensors?


  ;; *list-thermal* === (... (sensor scale permissible_limit maximum_limit) ...)
  ;; result === (... (t permissible_limit maximum_limit) ...)
  (cons (runtime)
	(map (lambda (x) (list (round (/ (string->number (if (number? (car x))
							     (list-ref (string-split (car (nc "127.0.0.1" 7634)) #\|) (car x))
							     (first-line-of-file (car x))))
					 (cadr x)))
			       (caddr x)
			       (cadddr x)))
	     *list-thermal*)))



(define (thermo-draw func-scale thermo-list)
  ;; thermo-list === (... (t permissible_limit maximum_limit) ...)
  (map (lambda (x) (let ((thermo (car x)))
		     (string-append 
		      (set-y-zero-to-bottom)
		      (generate-grounded-bar *bar-horizontal-space*
					     *vertical-border*
					     *bar-width*
					     (func-scale thermo 100 *graph-height*)
					     (cond ((< thermo (cadr x))  *color-1*)   ;; permissible_limit
						   ((> thermo (caddr x)) *color-5*)   ;; maximum_limit
						   (else                  *color-0*))) ;; normal
		      (set-y-zero-to-center)
		      (format #f "~2,'0d" thermo))))
       (car thermo-list)))




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





(define (cpu-scale x in-max out-max)
  ;;                                                     ignore in-max
  (map (lambda (element) (round (scale-proportional element *cpu-max-val* out-max))) x))







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
  ;;
  ;; return list: (fs-new-list) === list of list ("filesystem" "symbol" use=42% free="32M")
  
  (let* ((fs-info (string-split (system-with-output-to-string "df -h") #\newline))
	 (column-avail 3)
	 (column-use 4))
    (cons (runtime)
	  (map (lambda (x)
		 (let ((fs-current (map match:substring (list-matches "[^ %]+" (search-first-string-in-list fs-info (car x))))))
		   (append x
			   (list (string->number (list-ref fs-current column-use))
				 (list-ref fs-current column-avail)))))
	       *list-fs*))))
    


(define (fs-draw func-scale x-list)
  (map (lambda (x)
	 (string-append
	  (set-y-zero-to-center)
	  (format "^fg(#~6,'0x) ~a~a"
		  (if (> (caddr x) *fs-limit*) *color-5* *color-3*)
		  (cadr x)
		  (string-pad (cadddr x) 5))
	  (set-y-zero-to-bottom)
	  (string-join (generate-incremental-multi-bar func-scale (list (list (caddr x) (- 100 (caddr x))))) "")))
       (car x-list))) ;; car because need only one element of history







(define (no-diff-prepare new-val old-val)
  (cdr new-val)) ;; remove timestamp


(define (fs-scale x in-max out-max)
  ;; ignore in-min ->   0
  ;; ignore in-max -> 100
  (map (lambda (element) (truncate (scale-proportional element 100 out-max))) x))

     
(define (diff-prepare new-val old-val)
  (map - (cdr new-val) (cdr old-val))) ;; "cdr" because for simple prepare function not necessary "runtime" element



(define (cpu-diff-prepare new-val old-val)
  (let* ((cpu-val (map - new-val old-val)))
    (map (lambda (x) (/ x (car cpu-val)));; (car cpu-val) === accumulation time
	 (cdr cpu-val))))





(define (truncate-scale-list x in-max out-max)
  (map (lambda (element) (truncate (scale-proportional element in-max out-max))) x))


(define (round-scale x in-max out-max)
  (round (scale-proportional x in-max out-max)))









(define (generate-incremental-particle-bar h prev-height num)
  (list
   (cond ;; function is not called if the sum of all columns is less than 1. but the first column can be null, and the other does not. if the first does not draw (or simulate) the remaining columns will move and paint over the previous colonnade, and the total width of the graph is reduced to the width of the column plus the margin 
    ((and (= h 0) (> num 0))  "") ;; do not draw a small column
    ((and (= h 0) (= num 0)) *empty-bar*) ;; simulate space+bar, because: function is not called if the sum of all columns is less than 1. but the first column can be null, and the other does not. if the first does not draw (or simulate) the remaining columns will move and paint over the previous colonnade, and the total width of the graph is reduced to the width of the column plus the margin 

    (else (generate-grounded-bar (if (= num 0) *bar-horizontal-space* (- *bar-width*)) ;; x
				 prev-height                                           ;; y
				 *bar-width*                                           ;; w
				 h                                                     ;; h
				 (cond ((= num 0) *color-0*)  ;; system 
				       ((= num 1) *color-1*)  ;; user
				       ((= num 2) *color-2*)  ;; nice
				       (else      *color-3*))))) ;; fixme need more color
   *bar-vertical-space*))




(define (generate-incremental-multi-bar func-scale x-list)
  (define (multi-bar lst previous-high num)
    (if (null? lst)
	'()
	(begin
	  (let ((res-txt-shift (generate-incremental-particle-bar (car lst) previous-high num)))
	    (cons (car res-txt-shift) (multi-bar (cdr lst) (cadr res-txt-shift) (+ num 1)))))))
  
  (let* ((max-val (max (apply max (map (lambda (x) (apply + x)) x-list)))) ;; max increment
	 (real-height (- *graph-height* (* *bar-vertical-space* (- (length (car (last-pair x-list))) 1)))) ;; 1#2#3 number of "#" === (- length 1) === 2 ;; fixme: need only count the distance between the non-zero columns (short columns are not shown), but the resultant height of the columns (especially the small columns), depends on the distance between the columns. Otherwise, the top will often be an empty space, even when wholly loaded, due to rounding error
	 (y-list (map (lambda (x) (func-scale x max-val real-height)) x-list)))


    (map (lambda (lst)
	   (if (< (apply + lst) 1)
	       *empty-bar* ;; create a emptiness, if the total height of the column is less than 1
	       (string-append (set-y-zero-to-bottom)
			      ;;(set-lock-x)
			      (string-join (multi-bar lst *vertical-border* 0) "")
			      ;;(set-unlock-x)
			      )))
	 y-list)))







(define (generate-splitted-particle-bar h num)
  (cond ;; function is not called if the sum of all columns is less than 1. but the first column can be null, and the other does not. if the first does not draw (or simulate) the remaining columns will move and paint over the previous colonnade, and the total width of the graph is reduced to the width of the column plus the margin 
   ((and (= h 0) (> num 0))  "") ;; do not draw a small column
   ;;((and (= h 0) (= num 0)) (create-space (+ *bar-horizontal-space* *bar-width* ))) ;; simulate space+bar, because: function is not called if the sum of all columns is less than 1. but the first column can be null, and the other does not. if the first does not draw (or simulate) the remaining columns will move and paint over the previous colonnade, and the total width of the graph is reduced to the width of the column plus the margin 

   (else (generate-grounded-bar (if (= num 0) *bar-horizontal-space* (- *bar-width*)) ;; x
				(if (= num 0)                                         ;; y
				    (- *half-graph-with-border-height* h)             ;; y in
				    *bar-vertical-space*)                             ;; y out
				*bar-width*                                           ;; w
				h                                                     ;; h
				(cond ((= num 0) *color-0*)      ;; in
				      (else      *color-1*)))))) ;; out




(define (generate-splitted-multi-bar func-scale x-list)
  ;; x-list only pair list: (a b)
  (define (multi-bar lst num)
    (if (null? lst)
	'()
	(cons (generate-splitted-particle-bar (car lst) num) (multi-bar (cdr lst) (+ num 1)))))
  
  (let* ((max-val (max (apply max (map (lambda (x) (apply max x)) x-list)))) ;; max from all
	 (y-list (map (lambda (x) (func-scale x max-val *real-height-splitted-bar*)) x-list))
	 (z-list (map reverse y-list))) ;; painting is in the following order: first the lower column, and then the top. Because of this early to be out (tx) and then in (rx), or the need to turn in / out -> out / in before rendering

    (map (lambda (lst)
	   (if (< (apply + lst) 1)
	       *empty-bar* ;; skip if zero hight of all bar
	       (string-append (set-y-zero-to-bottom) (string-join (multi-bar lst 0) ""))))
	 z-list)))














(define (store-history func-read-raw func-prepare func-scale func-draw old-raw history-list refresh-time old-aa)
  ;; func-read-raw function returns a timestamp and a list of values
  ;;               (1234567890 3500 1700)
  ;;
  ;; func-prepare  function takes two lists with the raw data (previous list and current list).
  ;;               function returns the difference between the previous and the current value
  ;;               for the sensors using the accumulated values (cpu) without timestamp,
  ;;               or if the sensor shows the current value only returns the current value
  ;;               without timestamp (temperature)
  ;;
  ;; func-scale    function converts a real value to a number suitable for display on the chart.
  ;;               (3000 1000) => (3 1)
  ;;
  ;; func-draw     function that generates a description of the graph for dzen
  ;;               input value = ((7 3) (3 1))
  ;;               "^fg(#efbc69)^p(1)^r(2x7+0+10)^fg(#efbc69)^p(1)^r(2x3+0+10)^fg(#efbc69)^p(1)^r(2x3+0+10)^fg(#efbc69)^p(1)^r(2x1+0+10)"
  ;;
  ;; old           the previous value (timestamp, and values)
  ;;
  ;; history-list  historical list of unscaled values
  ;;               ((6927 3174) (3000 1000))
  ;;
  ;; old-aa        old ascii-art need if function called when timestamp not overdue
  ;;               how better:
  ;;               to eat processor (generating the same rectangles)
  ;;               or
  ;;               eat-in memory (using rectangles created by the last time)?

  (if (and (< (runtime) (+ (car old-raw) refresh-time)) (not (null? old-aa)))
	  
      (begin  ;; if not timestamp overdue, then restore from the history
  	(map display old-aa)
	(list store-history func-read-raw func-prepare func-scale func-draw old-raw history-list refresh-time old-aa))
      
      (let* ((new-raw (func-read-raw)) ;; if the timestamp is overdue then update
	     (new (func-prepare new-raw old-raw))
	     (new-history-list (append (cdr history-list) (list new)))
	     (aa (func-draw func-scale new-history-list)))
	(map display aa)
	(list store-history func-read-raw func-prepare func-scale func-draw new-raw new-history-list refresh-time aa))))






(define (make-static-info str)
  ;;(list (set-y-zero-to-center) ;;  fixme
  (format #t "~a" str)
  (list make-static-info str))



(define (add-color color txt)
  (format #f "^fg(#~6,'0x)~a" color txt))




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

    (sleep *minimum-refresh-time*)
    (dzinn new-lst)))


