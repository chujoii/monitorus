#!/usr/bin/guile -s
!#
; coding: utf-8

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



;;; Keywords: graph plot dzen monitoring system cpu net disk top process temperature sensor



;;; Usage:

;; nice -n 19 ./monitorus.scm | dzen2 -x 705 -y 0 -w 575 -h 21 -ta l -bg '#3f3f3f' -dock -fn "Mono:size=7" &



;;; History:

;; Version 0.1 was created at 2012.may.30



;;; Code:



;; ---------------------- start config ----------------------

(define *refresh-time* 3) ;; in seconds

(define *graph-width* 61) ;; (+ (* (/ time-interval-that-you-want-to-see-from-the-chart *refresh-time*) (+ *bar-width* *bar-horizontal-space*)) *bar-horizontal-space*) =for-example= (+ (* (/ 60 3) (+ 2 1)) 1) = 61
(define *graph-height* 19) ;; check *bar-vertical-space*
(define *bar-width* 2)
(define *bar-height* 2)
(define *bar-horizontal-space* 1)
(define *bar-vertical-space* 1) ;; check *graph-height*
(define *font-horizontal-size* 5)

;; to emulate the frame around the graph, you can set 
;; the height of the graph *graph-height* is less than the actual height of the graph in dzen
;; For example, if real_dzen_height = 21 (dzen2 ... -h 21 ...), then set the *graph-height* = 19 for a single pixel frame
;; (define *horizontal-border* 0)
;; (define *vertical-border* 0)

;; if *graph-height* is odd, then set *bar-vertical-space* to odd
;; if *graph-height* is even, then set *bar-vertical-space* to even
;; because:
;;
;; for splitted graph with *bar-vertical-space* = even (for example 0):
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
;; for splitted graph with *bar-vertical-space* = odd (for example 1):
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
;; (C *bar-vertical-space*=2, but not 1)



(define *ps-top-show* 2)  ;; number of top process to show
(define *ps-top-above* 5) ;; show only processes above (in %) ;; fixme: need to use
(define *ps-top-name-length* 8) ;; maximum length of process name

(define *cpu-quantity* 2) ;; fixme: it is necessary to calculate automatically

(define *list-thermal*
  ;; (sensor scale permissible_limit maximum_limit)
  ;; if sensor = number - number of fields in the results of the hddtemp's output
  ;;         for example: "nc localhost 7634" -> "|/dev/sda|mysuperdisk|47|C|" ->
  ;;                                             0       1       2     >3<  4
  ;; if sentor = "string" - file that contains the number
  (list (list "/sys/devices/platform/coretemp.0/temp2_input" 1000 50 100)
	(list "/sys/devices/platform/coretemp.0/temp3_input" 1000 50 100)
	(list "/sys/class/thermal/thermal_zone0/temp"        1000 50 100)   
	(list "/sys/class/thermal/thermal_zone1/temp"        1000 50 100)
	(list "/sys/class/thermal/thermal_zone2/temp"        1000 50 100)
	(list "/sys/class/thermal/thermal_zone3/temp"        1000 50 100)
	(list 3                                                 1 50  60)
	(list "/sys/class/thermal/thermal_zone4/temp"        1000 50 100)))


(define *disk-sda* "sda")
(define *disk-swap* "sda2")

(define *list-fs*
  (list (list "/dev/sda1" "b")
	(list "rootfs"    "/")
	(list "/dev/sda4" "h")))


;;                                    cpu    eth,sda
(define *color-0* #xb17e37) ;; orange system out
(define *color-1* #x439595) ;; aqua   user   in
(define *color-2* #x357d35) ;; green  nice
(define *color-3* #x999999) ;; gray   text
(define *color-4* #x000000) ;; black  background
(define *color-5* #xffffff) ;; white  limit

;; ---------------------- end config ----------------------


(load "dzinn.scm")



(dzinn 
 (list 
  (list make-text *color-3* "-[cpu")
  (list make-text *color-3* (canvas *graph-width* *graph-height* *color-4*))
  (list make-graph cpu-stat cpu-diff-prepare cpu-scale generate-incremental-multi-bar (cpu-stat) (make-list *number-of-bar* (list 0 0 0 0)) #t)


  
  (list make-text *color-3* "]-[eth0")
  (list make-text *color-3* (canvas *graph-width* *graph-height* *color-4*))
  (list make-graph eth0-stat diff-prepare simple-scale generate-splitted-multi-bar (eth0-stat) (make-list *number-of-bar* (list 0 0 0)) #f)


  
  (list make-text *color-3* "]-[sda")
  (list make-text *color-3* (canvas *graph-width* *graph-height* *color-4*))
  (list make-graph sda-stat diff-prepare simple-scale generate-splitted-multi-bar (sda-stat) (make-list *number-of-bar* (list 0 0 0)) #f)



  ;; disabled because usually do not use swap
  ;;(list make-text *color-3* "]-[swap")
  ;;(list make-text *color-3* (canvas *graph-width* *graph-height* *color-4*))
  ;;(list make-graph swap-stat diff-prepare simple-scale generate-splitted-multi-bar (swap-stat) (make-list *number-of-bar* (list 0 0 0)) #f)


  
  ;; ============================== danger! eat cpu
  (list make-text *color-3* "]-[fs")
  (list make-text *color-3* (canvas (+ (* (+ (* *font-horizontal-size* 7) *bar-horizontal-space* *bar-width* ) (length *list-fs*)) *bar-horizontal-space*) *graph-height* *color-4*))  ;; 7=(+ 1 5 1) 1=space 5=Available 1=fs-char ; fixme last *bar-horizontal-space* ??
  (list make-dynamic-text *color-3* fs-stat)



  ;; ============================== danger! eat cpu
  (list make-text *color-3* "]-[top")
  (list make-text *color-3* (canvas (+ (* *font-horizontal-size* *ps-top-name-length* *ps-top-show*) (* *bar-horizontal-space* *ps-top-show*) *bar-horizontal-space*) *graph-height* *color-4*)) ;; fixme (+ ... *bar-horizontal-space*) very strange
  (list make-dynamic-text *color-3* top-stat)


  
  (list make-text *color-3* "]-[mem")
  (list make-text *color-3* (canvas (+ (* *bar-width* 2) (* *bar-horizontal-space* 3)) *graph-height* *color-4*))
  (list make-dynamic-text *color-3* mem-stat)


  
  (list make-text *color-3* "]-[t")
  (list make-text *color-3* (canvas (+ (* (+ (* *font-horizontal-size* 2) *bar-width* *bar-horizontal-space*) (length *list-thermal*)) *bar-horizontal-space*) *graph-height* *color-4*)) ;; fixme (+ ... *bar-horizontal-space*) very strange
  (list make-dynamic-text *color-3* thermo-stat)


  
  ;; ============================== danger! eat many cpu and "pacmd ?" write information to disk evry refresh!
  ;; (list make-text *color-3* "]-[snd")
  ;; (list make-text *color-3* (canvas *graph-height* *graph-height* *color-4*))
  ;; (list make-dynamic-text *color-3* pulseaudio-stat)


  
  (list make-text *color-3* "]-")
  ))




