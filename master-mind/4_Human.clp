;  ---------------------------------------------
;  --- Definizione del modulo e dei template ---
;  ---------------------------------------------
(defmodule HUMAN (import MAIN ?ALL) (import GAME ?ALL) (import AGENT ?ALL) (export ?ALL))

(deffacts my-colors 
 (colors blue green red yellow orange white black purple)
)

(deftemplate new-guess-colors 
  (multislot colors (allowed-values blue green red yellow orange white black purple) (cardinality 4 4))
)

(deftemplate informations
  (multislot best-guess (allowed-values blue green red yellow orange white black purple) (cardinality 4 4))
  (slot best-guess-rp-mp (type INTEGER)) 
  (slot guess-at (type INTEGER)) 
  (multislot to-be-evaluated (allowed-values blue green red yellow orange white black purple)) 
  (slot tbe-at (type INTEGER)) 
  (slot already-operated (type INTEGER)(default 0)) 
)


;  ---------------------------------------------
;           --- Set previous step ---
;  ---------------------------------------------


(defrule set-previous-step (declare (salience 102))
  (status (step ?s)) 
  (not (previous-step  ?ps)) 
  =>
  (bind ?ps (- ?s 1))
  (assert(previous-step ?ps)) 
)


;  ---------------------------------------------
;             --- Manual guess ---
;  ---------------------------------------------
 

(defrule human-guess (declare (salience -2))
  (status (step ?s) (mode human))
  ?previous-step <- (previous-step ?)
  =>
  (printout t "   AGENT| Your guess at step " ?s crlf)
  (bind $?input (readline))
  (assert (new-guess-colors (colors (explode$ $?input)))) 
)


;  ---------------------------------------------
;            --- Automatic guess ---
;  ---------------------------------------------


(defrule new-colors-automatic-guess-info-at (declare (salience -1))
  ?ng <- (new-guess-colors (colors $?new-colors)) 
  (test (eq (length$ $?new-colors) 4)) 
  ?info <- (informations (already-operated ?already-operated))
  ?color-at <- (at ?)
  (not (try ?))
  ?previous-step <- (previous-step ?)
  (status (step ?s))
  =>
  (assert (guess (step ?s) (g $?new-colors))) 
  (printout t "   AGENT| Your guess at step " ?s crlf)
  (printout t "   AGENT| Guess colors " $?new-colors crlf)
  (retract ?ng)
  (retract ?previous-step) 
  (retract ?color-at) 
  (modify ?info (already-operated 0))
  (pop-focus)
)

(defrule new-colors-automatic-guess-info (declare (salience -1))
  (not (at))
  ?info <- (informations (already-operated ?already-operated))
  ?ng <- (new-guess-colors (colors $?new-colors)) 
  (test (eq (length$ $?new-colors) 4)) 
  (not (try ?)) 
  ?previous-step <- (previous-step ?) 
  (status (step ?s))
  =>
  (assert (guess (step ?s) (g $?new-colors))) 
  (printout t "   AGENT| Your guess at step " ?s crlf)
  (printout t "   AGENT| Guess colors " $?new-colors crlf)
  (retract ?ng) 
  (retract ?previous-step) 
  (modify ?info (already-operated 0)) 
  (pop-focus)
)

(defrule new-colors-automatic-guess-at (declare (salience -1))
  (not(informations))
  ?color-at <- (at ?)
  ?ng <- (new-guess-colors (colors $?new-colors)) 
  (test (eq (length$ $?new-colors) 4)) 
  (not (try ?)) 
  ?previous-step <- (previous-step ?) 
  (status (step ?s))
  =>
  (assert (guess (step ?s) (g $?new-colors))) 
  (printout t "   AGENT| Your guess at step " ?s crlf)
  (printout t "   AGENT| Guess colors " $?new-colors crlf)
  (retract ?ng) 
  (retract ?previous-step) 
  (retract ?color-at) 
  (pop-focus)
)

(defrule new-colors-automatic-guess (declare (salience -1))
  (not (at))
  (not (informations))
  ?ng <- (new-guess-colors (colors $?new-colors)) 
  (test (eq (length$ $?new-colors) 4)) 
  (not (try ?)) 
  ?previous-step <- (previous-step ?) 
  (status (step ?s))
  =>
  (assert (guess (step ?s) (g $?new-colors))) 
  (printout t "   AGENT| Your guess at step " ?s crlf)
  (printout t "   AGENT| Guess colors " $?new-colors crlf)
  (retract ?ng) 
  (retract ?previous-step) 
  (pop-focus)
)

;  ---------------------------------------------
;      --- Controllo se guess già fatta ---
;  ---------------------------------------------

(defrule check-if-already-asked-pass (declare (salience 90))
  ?ng <- (new-guess-colors (colors $?new-colors))
  (test (eq (length$ $?new-colors) 4)) 
  (not (try ?)) 
  (not (guess (g $?new-colors))) 
  =>
  ;(printout t "   AGENT| (During new-guess processing) Guess never asked before" crlf) 
)

(defrule check-if-already-asked-fail (declare (salience 90))
  ?ng <- (new-guess-colors (colors $?new-colors)) 
  ?ps <- (previous-step ?answer-step)
  (test (eq (length$ $?new-colors) 4)) 
  ?g <- (guess (g $?new-colors)) 
  (not (try ?))
  =>
  ;(printout t "   AGENT| (During new-guess processing) Guess already asked" crlf) 
  (retract ?ng)
  (retract ?ps)
  (assert (previous-step ?answer-step))
)


;  ---------------------------------------------
;                --- Caso 4 ---
;  ---------------------------------------------

 
(defrule 4-right-colors-wrong-order (declare (salience 100))
  (not (informations)) 
  (not (new-guess-colors))
  (status (step ?s) (mode human)) 
  (previous-step ?answer-step) 
  (guess (step ?answer-step) (g $?colors)) 
  (answer (step ?answer-step) (right-placed ?rp) (miss-placed ?mp)) 
  (test (= (+ ?rp ?mp) 4)) 
  =>
  ;(printout t "   AGENT| All colors guessed (but in the wrong order) at step: " ?answer-step crlf) 
  (assert (new-guess-colors (colors (create$)))) 
)

 
(defrule 4-right-colors-wrong-order-informations (declare (salience 100))
  (not (new-guess-colors))
  ?info <- (informations)
  (status (step ?s) (mode human)) 
  (previous-step ?answer-step) 
  (guess (step ?answer-step) (g $?colors)) 
  (answer (step ?answer-step) (right-placed ?rp) (miss-placed ?mp)) 
  (test (= (+ ?rp ?mp) 4)) 
  =>
  ;(printout t "   AGENT| All colors guessed (but in the wrong order) at step: " ?answer-step crlf) 
  (retract ?info)
  (assert (new-guess-colors (colors (create$)))) 
)

 
(defrule create-permutation (declare (salience 100))
  (new-guess-colors (colors $?new-guess-colors))
  (test (< (length$ $?new-guess-colors) 4)) 
  (status (step ?s) (mode human)) 
  (previous-step ?answer-step) 
  (guess (step ?answer-step) (g $?colors)) 
  (answer (step ?answer-step) (right-placed ?rp) (miss-placed ?mp)) 
  (test (= (+ ?rp ?mp) 4)) 
  =>
  (bind ?roll (random 1 4))
  (bind ?random-color (nth$ ?roll $?colors)) 
  (assert (try ?random-color))
)

(defrule try-permutation-success (declare (salience 100)) 
	?ng <- (new-guess-colors (colors $?new-guess-colors))
  ?t <- (try ?first-color-try)
	(test (not (member$ ?first-color-try $?new-guess-colors))) 
  (previous-step ?answer-step) 
  (answer (step ?answer-step) (right-placed ?rp) (miss-placed ?mp)) 
  (test (= (+ ?rp ?mp) 4)) 
=>
	(retract ?t) 
  (modify ?ng (colors $?new-guess-colors ?first-color-try))
)

(defrule try-permutation-fail (declare (salience 100)) 
	?ng <- (new-guess-colors (colors $?new-guess-colors))
  ?t <- (try ?first-color-try)
	(test (member$ ?first-color-try $?new-guess-colors)) 
  (previous-step ?answer-step) 
  (answer (step ?answer-step) (right-placed ?rp) (miss-placed ?mp)) 
=>
	(retract ?t) 
  (retract ?ng)
  (assert (new-guess-colors (colors $?new-guess-colors)))
)


;  ---------------------------------------------
;                --- Caso 01 ---
;  ---------------------------------------------


(defrule 01-right-colors (declare (salience 100)) 
  (not (informations))
  (previous-step ?answer-step)
  (guess (step ?answer-step) (g $?colors)) 
  (answer (step ?answer-step) (right-placed ?rp) (miss-placed ?mp)) 
  (test (<= (+ ?rp ?mp) 1)) 
  =>
  ;(printout t "   AGENT| Not enough colors were guessed right, changing the guess to the other colors. Step: " ?answer-step crlf) 
  (assert (at 1))
  (assert (new-guess-colors (colors (create$))))
)

(defrule change-whole-color-set (declare (salience 100)) 
  (not (informations))
  (previous-step ?answer-step)
  (answer (step ?answer-step) (right-placed ?rp) (miss-placed ?mp)) 
  (test (<= (+ ?rp ?mp) 1)) 
  ?color-at <- (at ?at)
  (test (<= ?at 8)) 
  (colors $?cls) 
  =>
  (bind ?color_to_check (nth$ ?at $?cls)) 
	(assert (try ?color_to_check)) 
  (bind ?new_at (+ ?at 1))
  (retract ?color-at)
  (assert (at ?new_at))
)
 
(defrule add-new-color-try (declare (salience 100))
  (not (informations))
  (previous-step ?answer-step)
  (answer (step ?answer-step) (right-placed ?rp) (miss-placed ?mp)) 
  (test (<= (+ ?rp ?mp) 1)) 
	?t <- (try ?color) 
  (guess (step ?answer-step) (g $?guess-colors))
	(test (not (member$ ?color $?guess-colors))) 
  ?ng <- (new-guess-colors (colors $?new-guess-colors))
=>
	(retract ?t) 
  (modify ?ng (colors $?new-guess-colors ?color))
)
 
(defrule remove-unnecessary-color-try (declare (salience 100))
  (not (informations))
  (previous-step ?answer-step)
  (answer (step ?answer-step) (right-placed ?rp) (miss-placed ?mp)) 
  (test (<= (+ ?rp ?mp) 1)) 
	?t <- (try ?) 
  (new-guess-colors (colors $?new-guess-colors))
  (test (eq (length$ $?new-guess-colors) 4))
=>
	(retract ?t) 
)


;  ---------------------------------------------
;                --- Caso 23 ---
;  ---------------------------------------------

 
(defrule 23-right-colors (declare (salience 100))
  (not (informations))
  (previous-step ?answer-step) 
  (guess (step ?answer-step) (g $?colors)) 
  (answer (step ?answer-step) (right-placed ?rp) (miss-placed ?mp)) 
  (or (test(= (+ ?rp ?mp) 2))
      (test(= (+ ?rp ?mp) 3))
  )
  =>
  ;(printout t "   AGENT| Enough colors guessed right to not change the whole selection, at step: " ?answer-step crlf)
  (bind ?sum (+ ?rp ?mp))
  (assert
    (informations (best-guess $?colors)
                  (best-guess-rp-mp ?sum)
                  (guess-at 1) 
                  (to-be-evaluated (create$))
                  (tbe-at 1) 
                  (already-operated 0)
    )
  )
  (assert (new-guess-colors (colors (create$))))
  (assert (at 1)) 
)

(defrule create-to-be-evaluated-list (declare (salience 100))
  (informations (best-guess-rp-mp ?sum))
  (or (test(= ?sum 2))
      (test(= ?sum 3))
  )
  ?color-at <- (at ?at)
  (test (<= ?at 8)) 
  (informations) 
  (colors $?cls) 
  =>
  (bind ?color_to_check (nth$ ?at $?cls)) 
	(assert (try ?color_to_check)) 
  (bind ?new_at (+ ?at 1))
  (retract ?color-at)
  (assert (at ?new_at))
)

 
(defrule add-try-to-be-evaluated (declare (salience 100))
  ?info <- (informations (best-guess $?best-guess-colors)(best-guess-rp-mp ?sum)(to-be-evaluated $?evaluation-colors))
  (or (test(= ?sum 2))
      (test(= ?sum 3))
  )
	?t <- (try ?color) 
	(test (not (member$ ?color $?best-guess-colors))) 
=>
	(retract ?t) 
  (modify ?info (to-be-evaluated $?evaluation-colors ?color)) 
)

 
(defrule remove-try-to-be-evaluated (declare (salience 100))
  ?info <- (informations (best-guess $?best-guess-colors)(best-guess-rp-mp ?sum)(to-be-evaluated $?evaluation-colors))
  (or (test(= ?sum 2))
      (test(= ?sum 3))
  )
	?t <- (try ?) 
  (test (eq (length$ $?evaluation-colors) 4)) 
=>
	(retract ?t) 
)

 
(defrule 23-single-color-change (declare (salience 100)) 
  ?info <- (informations (best-guess $?best-guess-colors)(best-guess-rp-mp ?sum)(guess-at ?guess-at)(to-be-evaluated $?evaluation-colors)(tbe-at ?tbe-at)(already-operated ?already-operated))
  (or (test(= ?sum 2))
      (test(= ?sum 3))
  )
  (previous-step ?answer-step)
  (answer (step ?answer-step) (right-placed ?rp) (miss-placed ?mp))
  (test (< (+ ?rp ?mp) 4)) 
  (not (try ?)) 
  ?ng <- (new-guess-colors (colors $?new-guess-colors)) 
  (test (< (length$ $?new-guess-colors) 4)) 
=>
  (bind $?new-try $?best-guess-colors) 
  (bind ?to-be-inserted (nth$ ?tbe-at $?evaluation-colors)) 
  (modify ?ng (colors (replace$ $?new-try ?guess-at ?guess-at ?to-be-inserted))) 
  (modify ?info (already-operated 1)) 
)
 
 
(defrule 23-higher-rp-mp-sum (declare (salience 100))
  ?info <- (informations (best-guess $?best-guess-colors)(best-guess-rp-mp ?sum)(guess-at ?guess-at)(to-be-evaluated $?evaluation-colors)(tbe-at ?tbe-at)(already-operated ?already-operated))
  (test (= ?already-operated 0)) 
  (previous-step ?answer-step)
  (answer (step ?answer-step) (right-placed ?rp) (miss-placed ?mp))
  (test (> ?sum (+ ?rp ?mp)))  
=>
  (bind ?new-guess-at (+ ?guess-at 1)) 
  (modify ?info (guess-at ?new-guess-at)
                (to-be-evaluated (delete$ $?evaluation-colors ?tbe-at ?tbe-at))
                (tbe-at 1) 
                (already-operated 1)) 
  (assert (new-guess-colors (colors (create$)))) 
)
 
(defrule 23-lower-rp-mp-sum (declare (salience 100))
  ?info <- (informations (best-guess $?best-guess-colors)(best-guess-rp-mp ?sum)(guess-at ?guess-at)(to-be-evaluated $?evaluation-colors)(tbe-at ?tbe-at)(already-operated ?already-operated))
  (test (= ?already-operated 0)) 
  (previous-step ?answer-step)
  (answer (step ?answer-step) (right-placed ?rp) (miss-placed ?mp)) 
  (test (< ?sum (+ ?rp ?mp))) 
  (test (< (+ ?rp ?mp) 4)) 
   
=>
  (bind ?to-be-inserted (nth$ ?tbe-at $?evaluation-colors)) 
  (bind ?new-guess-at (+ ?guess-at 1)) 
  (bind ?new-best-guess-rp-mp (+ ?rp ?mp))
  (modify ?info (best-guess (replace$ $?best-guess-colors ?guess-at ?guess-at ?to-be-inserted)) 
                (best-guess-rp-mp ?new-best-guess-rp-mp) 
                (guess-at ?new-guess-at) 
                (to-be-evaluated (delete$ $?evaluation-colors ?tbe-at ?tbe-at)) 
                (tbe-at 1) 
                (already-operated 1)) 
  (assert (new-guess-colors (colors (create$)))) 
)

(defrule 23-same-rp-mp-sum (declare (salience 100))
  ?info <- (informations (best-guess $?best-guess-colors)(best-guess-rp-mp ?sum)(guess-at ?guess-at)(to-be-evaluated $?evaluation-colors)(tbe-at ?tbe-at)(already-operated ?already-operated))
  (test (= ?already-operated 0)) 
  (previous-step ?answer-step)
  (test (neq 0 ?answer-step))
  (answer (step ?answer-step) (right-placed ?rp) (miss-placed ?mp)) 
  (test (= ?sum (+ ?rp ?mp))) 
=>
  (bind ?new-tbe-at (+ ?tbe-at 1))
  (modify ?info (tbe-at ?new-tbe-at)
                (already-operated 1)) 
  (assert (new-guess-colors (colors (create$)))) 
)