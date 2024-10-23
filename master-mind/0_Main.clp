;;Il main semplicemente conta il numero di tentativi fatti al fine di porre fine al gioco
;;Inoltre passa la palla una volta all'agente (colui che fa la mossa) ed una volta al game(conosce il codice segreto e restituisce il feedback(giusti/misplaced))
(defmodule MAIN (export ?ALL))

(deftemplate status (slot step) (slot mode (allowed-values human computer)));;status permette di avere il giocatore umano(per poterlo provare) o computer(fa tutto da solo)

(deffacts initial-facts
	(maxduration 10);;numero passi massimo
	;;(status (step 0) (mode human))
	(status (step 0) (mode computer))
	(agent-first);;fatto di controllo che da inizialmente il controllo all'agente
)

(defrule go-on-agent  (declare (salience 30))
   (maxduration ?d)
   (status (step ?s&:(< ?s ?d)) )
 =>
    ;(printout t crlf crlf)
    ;(printout t "vado ad agent  step" ?s)
    (focus AGENT)
)

(defrule go-on-env  (declare (salience 30))
   (maxduration ?d)
  ?f1<-	(status (step ?s&:(< ?s  ?d)))
=>
  ; (printout t crlf crlf)
  ; (printout t "vado a GAME  step" ?s)
  (focus GAME)
)

(defrule next-step  (declare (salience 20))
   (maxduration ?d)
  ?f1<-	(status (step ?s&:(< ?s  ?d)))
=>
 (bind ?s2 (+ ?s 1))
 (modify ?f1 (step ?s2))
)

(defrule game-over
	(maxduration ?d)
	(status (step ?s&:(>= ?s ?d)))
=>
	(focus GAME)
)