;;Implementa le regole del gioco
;;posso esportare solo guess e answer anche se altri moduli importano ?ALL da GAME
(defmodule GAME (import MAIN ?ALL) (export deftemplate guess answer))

(deftemplate secret-code
  ;;codice segreto
	(multislot code (allowed-values blue green red yellow orange white black purple) (cardinality 4 4))
)

(deftemplate guess;;i nostri tentativi (step + guess)
	(slot step (type INTEGER))
  ;;g = guess
	(multislot g (allowed-values blue green red yellow orange white black purple) (cardinality 4 4))
)

(deftemplate answer;;rapprensenta il feedback ottenuto allo step n-esimo
	(slot step (type INTEGER));;intero usato per matchare ad ogni guess il feedback corretto
	(slot right-placed (type INTEGER))
	(slot miss-placed (type INTEGER))
)

(deffacts my-colors
 (colors blue green red yellow orange white black purple)
)

;;se la mia guess è uguale al secret-code allora ho vinto
(defrule check-mate (declare (salience 100))
  (status (step ?s))
  ?f <- (guess (step ?s) (g ?k1 ?k2 ?k3 ?k4))
  (secret-code (code ?k1 ?k2 ?k3 ?k4))
  =>
  (printout t "GAME| You have discovered the secrete code!" crlf)
  (retract ?f)
  (halt) 
)
;;non ho idea di cosa faccia, credo inizializzi la answer per il turno corrente
(defrule prepare-answer
   (status (step ?s))
   (guess (step ?s))
=>
   (assert (answer (step ?s) (right-placed 0) (miss-placed 0)))
)      

;;controllo se ho ripetuto più colori uguali e nel caso ritraggo la mia guess e ripasso il controllo ad agent(non avrò conteggio rigt/misplaced e perdo questo turno)
(defrule check-repeated-colors (declare (salience 100))
  (status (step ?s))
  ?g <- (guess (step ?s) (g $?prima ?k $?durante ?k $?dopo))
=>
  (retract ?g)
  (pop-focus)
)

;;(suppongo) Si attiva tante volte quanti sono i misplacement totali
(defrule check-miss-placed
  (status (step ?s))
  (secret-code (code $?prima ?k $?dopo));;il mio secret code lo "spezzo" in prima, valore, dopo
  (guess (step ?s) (g $?prima2 ?k $?dopo2));;la mia guess la "spezzo" in prima, valore, dopo
  (test (neq (length$ $?prima2) (length$ $?prima)));;se il numero valori precedenti a k della guess sono != da quelli del secret-code
  (test (neq (length$ $?dopo2) (length$ $?dopo)));;se il numero valori successivi a k della guess sono != da quelli del secret-code
=>
  (bind ?new (gensym*))
  (assert (missplaced ?new));;asserisco di avere un misplaced nella mia guess
)

;;conteggio dei misplace (si attiverà tante volte quanti sono i missplaced in WM)
(defrule count-missplaced
  (status (step ?s))
  ?a <- (answer (step ?s) (miss-placed ?mp));;mi interessa sapere qual è la mia answer in modo da valorizzare con la bind i misplaced
  ?m <- (missplaced ?);;qualsiasi misplaced ci sia in WM mi va bene siccome saranno SOLO quelli dello step corrente
=>
  (retract ?m);;ritraggo il misplaced
  (bind ?new-mp (+ ?mp 1));;prendo il conteggio dei misplaced finora ottenuti dalla mia answer e lo incremento di 1
  (modify ?a (miss-placed ?new-mp));;modifico il valore misplaced della mia answer con il valore corretto appena calcolato
)

;;(suppongo) Si attiva tante volte quanti sono i right-placement totali
(defrule check-right-placed
  (status (step ?s))
  (secret-code (code $?prima ?k $?dopo));;il mio secret code lo "spezzo" in prima, valore, dopo
  (guess (step ?s) (g $?prima2  ?k $?dopo2));;la mia guess la "spezzo" in prima, valore, dopo
  (test (eq (length$ $?prima2) (length$ $?prima)));;se il numero valori precedenti a k della guess sono = a quelli del secret-code
  (test (eq (length$ $?dopo2) (length$ $?dopo)))   ;;se il numero valori successivi a k della guess sono = a quelli del secret-code
=>
  (bind ?new (gensym*))
  (assert (rightplaced ?new));;asserisco di avere un rightplaced nella mia guess
)

;;conteggio dei right-placed (si attiverà tante volte quanti sono i rightplaced in WM)
(defrule count-rightplaced
  (status (step ?s))
  ?a <- (answer (step ?s) (right-placed ?rp) (miss-placed ?mp));;mi interessa sapere qual è la mia answer in modo da valorizzare con la bind i right-placed
  ?r <- (rightplaced ?);;qualsiasi rightplaced ci sia in WM mi va bene siccome saranno SOLO quelli dello step corrente
=>
  (retract ?r);;ritraggo il misplaced
  (bind ?new-rp (+ ?rp 1));;prendo il conteggio dei right-placed finora ottenuti dalla mia answer e lo incremento di 1
  (modify ?a (right-placed ?new-rp));;modifico il valore right-placed della mia answer con il valore corretto appena calcolato
)

;;stampa dei right-placed e misplaced
(defrule for-humans (declare (salience -10))
  (status (step ?s) (mode human))
  (answer (step ?s) (right-placed ?rp) (miss-placed ?mp)) 
=>
   (printout t "GAME| Right placed " ?rp " missplaced " ?mp crlf)
)  

;;stampa game over quando il turno eccede il numero massimo di turni possibili
(defrule for-humans-gameover (declare (salience -15))
  (status (step ?s));;(mode human));;commentato altrimenti non mi fa la stampa del game over nel caso computer
  (maxduration ?d&:(>= ?s ?d))
  (secret-code (code $?code))
=>
   (printout t "GAME| GAME OVER!! " crlf)
   (printout t "GAME| The secret code was: " $?code crlf)
)  

;;generazione del codice segreto((suppongo) inizialmente vuoto)
(defrule random-start (declare (salience 100))
	(random)
	(not (secret-code (code $?)))
=>
	(assert (secret-code (code (create$))))
)

;;una volta che ho il codice segreto(in questo caso con un numero di elementi < 4) genero un colore random e lo vado ad aggiungere ad un try che controllerò nella regola sotto
(defrule random-code (declare (salience 100))
	(random)
	(colors $?cls)
	(secret-code (code $?colors));;prendo i colori già inseriti nel codice segreto
	(test (neq (length$ $?colors) 4));;controllo di averne un numero < 4
=>
	(bind ?roll (random 1 8));;allora genero un numero random tra 1 e 8 (possibili colori)
	(bind ?c-sym (nth$ ?roll $?cls));;nth restituisce lo specifico field (in questo caso un colore, quello pari a roll numero random estratto) se esiste altrimenti nil
	(assert (try ?c-sym));;asserisco un tentativo di aggiunta colore con try
)

;;controllo se posso aggiungere il colore try alla lista dei colori del codice segreto
(defrule try-new-color-yes (declare (salience 100))
	(random)
	?s <- (secret-code (code $?colors));;?s diventa quello che per il momento è il mio codice segreto
	(test (neq (length$ $?colors) 4));;controllo che il codice segreto non abbia già 4 colori
	?t <- (try ?c-sym);;?t diventa il mio try, tentativo di aggiunta di un colore
	(test (not (member$ ?c-sym $?colors)));;se il colore che voglio aggiungere ?c-sym NON è nella lista dei colori di secret-code
=>
	(retract ?t);;allora tolto il fatto ?t, try precedentemente asserito
	(modify ?s (code $?colors ?c-sym));;modifico il codice segreto aggiungendo il colore try (?c-sym)
  ;;STAMPA AGGIUNTA DA ME PER DEBUGGING
  (printout t "GAME| secret-code:" $?colors ?c-sym crlf)
)

(defrule try-new-color-no (declare (salience 100))
	(random)
	?s <- (secret-code (code $?colors));;?s diventa quello che per il momento è il mio codice segreto
	(test (neq (length$ $?colors) 4));;controllo che il codice segreto non abbia già 4 colori
	?t <- (try ?c-sym);;?t diventa il mio try, tentativo di aggiunta di un colore
	(test (member$ ?c-sym $?colors));;se il colore del try che voglio aggiungere ?c-sym APPARTIENE alla lista dei colori di secret-code
=>
	(retract ?t);;ritraggo il try
	(retract ?s);;ritraggo il secred code
	(assert (secret-code (code $?colors)));;asserisco il secret-code con i colori precedentemente definiti
)