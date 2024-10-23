;;fatto inserito per semplificarci la vita siccome andando a decommentare
;;possiamo inserire il codice segreto che vogliamo
(deffacts secret-code
  ;;(secret-code (code blue green orange purple))
  ;;valore a caso che mi serve solo per far partire la generazione randomica del codice segreto
  (random)
)

;;per testare 01-right-colors usare quanto segue:
;;secret-code blue green orange purple
;;tentativo iniziale: blue black white red

;;per testare tutti i casi di 23-right-colors usare quanto segue:
;;secret-code blue green orange purple
;;tentativo iniziale: purple orange white yellow
;;black orange green yellow
;;seguire poi le slides su GoodNotes

;;per testare 4-right-colors-wrong-order usare quanto segue:
;;secret-code blue green orange purple
;;tentativo iniziale: green orange purple blue