(defmodule AGENT (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))

;;passo il controllo al modulo HUMAN (versione naive)
(defrule human-agent 
  (status (step ?s&:(< ?s 10))(mode human))
  =>
  (focus HUMAN)
)
 
;;passo il controllo al modulo COMPUTER (versione migliore)
(defrule computer-agent 
  (status (step ?s&:(< ?s 10))(mode computer))
  =>
  (focus COMPUTER)
)