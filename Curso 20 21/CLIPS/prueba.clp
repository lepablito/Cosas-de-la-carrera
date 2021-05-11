;Plantilla univaluado
(deftemplate oavc-u
	(slot objeto (type SYMBOL))
	(slot atributo(type SYMBOL))
	(slot valor)
	(slot factor (type FLOAT)
	(range -1.0 +1.0))
)

;Plantilla multivaluado
(deftemplate oavc-m
	(slot objeto (type SYMBOL))
	(slot atributo(type SYMBOL))
	(slot valor)
	(slot factor (type FLOAT) (range -1.0 +1.0))
)

;Garantizar semantica de univaluados
(defrule garantizar_semantica_univaluada
	(declare (salience 9000))
	?f1 <- (oavc-u (objeto ?o1) (atributo ?a1) (valor ?v1))
	?f2 <- (oavc-u (objeto ?o1) (atributo ?a1) (valor ?v2))
	(test (neq ?f1 ?f2))
	=>
	(retract ?f2)
)

; Permitir hechos repetidos
(defrule duplicar-hechos
	(declare (salience 10000))
	=>
	(set-fact-duplication TRUE))


;Acumulacion de positivos univaluados
(defrule acumula-positivos-univaluados 
	(declare (salience 10000)) 
	?fact1 <- (oavc-u (objeto ?o) (atributo ?a) (valor ?v) (factor ?f1&:(>= ?f1 0)&:(< ?f1 1))) 
	?fact2 <- (oavc-u (objeto ?o) (atributo ?a) (valor ?v) (factor ?f2&:(>= ?f2 0)&:(< ?f2 1))) 
	(test (neq ?fact1 ?fact2)) 
	=> 
	(retract ?fact1) 
	(bind ?f3 (+ ?f1 (* ?f2 (- 1 ?f1)))) 
	(modify ?fact2 (factor ?f3))
)

;Acumulacion de positivos multivaluado
(defrule acumula-positivos-multivaluados 
	(declare (salience 10000)) 
	?fact1 <- (oavc-m (objeto ?o) (atributo ?a) (valor ?v) (factor ?f1&:(>= ?f1 0)&:(< ?f1 1))) 
	?fact2 <- (oavc-m (objeto ?o) (atributo ?a) (valor ?v) (factor ?f2&:(>= ?f2 0)&:(< ?f2 1))) 
	(test (neq ?fact1 ?fact2)) 
	=> 
	(retract ?fact1) 
	(bind ?f3 (+ ?f1 (* ?f2 (- 1 ?f1)))) 
	(modify ?fact2 (factor ?f3))
)

;Acumulacion de negativos univaluados
(defrule acumula-negativos-univaluados 
	(declare (salience 10000)) 
	?fact1 <- (oavc-u (objeto ?o) (atributo ?a) (valor ?v) (factor ?f1&:(>= ?f1 0)&:(< ?f1 1))) 
	?fact2 <- (oavc-u (objeto ?o) (atributo ?a) (valor ?v) (factor ?f2&:(>= ?f2 0)&:(< ?f2 1))) 
	(test (neq ?fact1 ?fact2)) 
	=> 
	(retract ?fact1) 
	(bind ?f3 (+ ?f1 (* ?f2 (+ 1 ?f1)))) 
	(modify ?fact2 (factor ?f3))
)

;Acumulacion de negativos multivaluado
(defrule acumula-negativos-multivaluados 
	(declare (salience 10000)) 
	?fact1 <- (oavc-m (objeto ?o) (atributo ?a) (valor ?v) (factor ?f1&:(>= ?f1 0)&:(< ?f1 1))) 
	?fact2 <- (oavc-m (objeto ?o) (atributo ?a) (valor ?v) (factor ?f2&:(>= ?f2 0)&:(< ?f2 1))) 
	(test (neq ?fact1 ?fact2)) 
	=> 
	(retract ?fact1) 
	(bind ?f3 (+ ?f1 (* ?f2 (+ 1 ?f1)))) 
	(modify ?fact2 (factor ?f3))
)

;Acumulacion de positivo y negativo univaluado
(defrule acumula-positivos-negativos-unnivaluados
	(declare (salience 10000)) 
	?fact1 <- (oavc-u (objeto ?o) (atributo ?a) (valor ?v) (factor ?f1&:(>= ?f1 0)&:(< ?f1 1))) 
	?fact2 <- (oavc-u (objeto ?o) (atributo ?a) (valor ?v) (factor ?f2&:(>= ?f2 0)&:(< ?f2 1))) 
	(test (neq ?fact1 ?fact2)) 
	=> 
	(retract ?fact1) 
	(bind ?f3 (/ (+ ?f1 ?f2)(- 1 (min (abs ?f1)(abs ?f2)))))
	(modify ?fact2 (factor ?f3))
)

;Acumulacion de positivo y negativo multivaluado
(defrule acumula-positivos-negativos-multivaluados 
	(declare (salience 10000)) 
	?fact1 <- (oavc-m (objeto ?o) (atributo ?a) (valor ?v) (factor ?f1&:(>= ?f1 0)&:(< ?f1 1))) 
	?fact2 <- (oavc-m (objeto ?o) (atributo ?a) (valor ?v) (factor ?f2&:(>= ?f2 0)&:(< ?f2 1))) 
	(test (neq ?fact1 ?fact2)) 
	=> 
	(retract ?fact1) 
	(bind ?f3 (/ (+ ?f1 ?f2)(- 1 (min (abs ?f1)(abs ?f2)))))
	(modify ?fact2 (factor ?f3))
)

;ORDINARIA 2019
(deffacts hechos
    (oavc-u (objeto nivel_vaso) (atributo desviacion) (valor 15) (factor 1.0))
    (oavc-u (objeto nivel_vaso) (atributo incremento) (valor 11) (factor 1.0))
    (oavc-u (objeto bomba) (atributo estado) (valor marcha) (factor 1.0))
    (oavc-u (objeto rompe_espumas) (atributo estado) (valor marcha) (factor 1.0))
    (oavc-u (objeto caudal_extraccion) (atributo valor) (valor bajo) (factor 1.0))
) 

(deffunction subir(?ic)
	(if(<= ?ic 0)then -1.0
		else
			(if (< ?ic 20)then (- (* ?ic 0.1) 1)
				else 1.0))
)

(defrule subirincremento
	(declare (salience 8000))
	(oavc-u (objeto nivel_vaso) (atributo incremento) (valor ?v) (factor ?f &:(>= ?f 0.2)))
	(test (>= (* (subir ?v) ?f) 0.2))
	=>
	(bind ?f1 (subir ?v))
	(bind ?fn (* ?f1 ?f))
	(assert (oavc-m (objeto nivel_vaso) (atributo tendencia) (valor subir) (factor ?fn)))
)

(defrule A
	(oavc-u (objeto nivel_vaso) (atributo desviacion) (valor ?v&: (> ?v 50)) (factor ?f &:(>= ?f 0.2)))
	=>
	(bind ?fn (* ?f 1.0))
	(assert (oavc-m (objeto nivel_vaso) (atributo tendencia) (valor oscilar) (factor ?fn)))
)
(defrule C
	(oavc-u (objeto problema_abstracto)(atributo valor)(valor espuma)(factor ?f1))
	(oavc-u (objeto bomba)(atributo estado)(valor parado)(factor ?f2))
	(test(>= (min ?f1 ?f2) 0.2))
	=>
	(bind ?fn (* (min ?f1 ?f2) 1.0))
	(assert (oavc-m (objeto problema) (atributo valor) (valor no_antiespumeante) (factor ?fn)))
)
(defrule noproblemo
	(not(oavc-m (objeto problema) (atributo valor) (valor ?var) (factor ?f)))
	=>
	(printout t "No hay problemas" crlf)
)