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

(deffacts inmunocomprometido "Evaluar OR" 
	(oavc-u (objeto paciente) (atributo grupo-edad) (valor tercera-edad) (factor 0.1)) 
	(oavc-u (objeto paciente) (atributo operacion-reciente) (valor si) (factor 0.8))
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

; Combinacion AND y propagacion
(defrule pseudomonas 
	(oavc-u (objeto organismo) (atributo tincion) (valor gramneg) (factor ?f1)) 
	(oavc-u (objeto organismo) (atributo morfologia) (valor bacilos) (factor ?f2)) 
	(oavc-m (objeto paciente) (atributo es-un) (valor paciente-inmunocomprometido) (factor ?f3)) 
	(test (> (min ?f1 ?f2 ?f3) 0.2)) 
	=> 
	(bind ?certezaRegla 0.6) 
	(bind ?f (* (min ?f1 ?f2 ?f3) ?certezaRegla)) 
	(assert (oavc-u  (objeto organismo) (atributo identidad) (valor pseudomonas) (factor ?f))) 
)

; Ambas condiciones son ciertas
(defrule paciente-inmunocomprometido-1 "AMBAS" 
	(oavc-u (objeto paciente) (atributo grupo-edad) (valor tercera-edad) (factor ?f1&:(> ?f1 0.2))) 
	(oavc-u (objeto paciente) (atributo operacion-reciente) (valor si) (factor ?f2&:(> ?f2 0.2))) 
	=> 
	(bind ?f (* (max ?f1 ?f2) 1.0)) 
	(assert (oavc-m  (objeto paciente) (atributo es-un) (valor paciente-inmunocomprometido) (factor ?f))) 
)


;Solo la primera condicion es cierta
(defrule paciente-inmunocomprometido-2 "SOLO primera" 
	(oavc-u (objeto paciente) (atributo grupo-edad) (valor tercera-edad) (factor ?f1&:(> ?f1 0.2))) 
	(not (oavc-u (objeto paciente) (atributo operacion-reciente) (valor si) (factor ?f2&:(> ?f2 0.2)))) 
	=> 
	(bind ?f (* ?f1  1.0)) 
	(assert (oavc-m  (objeto paciente) (atributo es-un) (valor paciente-inmunocomprometido) (factor ?f))) 
)

;Solo la segunda es cierta
(defrule paciente-inmunocomprometido-3 "SOLO segunda" 
	(not (oavc-u (objeto paciente) (atributo grupo-edad) (valor tercera-edad) (factor ?f1&:(> ?f1 0.2)))) 
	(oavc-u (objeto paciente) (atributo operacion-reciente) (valor si) (factor ?f2&:(> ?f2 0.2))) 
	=> 
	(bind ?f (* ?f2  1.0)) 
	(assert (oavc-m  (objeto paciente) (atributo es-un) (valor paciente-inmunocomprometido) (factor ?f))) 
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deffacts hechos
    ;; (oavc-u (objeto nivel_vaso) (atributo desviacion) (valor 70) (factor 1.0))
    ;; (oavc-u (objeto nivel_vaso) (atributo incremento) (valor 17) (factor 1.0))
    ;; (oavc-u (objeto bomba) (atributo estado) (valor parado) (factor 1.0))
    ;; (oavc-u (objeto rompe_espumas) (atributo estado) (valor marcha) (factor 1.0))
    ;; (oavc-u (objeto caudal_extraccion) (atributo valor) (valor nulo) (factor 1.0))

    ;; (oavc-u (objeto nivel_vaso) (atributo desviacion) (valor 10) (factor 1.0))
    ;; (oavc-u (objeto nivel_vaso) (atributo incremento) (valor 17) (factor 1.0))
    ;; (oavc-u (objeto bomba) (atributo estado) (valor parado) (factor 1.0))		
    ;; (oavc-u (objeto rompe_espumas) (atributo estado) (valor marcha) (factor 1.0))
    ;; (oavc-u (objeto caudal_extraccion) (atributo valor) (valor nulo) (factor 1.0))

    (oavc-u (objeto nivel_vaso) (atributo desviacion) (valor 30) (factor 1.0))
    (oavc-u (objeto nivel_vaso) (atributo incremento) (valor 15) (factor 1.0))
    (oavc-u (objeto bomba) (atributo estado) (valor marcha) (factor 1.0))
    (oavc-u (objeto rompe_espumas) (atributo estado) (valor marcha) (factor 1.0))
    (oavc-u (objeto caudal_extraccion) (atributo valor) (valor bajo) (factor 1.0))
)


;Si a <= 0 -> -1; Si menor de 20 -> funcion; Else 1.0
(deffunction factor_subir(?a)
	(if(<= ?a 0)
		then -1.0
	else
		(if (< ?a 20)
			; y = 0.1x - 1.0
			then (- (* 0.1 ?a) 1.0)
		else 1.0))
)


(defrule subir
	(declare (salience 800))
	(oavc-u (objeto nivel_vaso) (atributo incremento) (valor ?ic) (factor ?f1&:(>= ?f1 0.2)))
	(test (>= (* (factor_subir ?ic) ?f1) 0.2))
	=>
	(bind ?fn (factor_subir ?ic))
	(bind ?f (* ?f1 ?fn))
	(assert (oavc-m (objeto nivel_vaso)(atributo tendencia)(valor subir)(factor ?f)))
)

(defrule A
        (oavc-u (objeto nivel_vaso) (atributo desviacion) (valor ?des &:(> ?des 50)) (factor ?f1&:(>= ?f1 0.2)))
    =>
        (bind ?f (* ?f1 1))
        (assert (oavc-m (objeto nivel_vaso) (atributo tendencia) (valor oscilar) (factor ?f)))
)

(defrule B
        (oavc-m (objeto nivel_vaso) (atributo tendencia) (valor oscilar) (factor ?f1&:(>= ?f1 0.2)))
    =>
        (bind ?f (* ?f1 0.8))
        (assert (oavc-m (objeto problema_abstracto) (atributo valor) (valor espuma) (factor ?f)))
)

(defrule C
        (oavc-m (objeto problema_abstracto) (atributo valor) (valor espuma) (factor ?f1))
        (oavc-u (objeto bomba) (atributo estado) (valor parado) (factor ?f2))
        (test (>= (min ?f1 ?f2) 0.2))
    =>
        (bind ?f (* (min ?f1 ?f2) 1))
        (assert (oavc-m (objeto problema) (atributo valor) (valor no_antiespumante) (factor ?f)))

)

(defrule D
        (not (oavc-m (objeto nivel_vaso) (atributo tendencia) (valor oscilar) (factor ?f1&:(>= ?f1 0.2))))
        (oavc-m (objeto nivel_vaso) (atributo tendencia) (valor subir) (factor ?f2&:(>= ?f2 0.2)))
    =>
        (bind ?f (* ?f2 0.9))
        (assert (oavc-m (objeto problema_abstracto) (atributo valor) (valor circulacion-difusor) (factor ?f)))
)

(defrule E
        (oavc-m (objeto problema_abstracto) (atributo valor) (valor circulacion-difusor) (factor ?f1))
        (oavc-u (objeto caudal_extraccion) (atributo valor) (valor nulo) (factor ?f2))
        (test (>= (min ?f1 ?f2) 0.2))
    =>
        (bind ?f (* (min ?f1 ?f2) 1))
        (assert (oavc-m (objeto problema) (atributo valor) (valor tapon_difusor) (factor ?f)))
)

(defrule F
        (not (oavc-m (objeto nivel_vaso)(atributo tendencia)(valor oscilar)(factor ?f1&:(>= ?f1 0.2))))
        (not (oavc-m (objeto nivel_vaso)(atributo tendencia)(valor subir)(factor ?f2&:(>= ?f2 0.2))))
    =>
        (assert (oavc-m (objeto problema) (atributo valor) (valor desconocido) (factor 0.9)))
)

(defrule imprimir
        (oavc-m (objeto problema) (atributo valor) (valor ?var) (factor ?f))

    =>
        (printout t "El sistema tiene un problema en " ?var " con una certeza de " ?f "." crlf)
)

(defrule imprimirNoProblema
        (not (oavc-m (objeto problema) (atributo valor) (valor ?var) (factor ?f)))
    =>
        (printout t "El sistema no tiene problemas" crlf)
)