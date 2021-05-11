; Ejercicio 3 Entrega 1 CLIPS
;Daniel López Martinez / Pablo Marcos Parra

; Lee por teclado el comportamiento del motor (correcto, se para, no arranca)
(defrule ObtenerComportamientoMotor
	(declare(salience 10000))
	=>
	(printout t "Introducir comportamiento motor (correcto/se_para/no_arranca):" crlf)
	(assert (motor comportamiento = (read T)))
) 

; Lee por teclado el valor de la inspección del fusible (correcto, roto)
(defrule ObtenerInspeccionFusible
	(declare(salience 9999))
	=>
	(printout t "Introducir inspeccion fusible(correcto/roto):" crlf)
	(assert (fusible inspeccion = (read T)))
)

; Lee por teclado el valor del indicador de la batería (int)
(defrule ObtenerValorBateria
	(declare(salience 9998))
	=>
	(printout t "Introducir valor indicador batería (int):" crlf)
	(assert (bateria indicador = (read T)))
) 

; Lee por teclado el valor del indicador del combustible (int)
(defrule ObtenerValorDeposito
	(declare(salience 9997))
	=>
	(printout t "Introducir valor indicador combustible:" crlf)
	(assert (deposito indicador = (read T)))
)

; Los siguientes 2 defrules son la primera etapa: paso de las quejas(comportamiento motor) a las causas intermedias (potencia desconectada y/o combustible en motor falso)
(defrule Quejas-CausasIntermedias1
	(motor comportamiento no_arranca)
	=>
	(assert(potencia estado desconectada)
		(motor combustible FALSE))
)

(defrule Quejas-CausasIntermedias2
	(motor comportamiento se_para)
	=>
	(assert (motor combustible FALSE))
)

; Los siguientes 3 defrules son la segunda etapa: paso de las causas intermedias y lecturas (fusible, bateria y deposito) a las causas originales (fusible roto, bateria baja, deposito vacio)
(defrule CausasIntermedias-Originales1
	(potencia estado desconectada)
	(fusible inspeccion roto)
	=>
	(assert(fusible estado fundido)
		(causa valor fusible-fundido))
)
(defrule CausasIntermedias-Originales2
	(potencia estado desconectada)
	(bateria indicador 0)
	=>
	(assert(bateria estado baja)
		(causa valor bateria-baja))
)
(defrule CausasIntermedias-Originales3
	(motor combustible FALSE)
	(deposito indicador 0)
	=>
	(assert (deposito estado vacio)
		(causa valor deposito-vacio))
)

;Imprimimos la causa/causas obtenidas por pantalla
(defrule Imprimir-Causa
	(causa valor ?x)
	=>
	(printout t "La causa es: " ?x crlf)
)
	