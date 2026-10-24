// Integrantes:
// Angel de Jesus Fuentes Balderrama
// Angel Osvaldo Walle Vera
// Cristian Eduardo Sifuentes Hernandez
// Erik Daniel Izaguirre Perez
// Erik Darien Cedillo Garza
// Jeshil Alejandro Arrieta Quintero
// Omar Emiliano Herrera Fanjon

// Grupo TSU D-76236   23 de Septiembre de 2026



// Funciones graficas para la interfaz (GUI)
Funcion ImprimirTitulo
	Escribir "  ____      _            _           _                 "
	Escribir " / ___|__ _| | ___ _   _| | __ _  __| | ___  _ __ __ _ "
	Escribir "| |   / _` | |/ __| | | | |/ _` |/ _` |/ _ \| `__/ _` |"
	Escribir "| |__| (_| | | (__| |_| | | (_| | (_| | (_) | | | (_| |"
	Escribir " \____\__,_|_|\___|\__,_|_|\__,_|\__,_|\___/|_|  \__,_|"
	Escribir ""
FinFuncion

Funcion ImprimirOpciones
	Escribir "  Selecciona una opción..."
	Escribir "+--------------------------------------------------------+"
	Escribir "|    1. Trabajo de una fuerza                            |"
	Escribir "|    2. Trabajo resultante                               |"
	Escribir "|    3. Energía cinética                                 |"
	Escribir "|    4. Energía potencial                                |"
	Escribir "|    5. Teorema del trabajo y la energía                 |"
	Escribir "|    6. Conservación de la energía                       |"
	Escribir "|    7. Conservación con fricción                        |"
	Escribir "|    8. Potencia                                         |"
	Escribir "|    9. Salir                                            |"
	Escribir "+--------------------------------------------------------+"
FinFuncion

Funcion ImprimirLinea(lon, c1, c2, c3)
	i <- 0
	Escribir c1 Sin Saltar
	Repetir
		Escribir c2 Sin Saltar
		i <- i + 1
	Hasta Que i = lon
	Escribir c3
FinFuncion

Funcion ImprimirPregunta(pregunta)
	Definir longitudPregunta Como Entero
	Definir i Como Entero

	longitudPregunta <- Longitud(pregunta)

	ImprimirLinea(longitudPregunta + 4, "~", "~", "~")

	Escribir "|  " Sin Saltar
	Escribir pregunta Sin Saltar
	Escribir "  |"

	ImprimirLinea(longitudPregunta + 4, "~", "~", "~")
FinFuncion

Funcion ImprimirSubtitulo(subtitulo)
	Definir longitudPregunta Como Entero
	Definir i Como Entero
	
	longitudSubtitulo <- Longitud(subtitulo)
	
	ImprimirLinea(longitudPregunta + 8, "[", "=", "]")
	
	Escribir "[    " Sin Saltar
	Escribir subtitulo Sin Saltar
	Escribir "    ]"
	
	ImprimirLinea(longitudPregunta + 8, "[", "=", "]")
FinFuncion

Funcion ImprimirDialogo(dialogo)
	Definir longitudDialogo Como Entero
	Definir i Como Entero
	
	longitudDialogo <- Longitud(dialogo)
	
	ImprimirLinea(longitudDialogo + 4, "·", "·", "·")
	
	Escribir ":  " Sin Saltar
	Escribir dialogo Sin Saltar
	Escribir "  :"
	
	ImprimirLinea(longitudDialogo + 4, "·", "·", "·")
FinFuncion



// Subprocesos de cada formula
SubProceso trabajo <- CalcularTrabajo(F, s, theta)
	trabajo <- F * s * cos(theta * PI / 180)
FinSubProceso

SubProceso trabajo <- CalcularTrabajoResultante(F, micro, m, s)
	trabajo <- (F - micro * m * GRAVEDAD) * s
FinSubProceso

SubProceso energia <- CalcularEnergiaCinetica(m, v)
	energia <- 0.5 * m * v * v
FinSubProceso

SubProceso energia <- CalcularEnergiaPotencial(m, h)
	energia <- masa * GRAVEDAD * h
FinSubProceso

SubProceso fuerza <- CalcularTeoremaTrabajoEnergia(m, vo, vf, s)
	fuerza <- (0.5 * m * vo * vf - 0.5 * m * vf * vo) / s
FinSubProceso

SubProceso velocidad <- CalcularConservacionEnergia(h)
	velocidad <- Raiz(2 * GRAVEDAD * h)
FinSubProceso

SubProceso velocidadFinal <- CalcularConservacionFriccion(m, h, f, s)
	velocidadFinal <- Raiz(2 * GRAVEDAD * h - (2 * f * s) / (m))
FinSubProceso

SubProceso wats <- CalcularPotenciaVatios(W, t)
	wats <- W / t
FinSubProceso

SubProceso hp <- CalcularPotenciaCaballos(F, v)
	hp <- F * v
FinSubProceso

Funcion retorno <- Ejecutar(opcion_interfaz)
	retorno <- 0
	
	// Sera el titulo de cada formula
	Definir subtitulo Como Cadena
	
	// Puede ser y, n, Y, N, o vacio
	Definir respuesta Como Caracter
	
	// CONSTANTES
	Definir GRAVEDAD Como Real
	GRAVEDAD <- 9.8
	
	Segun opcion_interfaz Hacer
		0:
			retorno <- 1
		1:
			subtitulo <- "TRABAJO DE UNA FUERZA"

			Definir F Como Real
			Definir s Como Real
			Definir theta como Real
			Definir W Como Real

			Repetir
				ImprimirPregunta("Ingresa la magnitud de la fuerza (en Newtons):")
			    Leer F

				ImprimirPregunta("Ingresa la distancia / desplazamiento (en metros):")
		    	Leer s

				ImprimirPregunta("Ingresa el ángulo entre la fuerza y el movimiento (en grados):")
		    	Leer theta

		    	W <- CalcularTrabajo(F, s, theta)
				
				Borrar Pantalla
				ImprimirTitulo
				
		    	Escribir "El trabajo de la fuerza es de: ", W, " J."
				Escribir ""
				
				ImprimirDialogo("¿Quieres hacer otro cálculo? (Y / N)")
		    	Leer respuesta

			Hasta Que respuesta = "n" o respuesta = "N"
		2:
			subtitulo <- "TRABAJO RESULTANTE"
			
			Definir F Como Real
			Definir micro Como Real
			Definir m Como Real
			Definir s Como Real
			
			Definir W Como Real

			Repetir
				ImprimirPregunta("Ingresa la magnitud de la fuerza (en Newtons):")
				Leer F
				
				ImprimirPregunta("Ingresa el valor del coeficiente de fricción (valores de entre 0 y 1):")
				Leer micro
				
				ImprimirPregunta("Ingresa la masa (en kilogramos):")
				Leer m
				
				ImprimirPregunta("Ingresa la distancia (en metros):")
				Leer s
				
				W <- CalcularTrabajoResultante(F, micro, m, s)

				Borrar Pantalla
				ImprimirTitulo
				
				Escribir "El trabajo resultante es de: ", trabajo, " J."

				ImprimirDialogo("¿Quieres hacer otro cálculo? (Y / N)")
		    	Leer respuesta

			Hasta Que respuesta = "n"
		3:
			subtitulo <- "ENERGÍA CINÉTICA"
			
			// Entradas
			Definir m Como Real
			Definir v Como Real
			
			// Resultado
			Definir energia Como Real
			
			Repetir
				ImprimirPregunta("Ingresa la masa (en kilogramos):")
				Leer m
				
				ImprimirPregunta("Ingresa la velocidad constante (en metros por segundo):")
				Leer v
				
				energia <- CalcularEnergiaCinetica(m, v)
				
				Borrar Pantalla
				ImprimirTitulo
				
				Escribir "La energía cinética es de: ", energia, " J."
				
				ImprimirDialogo("¿Quieres hacer otro cálculo? (Y / N)")
		    	Leer respuesta
				
			Hasta Que respuesta = "n"
		4:
			subtitulo <- "ENERGÍA POTENCIAL"
			
			// Entradas
			Definir m Como Real
			Definir h Como Real
			
			// Resultado
			Definir energia Como Real
			
			Repetir
				ImprimirPregunta("Ingresa la masa (en kilogramos):")
				Leer m
				
				ImprimirPregunta("Ingresa la altura (en metros):")
				Leer h
				
				energia <- CalcularEnergiaPotencial(m, h)
				
				Borrar Pantalla
				ImprimirTitulo
				
				Escribir "La energía potencial es de: ", energia, " J."
				
				ImprimirDialogo("¿Quieres hacer otro cálculo? (Y / N)")
		    	Leer respuesta
				
			Hasta Que respuesta = "n"
		5:
			subtitulo <- "TEOREMA DEL TRABAJO Y LA ENERGÍA"
			
			// Entradas
			Definir m Como Real
			Definir vo Como Real
			Definir vf Como Real
			Definir s Como Real
			
			// Resultado
			Definir F Como Real
			
			Repetir
				ImprimirPregunta("Ingresa la masa (en kilogramos):")
				Leer m
				
				ImprimirPregunta("Ingresa la velocidad inicial (en metros por segundo):")
				Leer vo
				
				ImprimirPregunta("Ingresa la velocidad final (en metros por segundo):")
				Leer vf
				
				ImprimirPregunta("Ingresa el desplazamiento (en metros):")
				Leer s
				
				F <- CalcularTeoremaTrabajoEnergia(m, vo, vf, s)
				
				Borrar Pantalla
				ImprimirTitulo
				
				Escribir "La fuerza es de: ", F, " N."
				
				ImprimirDialogo("¿Quieres hacer otro cálculo? (Y / N)")
		    	Leer respuesta
				
			Hasta Que respuesta = "n"
		6:
			subtitulo <- "CONSERVACIÓN DE LA ENERGÍA"
			
			// Entradas
			Definir h Como Real
			
			// Resultado
			Definir v Como Real
			
			Repetir
				ImprimirPregunta("Ingresa la altura (en metros):")
				Leer h
				
				v <- CalcularConservacionEnergia(h)
				
				Borrar Pantalla
				ImprimirTitulo
				
				Escribir "La velocidad es de: ", v, " m/s."
				
				ImprimirDialogo("¿Quieres hacer otro cálculo? (Y / N)")
		    	Leer respuesta
				
			Hasta Que respuesta = "n"
		7:
		    subtitulo <- "CONSERVACIÓN CON FRICCIÓN"
			
			// Entradas
			Definir m Como Real
			Definir h Como Real
			Definir f Como Real
			Definir s Como Real
			
			// Resultado
			Definir vf Como Real
			
			Repetir
				ImprimirPregunta("Ingresa la masa (en kilogramos):")
				Leer m
				
				ImprimirPregunta("Ingresa la altura (en metros):")
				Leer h
				
				ImprimirPregunta("Ingresa la fuerza (en Newtons):")
				Leer f
				
				ImprimirPregunta("Ingresa el desplazamiento (en metros):")
				Leer s
				
				vf <- CalcularConservacionFriccion(m, h, f, s)
				
				Borrar Pantalla
				ImprimirTitulo
				
				Escribir "La velocidad final es de: ", vf, " m/s."
				
				ImprimirDialogo("¿Quieres hacer otro cálculo? (Y / N)")
		    	Leer respuesta
				
			Hasta Que respuesta = "n"
		8:
			ImprimirDialogo("Opcion no disponible ::(")
		9:
			retorno <- 2
		De Otro Modo:
			retorno <- 3
	FinSegun
FinFuncion

Algoritmo CalculadoraDeFormulas
	Definir opcion_interfaz Como Entero
	Definir retorno como Entero
	
	Repetir
		// Mostrar panel de opciones
		Borrar Pantalla
		ImprimirTitulo
		ImprimirOpciones

		Leer opcion_interfaz

		// Ejecutar opcion de cada formula
		retorno <- Ejecutar(opcion_interfaz)
		
		Segun (retorno) Hacer
			1:
				retorno <- retorno
			2:
				opcion_interfaz <- 9
				Escribir " _  _  __    __ _____ __    ___ ___  __  __  _ _____ __  "
				Escribir "| || |/  \ /` _/_   _/  \  | _,\ _ \/__\|  \| |_   _/__\ "
				Escribir "| >< | /\ |`._`. | || /\ | | v_/ v / \/ | | ` | | || \/ |"
				Escribir "|_||_|_||_||___/ |_||_||_| |_| |_|_\\__/|_|\__| |_| \__/ "
			3:
				Escribir "  __  ___  ____  __  __  _   _ __  _  _   _   __  _   _ __   __  "
				Escribir " /__\| _,\/ _/ |/__\|  \| | | |  \| || \ / | /  \| | | | _\ /  \ "
				Escribir "| \/ | v_/ \_| | \/ | | ` | | | | ` |`\ V /`| /\ | |_| | v | /\ |"
				Escribir " \__/|_|  \__/_|\__/|_|\__| |_|_|\__|  \_/  |_||_|___|_|__/|_||_|"
				Esperar Tecla
			De Otro Modo:
				Escribir " ___ ___ ___  __  _  __  __  _  __    ___ __  _ _____ ___ ___ "
				Escribir "| _,\ _ \ __/` _/| |/__\|  \| |/  \  | __|  \| |_   _| __| _ \"
				Escribir "| v_/ v / _|`._`.| | \/ | | ` | /\ | | _|| ` | | | | | _|| v /"
				Escribir "|_| |_|_\___|___/|_|\__/|_|\__|_||_| |___|_|\__| |_| |___|_|_\"
				Esperar Tecla
		FinSegun

	Hasta Que opcion_interfaz = 9
FinAlgoritmo
