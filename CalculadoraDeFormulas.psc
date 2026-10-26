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

Funcion ImprimirSubtitulo(subtitulo) //Funcion para mostrar como subtitulo la opción seleccionada
	Definir longitudSubtitulo Como Entero //corrección en el nombre de la variable
	Definir i Como Entero
	
	longitudSubtitulo <- Longitud(subtitulo) //corrección en el nombre de la variable
	
	ImprimirLinea(longitudSubtitulo + 8, "[", "=", "]")  //corrección en el nombre de la variable
	
	Escribir "[    " Sin Saltar
	Escribir subtitulo Sin Saltar
	Escribir "    ]"
	
	ImprimirLinea(longitudSubtitulo + 8, "[", "=", "]") //corrección en el nombre de la variable
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
SubProceso trabajo <- CalcularTrabajo(F, d, theta) //Se cambio "s" por "d"
	trabajo <- F * d * cos(theta * PI / 180)
FinSubProceso

SubProceso trabajo <- CalcularTrabajoResultante(F, micro, m, d, GRAVEDAD)//Se cambio "s" por "d"
	trabajo <- (F - (micro * m * GRAVEDAD)) * d //Se cambio "s" por "d"
FinSubProceso

SubProceso energia <- CalcularEnergiaCinetica(m, v)
	energia <- 0.5 * m * v * v
FinSubProceso

SubProceso energia <- CalcularEnergiaPotencial(m, h, GRAVEDAD) //Se agrego GRAVEDAD como argumento
	energia <- m * GRAVEDAD * h
FinSubProceso

SubProceso fuerza <- CalcularTeoremaTrabajoEnergia(m, vo, vf, s)
	fuerza <- (0.5 * m * vo * vf - 0.5 * m * vf * vo) / s
FinSubProceso

SubProceso velocidad <- CalcularConservacionEnergia(h, GRAVEDAD) //Se agrego GRAVEDAD como argumento
	velocidad <- Raiz(2 * GRAVEDAD * h)
FinSubProceso

SubProceso velocidadFinal <- CalcularConservacionFriccion(m, h, f, s, GRAVEDAD) //Se agrego GRAVEDAD como argumento
	velocidadFinal <- Raiz(2 * GRAVEDAD * h - (2 * f * s) / (m))
FinSubProceso

SubProceso wats <- CalcularPotenciaVatios(W, t)
	wats <- W / t
FinSubProceso

SubProceso hp <- CalcularPotenciaCaballos(F, v)
	hp <- F * v
FinSubProceso

SubProceso hp <- ConvertirVatiosACaballosFuerza(Vatios)
    hp <- Vatios / 746
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
			Borrar Pantalla
			
			Definir F Como Real //Fuerza
			Definir d Como Real //Se cambio "s" por "d" >> eso indica distacia, o desplazamiento
			Definir theta como Real // angulo entre la fuerza y el movimiento
			Definir W Como Real //Trabajo
			
			Repetir
				
				subtitulo <- "TRABAJO DE UNA FUERZA" //Reposición para que se muestre el suntítulo cada que se vuelva a realizar un cálculo
				ImprimirSubtitulo(subtitulo)
				
				Repetir
					ImprimirPregunta("Ingresa la magnitud de la fuerza (en Newtons):")
					Leer F
					
					si F < 0
						Escribir "La fuerza no puede ser negativa"
						Esperar 1.5 Segundos
					FinSi
				Hasta Que F >= 0

				Repetir
					ImprimirPregunta("Ingresa la distancia / desplazamiento (en metros):")
					Leer d //Se cambio "s" por "d"
					
					si d < 0 //Se cambio "s" por "d"
						Escribir "La distancia / desplazamiento no puede ser negativa"
						Esperar 1.5 Segundos
					FinSi
				Hasta Que d >= 0
				
				Repetir
					ImprimirPregunta("Ingresa el ángulo entre la fuerza y el movimiento (en grados):")
					Leer theta
					
					si theta < 0
						Escribir "El  ángulo entre la fuerza y el movimiento no puede ser negativo"
						Esperar 1.5 segundos
					FinSi
				Hasta Que theta >= 0

		    	W <- CalcularTrabajo(F, d, theta) //Se cambio "s" por "d"
				
				Repetir
					Borrar Pantalla
					ImprimirTitulo
					
					Escribir "El trabajo de la fuerza es de: ", W, " J."
					Escribir ""
					
					ImprimirDialogo("¿Quieres hacer otro cálculo? (Y / N)")
					Leer respuesta
					
					respuesta <- Mayusculas(respuesta) //convierte la entrada en mayuscula
				 
					Si respuesta <> 'Y' Y respuesta <> 'N' Entonces //condición para "limpiar" la pantalla y mostrar de nuevo el subtítulo
						Escribir "Entrada invalida"
						Esperar 1.5 Segundos
					FinSi
					
				Hasta Que respuesta == 'Y' o respuesta == 'N'
				Borrar Pantalla

			Hasta Que respuesta = 'N'
		2:
			Borrar Pantalla
			//subtitulo <- "TRABAJO RESULTANTE"
			
			Definir F Como Real //Fuerza
			Definir micro Como Real //coeficiente de friccion
			Definir m Como Real //masa
			Definir d Como Real //Se cambio "s" por "d" >> eso indica distacia, o desplazamiento
			Definir W Como Real //Trabajo

			Repetir
				
				subtitulo <- "TRABAJO RESULTANTE"
				ImprimirSubtitulo(subtitulo)
				
				Repetir 
					ImprimirPregunta("Ingresa la magnitud de la fuerza (en Newtons):")
					Leer F
					
					si F < 0
						Escribir "La fuerza no puede ser negativa"
						Esperar 1.5 Segundos
					FinSi
				Hasta Que F >= 0
				
				Repetir 
					ImprimirPregunta("Ingresa el valor del coeficiente de fricción (valores de entre 0 y 1):")
					Leer micro
					
					si micro < 0 Y micro > 1
						Escribir "El valor del coeficien de fricción debe de ser entr 0 y 1"
						Esperar 1.5 Segundos
					FinSi
				Hasta Que micro >= 0 Y micro <= 1
				
				Repetir
					ImprimirPregunta("Ingresa la masa (en kilogramos):")
					Leer m
					
					si m < 0
						Escribir "La masa no puede ser negativa"
						Esperar 1.5 Segundos
					FinSi
				Hasta Que m >= 0
				
				Repetir
					ImprimirPregunta("Ingresa la distancia (en metros):")
					Leer d
					
					Si d < 0 
						Escribir "La distancia no puede ser negativa"
						Esperar 1.5 Segundos
					FinSi
				Hasta Que d >= 0
				
				W <- CalcularTrabajoResultante(F, micro, m, d, GRAVEDAD)
				
				Repetir
					Borrar Pantalla
					ImprimirTitulo
					
					Escribir "El trabajo resultante es de: ", W, " J."
					Escribir ""
				
					ImprimirDialogo("¿Quieres hacer otro cálculo? (Y / N)")
					Leer respuesta
					
					respuesta <- Mayusculas(respuesta) //convierte la entrada en mayuscula
					
					Si respuesta <> 'Y' Y respuesta <> 'N' Entonces //condición para "limpiar" la pantalla y mostrar de nuevo el subtítulo
						Escribir "Entrada invalida"
						Esperar 1.5 Segundos
					FinSi
				Hasta Que respuesta == 'Y' o respuesta == 'N'
				Borrar Pantalla

			Hasta Que respuesta = 'N'
		3:
			Borrar Pantalla
			
			// Entradas
			Definir m Como Real
			Definir v Como Real
			
			// Resultado
			Definir E Como Real
			
			Repetir
				subtitulo <- "ENERGÍA CINÉTICA"
				ImprimirSubtitulo(subtitulo)
				
				Repetir
					ImprimirPregunta("Ingresa la masa (en kilogramos):")
					Leer m
					
					si m < 0
						Escribir "La masa no puede ser negativa"
						Esperar 1.5 Segundos
					FinSi
				Hasta Que m >= 0
				
				Repetir
					ImprimirPregunta("Ingresa la velocidad constante (en metros por segundo):")
					Leer v
					
					Si v < 0
						Escribir "La velocidad constante no puede ser menor a 0"
						Esperar 1.5 Segundos
					FinSi
				Hasta Que v >= 0
				
				E <- CalcularEnergiaCinetica(m, v)
				
				Repetir
					Borrar Pantalla
					ImprimirTitulo
					
					Escribir "La energía cinética es de: ", E, " J."
					Escribir ""
					
					ImprimirDialogo("¿Quieres hacer otro cálculo? (Y / N)")
					Leer respuesta
					
					respuesta <- Mayusculas(respuesta) //convierte la entrada en mayuscula
					
					Si respuesta <> 'Y' Y respuesta <> 'N' Entonces //condición para "limpiar" la pantalla y mostrar de nuevo el subtítulo
						Escribir "Entrada invalida"
						Esperar 1.5 Segundos
					FinSi
				Hasta Que respuesta == 'Y' o respuesta == 'N'
				Borrar Pantalla
				
			Hasta Que respuesta = 'N'
		4:
			Borrar Pantalla
			
			Definir m Como Real
			Definir h Como Real
			Definir E Como Real
			
			Repetir
				
				subtitulo <- "ENERGÍA POTENCIAL"
				ImprimirSubtitulo(subtitulo)
				
				Repetir
					ImprimirPregunta("Ingresa la masa (en kilogramos):")
					Leer m
					
					si m < 0
						Escribir "La masa no puede ser negativa"
						Esperar 1.5 Segundos
					FinSi
				Hasta Que m >= 0
				
				ImprimirPregunta("Ingresa la altura (en metros):")
				Leer h
				
				E <- CalcularEnergiaPotencial(m,h, GRAVEDAD)
				
				Repetir
					Borrar Pantalla
					ImprimirTitulo
					
					Escribir "La energía cinética es de: ", E, " J."
					Escribir ""
					
					ImprimirDialogo("¿Quieres hacer otro cálculo? (Y / N)")
					Leer respuesta
					
					respuesta <- Mayusculas(respuesta) //convierte la entrada en mayuscula
					
					Si respuesta <> 'Y' Y respuesta <> 'N' Entonces //condición para "limpiar" la pantalla y mostrar de nuevo el subtítulo
						Escribir "Entrada invalida"
						Esperar 1.5 Segundos
					FinSi
				Hasta Que respuesta == 'Y' o respuesta == 'N'
				Borrar Pantalla
				
			Hasta Que respuesta = 'N'
		5:
			Borrar Pantalla
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
			Borrar Pantalla
			subtitulo <- "CONSERVACIÓN DE LA ENERGÍA"
			
			// Entradas
			Definir h Como Real
			
			// Resultado
			Definir v Como Real
			
			Repetir
				ImprimirPregunta("Ingresa la altura (en metros):")
				Leer h
				
				v <- CalcularConservacionEnergia(h, GRAVEDAD)
				
				Borrar Pantalla
				ImprimirTitulo
				
				Escribir "La velocidad es de: ", v, " m/s."
				
				ImprimirDialogo("¿Quieres hacer otro cálculo? (Y / N)")
		    	Leer respuesta
				
			Hasta Que respuesta = "n"
		7:
			Borrar Pantalla
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
				
				vf <- CalcularConservacionFriccion(m, h, f, s, GRAVEDAD)
				
				Borrar Pantalla
				ImprimirTitulo
				
				Escribir "La velocidad final es de: ", vf, " m/s."
				
				ImprimirDialogo("¿Quieres hacer otro cálculo? (Y / N)")
		    	Leer respuesta
				
			Hasta Que respuesta = "n"
		8:
			Borrar Pantalla
		    subtitulo <- "POTENCIA"
			
			// Entradas
			Definir W Como Real
			Definir t Como Real
			Definir F Como Real
			Definir v Como Real
			
			// Resultado
			Definir Vatios Como Real
			Definir CaballosFuerza Como Real
			
            Definir sub_opcion Como Entero

            // Ciclo de opcion completa
            Repetir
                // Ciclo de sub-opcion
                Repetir
                    Borrar Pantalla
                    ImprimirTitulo
                    
                    // Seleccion de opcion multiple
                    ImprimirDialogo("Seleccione una de las siguientes opciones:")
                    Escribir "|  1. W (Trabajo) y t (Tiempo)"
                    Escribir "|  2. F (Fuerza) y v (Velocidad)"

                    Leer sub_opcion

                    Segun sub_opcion Hacer
                        1:
                            ImprimirPregunta("Ingresa el trabajo (en joules):") 
                            Leer W

                            ImprimirPregunta("Ingresa el tiempo (en segundos):") 
                            Leer t

                            Vatios <- CalcularPotenciaVatios(W, t)
                            CaballosFuerza <- ConvertirVatiosACaballosFuerza(Vatios)
                        2:
                            ImprimirPregunta("Ingresa la fuerza (en Newtons):") 
                            Leer F

                            ImprimirPregunta("Ingresa la velocidad (en metros por segundo):") 
                            Leer v

                            Vatios <- CalcularPotenciaCaballos(F, v)
                            CaballosFuerza <- ConvertirVatiosACaballosFuerza(Vatios)
                        De Otro Modo:
                            Escribir "Entrada invalida"
                            Esperar 1.5 Segundos
                    FinSegun
                Hasta Que sub_opcion = 1 O sub_opcion = 2

            Repetir
                Borrar Pantalla
                ImprimirTitulo

                // Mostrar el resultado en cada dialogo
                Escribir "La potencia en Vatios es de ", Vatios, " W."
                Escribir "La potencia en Caballos de Fuerza es de ", CaballosFuerza, " hp."
                Escribir ""

                ImprimirDialogo("¿Quieres hacer otro cálculo? (Y / N)")
                Leer respuesta
                
                respuesta <- Mayusculas(respuesta) //convierte la entrada en mayuscula
             
                Si respuesta <> 'Y' Y respuesta <> 'N' Entonces //condición para "limpiar" la pantalla y mostrar de nuevo el subtítulo
                    Escribir "Entrada invalida"
                    Esperar 1.5 Segundos
                FinSi
                    
                Hasta Que respuesta == 'Y' o respuesta == 'N'
                Borrar Pantalla

            Hasta Que respuesta = 'N'
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
