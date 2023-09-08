domains
	maquina, funciona, potencia, aroma, est = symbol
	consumo =  real
	seleccion, motor_maquina, cabezote_maquina = integer
facts-motores
	motor(motor_maquina,funciona)
	consumoder(motor_maquina,consumo)
	consumoizq(motor_maquina,consumo)
	fuerza(motor_maquina,potencia)
	facilfrenar(motor_maquina)
	secalienta(motor_maquina)
	olor(motor_maquina,aroma)
	sonidosquererencender(motor_maquina)
	pedalregresa(motor_maquina)
	ningunruido(motor_maquina)
	fallaelectrica(motor_maquina)
	seculebrea(motor_maquina)
	haceruido(motor_maquina)
	diagnostico(motor_maquina,est)
	puchado(motor_maquina)
facts-cabezotes
	diagnosticoc(cabezote_maquina,est)
	tuberiastapadas(cabezote_maquina)
	engranajespegados(cabezote_maquina)
	execesodesuiciedad(cabezote_maquina)
	fallapuntadainferior(cabezote_maquina)
	fallapuntadasuperior(cabezote_maquina)
	saltapuntada(cabezote_maquina)
	rompehilo(cabezote_maquina)
	quiebraaguja(cabezote_maquina)
	arrastremalo(cabezote_maquina)
	danamaterial(cabezote_maquina)
predicates
	%%predicados de las reglas
	%%motores
	nondeterm fallam(integer,motor_maquina)
	nondeterm imprimir(integer,integer,motor_maquina)
	nondeterm estado(motor_maquina,est) 
	nondeterm examinarmotor(motor_maquina)
	nondeterm darpuchon(motor_maquina)
	nondeterm puchon(motor_maquina)
	
	%%cabezotes
	nondeterm estadoc(cabezote_maquina,est)
	nondeterm examinarcabezote(cabezote_maquina)
	nondeterm fallac(integer,cabezote_maquina)
	nondeterm imprimirc(integer,integer,cabezote_maquina)
	
	%%predicado del menu principal
	nondeterm caso(seleccion)
	nondeterm menu
	nondeterm pasomotor(motor_maquina,integer)
	nondeterm limpiardatomotor(motor_maquina,integer)
	nondeterm pasocabezote(cabezote_maquina,integer)
	nondeterm ent(integer,integer,integer)
	nondeterm limpiar(integer,integer)
	nondeterm entrada(integer,integer,integer)
	nondeterm limpiarc(integer,integer)
	nondeterm limpiarcabezote(integer,cabezote_maquina)

clauses
	%%reglas para motores
	estado(MOTOR,"quemado"):-
				motor(MOTOR,"funciona"),
				fuerza(MOTOR,"poca"),
				assert(diagnostico(MOTOR,"quemado"));
				
				motor(MOTOR,"funciona"),
				consumoder(MOTOR,DER),
				consumoizq(MOTOR,IZQ),
				X=DER-IZQ,abs(X)>DER*0.15,
				assert(diagnostico(MOTOR,"quemado"));
				
				motor(MOTOR,"funciona"),
				facilfrenar(MOTOR),
				assert(diagnostico(MOTOR,"quemado"));
				
				motor(MOTOR,"funciona"),
				secalienta(MOTOR),
				olor(MOTOR,"caucho quemado"),
				assert(diagnostico(MOTOR,"quemado"));
				
				motor(MOTOR,"no funciona"),
				olor(MOTOR,"caucho quemado"),
				assert(diagnostico(MOTOR,"quemado")).
				
	estado(MOTOR,"sucio"):-
				motor(MOTOR,"funciona"),
				secalienta(MOTOR),
				not (olor(MOTOR,"caucho quemado")),
				not (olor(MOTOR,"madera quemada")),
				assert(diagnostico(MOTOR,"sucio")).
				
	estado(MOTOR,"Falla en capacitor"):-puchado(MOTOR),
					    assert(diagnostico(MOTOR,"Falla en capacitor")).
	
	estado(MOTOR,"Falla en clost: Pedal Mal Ajustado"):-
					motor(MOTOR,"no funciona"),
					sonidosquererencender(MOTOR),
					not (pedalregresa(MOTOR)),
					assert(diagnostico(MOTOR,"Falla en clost: Pedal Mal Ajustado")).
					
	estado(MOTOR, "Falla en clost: Corchos"):-
						motor(MOTOR,"no funciona"),
						olor(MOTOR,"madera quemada"),
						assert(diagnostico(MOTOR,"Falla en clost: Corchos")).
						
	estado(MOTOR, "Eje Torcido"):-
				     motor(MOTOR,"funciona"),
				     seculebrea(MOTOR),
				     haceruido(MOTOR),
				     assert(diagnostico(MOTOR,"Eje Torcido")).
			     
					
	estado(MOTOR,"\na. Falla En Fusibles \nb. CortoCircuito \n c. Quemado \nd. Falla Electrica\n"):-
					motor(MOTOR,"no funciona"),
					ningunruido(MOTOR),
					not (fallaelectrica(MOTOR)),
					assert(diagnostico(MOTOR,"a.Falla En Fusibles \nb. CortoCircuito \nc. Quemado d. Falla Electrica")).
					
	estado(MOTOR,"Falla Electrica"):-
					motor(MOTOR,"no funciona"),
					ningunruido(MOTOR),
					fallaelectrica(MOTOR),
					assert(diagnostico(MOTOR,"Falla Electrica")).
	darpuchon(MOTOR):-
			motor(MOTOR,"no funciona"),
			sonidosquererencender(MOTOR),
			pedalregresa(MOTOR).
			
	puchon(MOTOR):-
			darpuchon(MOTOR),
			write("\n****DE UN PUCHON AL MOTOR**** \n 1 Motor Enciende \n 2 Motor No enciende \n"),readint(PUCHADA),assert(puchado(MOTOR)),PUCHADA=1;
			retract(puchado(MOTOR)). %%1 funciona
			
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	%%reglas para cabezotes
	estadoc(CABEZOTE,"sucio"):-engranajespegados(CABEZOTE),
				   assert(diagnosticoc(CABEZOTE,"sucio"));
				   
				   tuberiastapadas(CABEZOTE),
				   assert(diagnosticoc(CABEZOTE,"sucio")).
				   
	estadoc(CABEZOTE,"fpuntadainferior"):-fallapuntadainferior(CABEZOTE),
				   	      assert(diagnosticoc(CABEZOTE,"fpuntadainferior")).
				   	      
	estadoc(CABEZOTE,"fpuntadasuperior"):-fallapuntadasuperior(CABEZOTE),
				   	      assert(diagnosticoc(CABEZOTE,"fpuntadasuperior")).
				   	      
	estadoc(CABEZOTE,"saltapuntada"):-saltapuntada(CABEZOTE),
				   	  assert(diagnosticoc(CABEZOTE,"saltapuntada")).
	
	estadoc(CABEZOTE,"rompehilo"):-rompehilo(CABEZOTE),
				   	  assert(diagnosticoc(CABEZOTE,"rompehilo")).
				   	  
	estadoc(CABEZOTE,"quiebraaguja"):-quiebraaguja(CABEZOTE),
				   	  assert(diagnosticoc(CABEZOTE,"quiebraaguja")).
	
	estadoc(CABEZOTE,"arrastremalo"):-arrastremalo(CABEZOTE),
					  assert(diagnosticoc(CABEZOTE,"arrastremalo")).
					  
	
	estadoc(CABEZOTE,"danamaterial"):-danamaterial(CABEZOTE),
					  assert(diagnosticoc(CABEZOTE,"danamaterial")).
	
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   	    
				
	menu:-write("SELECCIONAR TIPO DE PROBLEMA \n 1: Ingresar datos de un Motor \n 2: Consultar Sobre un Motor \n 3: Ingresar un Cabezote \n4: Consultar un Cabezote \n otro Salir"),nl,readint(N),caso(N).
	caso(1):-
		write("Ingrese un codigo para el motor\n"),readint(MOTOR),nl,
		limpiardatomotor(MOTOR,1),limpiardatomotor(MOTOR,2),pasomotor(MOTOR,1),
		menu.
	caso(2):-
		write("Ingrese el codigo del motor\n"),readint(MOTOR),nl,write("\n\n\n\n\n**************LISTA DE POSIBLES FALLOS DEL MOTOR ",MOTOR," *********************\n\n\n\n"),
		limpiardatomotor(MOTOR,2),
		examinarmotor(MOTOR),
		imprimir(0,0,MOTOR),
		write("\n\n\n\n\n****************FIN LISTA*********************\n\n\n\n"),
		menu.
	caso(3):-
		write("Ingrese un codigo para el Cabezote \n"),readint(CABEZOTE),nl,
		limpiarcabezote(2,CABEZOTE),limpiarcabezote(1,CABEZOTE),pasocabezote(CABEZOTE,1),
		menu.
	caso(4):-
		write("Ingrese el codigo del Cabezote\n"),readint(CABEZOTE),nl,write("\n\n\n\n\n**************LISTA DE POSIBLES FALLOS DEL CABEZOTE ",CABEZOTE," *********************\n\n\n\n"),
		limpiarcabezote(2,CABEZOTE),
		examinarcabezote(CABEZOTE),
		imprimirc(0,0,CABEZOTE),
		write("\n\n\n\n\n****************FIN LISTA*********************\n\n\n\n"),
		menu.
	caso(_):-
		save("D:\\motores.txt",motores),
		save("D:\\cabezotes.txt",cabezotes),
		write("Fin Programa"),nl,!.
		
		
	%%Llenado de los datos del motor
	pasomotor(MOTOR,1):-write("¿El Motor Funciona? \n 1. SI \n 2. NO\n"),readint(FUNCIONA),entrada(1,MOTOR,FUNCIONA).
	pasomotor(MOTOR,2):-write("\n***PRUEBE LOS CONSUMOS DEL MOTOR***\n"),entrada(2,MOTOR,0).
	pasomotor(MOTOR,3):-write("\n***INGRESE LA FUERZA CUALITATIVA DEL MOTOR \n1. FUERTE \n2. NORMAL, \n3. DEBIL***\n"),readint(FUERZA),entrada(3,MOTOR,FUERZA).
	pasomotor(MOTOR,4):-write("\n***PRUEBE SI EL MOTOR ES FACIL DE FRENAR \n1. SI \n2. NO***\n"),readint(FRENAR),entrada(4,MOTOR,FRENAR).
	pasomotor(MOTOR,5):-write("\n***PRUEBE SI EL MOTOR SE CALIENTA \n1. SI \n2. NO***\n"),readint(CALIENTA),entrada(5,MOTOR,CALIENTA).
	pasomotor(MOTOR,6):-write("\n***COMPRUEBE SI EL MOTOR SE CULEBREA \n1. SI \n2. NO***\n"),readint(CULEBRA),entrada(6,MOTOR,CULEBRA).
	pasomotor(MOTOR,7):-write("\n***COMPRUEBE SI EL MOTOR HACE RUIDOS CHIFLIDOS O DE CHOQUES \n1. SI \n2. NO***\n"),readint(RUIDOS),entrada(7,MOTOR,RUIDOS).
	pasomotor(MOTOR,8):-write("\n***COMPRUEBE EL OLOR DEL MOTOR \n1. CAUCH0 QUEMADO \n2. MADERA QUEMADA \n3. OTRO***\n"),readint(OLOR),entrada(8,MOTOR,OLOR).
	pasomotor(MOTOR,9):-examinarmotor(MOTOR).
	pasomotor(MOTOR,10):-write("\n***COMPRUEBE EL OLOR DEL MOTOR \n1. CAUCH0 QUEMADO \n2. MADERA QUEMADA \n3. OTRO***\n"),readint(OLOR),entrada(9,MOTOR,OLOR).
	pasomotor(MOTOR,11):-write("\n***COMPRUEBE SI MOTOR HACE RUIDOS DE QUERER ENCENDER \n1. SI \n2. NO ***\n"),readint(RUIDOEN),entrada(10,MOTOR,RUIDOEN).
	pasomotor(MOTOR,12):-write("\n***COMPRUEBE SI EL PEDAL DEL MOTOR REGRESA \n1. SI \n2. NO ***\n"),readint(PEDAL),entrada(11,MOTOR,PEDAL).
	pasomotor(MOTOR,13):-limpiar(14,MOTOR),puchon(MOTOR).
	pasomotor(MOTOR,14):-write("\n***COMPRUEBE SI MOTOR NO HACE ABSOLUTAMENTE NINGUN RUIDO \n1. SI(NINGUN RUIDO) \n2. NO(HACE RUIDOS) ***\n"),readint(NRUIDO),entrada(12,MOTOR,NRUIDO).
	pasomotor(MOTOR,15):-write("\n***COMPRUEBE SI HAY CHISPA EN ALGUN LADO(CABLE, CAJA ENCENDIDO O MOTOR) \n1. SI \n2. NO***\n"),readint(CHISPA),entrada(13,MOTOR,CHISPA).
	pasomotor(MOTOR,15):-examinarmotor(MOTOR).
	
	%%Entradas cuando el motor funciona
	entrada(1,MOTOR,1):-limpiar(1,MOTOR),assert(motor(MOTOR,"funciona")),pasomotor(MOTOR,2).
	entrada(2,MOTOR,0):-write("\nIngrese el consumo hacia la derecha\n"),readint(DER),limpiar(11,MOTOR),assert(consumoder(MOTOR,DER)),entrada(2,MOTOR,1).
	entrada(2,MOTOR,0):-write("\nIngrese el consumo hacia la izquierda\n"),readint(IZQ),limpiar(12,MOTOR),assert(consumoizq(MOTOR,IZQ)),pasomotor(MOTOR,3).
	entrada(3,MOTOR,1):-limpiar(6,MOTOR),assert(fuerza(MOTOR,"mucha")),pasomotor(MOTOR,4).
	entrada(3,MOTOR,2):-limpiar(6,MOTOR),assert(fuerza(MOTOR,"normal")),pasomotor(MOTOR,4).
	entrada(3,MOTOR,FUERZA):-FUERZA<>2,FUERZA<>1,limpiar(6,MOTOR),assert(fuerza(MOTOR,"poca")),pasomotor(MOTOR,4).
	entrada(4,MOTOR,1):-limpiar(5,MOTOR),assert(facilfrenar(MOTOR)),pasomotor(MOTOR,5).
	entrada(4,MOTOR,FRENAR):-FRENAR<>1,limpiar(5,MOTOR),pasomotor(MOTOR,5).
	entrada(5,MOTOR,1):-limpiar(4,MOTOR),assert(secalienta(MOTOR)),pasomotor(MOTOR,6).
	entrada(5,MOTOR,CALIENTA):-CALIENTA<>1,limpiar(4,MOTOR),pasomotor(MOTOR,6).
	entrada(6,MOTOR,1):-limpiar(10,MOTOR),assert(seculebrea(MOTOR)),pasomotor(MOTOR,7).
	entrada(6,MOTOR,CULEBRA):-CULEBRA<>1,limpiar(10,MOTOR),pasomotor(MOTOR,7).
	entrada(7,MOTOR,1):-limpiar(8,MOTOR),assert(haceruido(MOTOR)),pasomotor(MOTOR,8).
	entrada(7,MOTOR,RUIDOS):-RUIDOS<>1,limpiar(8,MOTOR),pasomotor(MOTOR,8).
	entrada(8,MOTOR,1):-limpiar(2,MOTOR),assert(olor(MOTOR,"caucho quemado")),pasomotor(MOTOR,9).
	entrada(8,MOTOR,2):-limpiar(2,MOTOR),assert(olor(MOTOR,"madera quemada")),pasomotor(MOTOR,9).
	entrada(8,MOTOR,OLOR):-OLOR<>1,OLOR<>2,limpiar(2,MOTOR),assert(olor(MOTOR,"otro")),pasomotor(MOTOR,9).
	
	
	%%Entradas cuando el motor no funciona
	entrada(1,MOTOR,FUNCIONA):-FUNCIONA<>1,limpiar(1,MOTOR),assert(motor(MOTOR,"no funciona")),pasomotor(MOTOR,10).
	entrada(9,MOTOR,1):-limpiar(2,MOTOR),assert(olor(MOTOR,"caucho quemado")),pasomotor(MOTOR,11).
	entrada(9,MOTOR,2):-limpiar(2,MOTOR),assert(olor(MOTOR,"madera quemada")),pasomotor(MOTOR,11).
	entrada(9,MOTOR,OLOR):-OLOR<>1,OLOR<>2,limpiar(2,MOTOR),assert(olor(MOTOR,"otro")),pasomotor(MOTOR,11).
	
	%%Ruidos de querer encender
	entrada(10,MOTOR,1):-limpiar(3,MOTOR),assert(sonidosquererencender(MOTOR)),pasomotor(MOTOR,12).
	entrada(11,MOTOR,1):-limpiar(13,MOTOR),assert(pedalregresa(MOTOR)),pasomotor(MOTOR,13).
	entrada(11,MOTOR,PEDAL):-PEDAL<>1,limpiar(13,MOTOR),pasomotor(MOTOR,14).
	
	%No hace ruidos de querer encender
	entrada(10,MOTOR,RUIDOEN):-RUIDOEN<>1,limpiar(3,MOTOR),pasomotor(MOTOR,14).
	%%%%%
	
	entrada(12,MOTOR,1):-limpiar(7,MOTOR),assert(ningunruido(MOTOR)),pasomotor(MOTOR,15).
	entrada(12,MOTOR,NRUIDO):-NRUIDO<>1,limpiar(7,MOTOR),pasomotor(MOTOR,15).
	entrada(13,MOTOR,1):-limpiar(9,MOTOR),assert(fallaelectrica(MOTOR)),pasomotor(MOTOR,16).
	entrada(13,MOTOR,CHISPA):-CHISPA<>1,limpiar(9,MOTOR),pasomotor(MOTOR,16).
			      
	%%limpia datos del motor cuando se sobreescriben			      
	limpiardatomotor(MOTOR,1):- limpiar(1,MOTOR),limpiar(2,MOTOR),limpiar(3,MOTOR),limpiar(4,MOTOR),limpiar(5,MOTOR),limpiar(6,MOTOR),limpiar(7,MOTOR),limpiar(8,MOTOR),
	                             limpiar(9,MOTOR),limpiar(10,MOTOR),limpiar(11,MOTOR),limpiar(12,MOTOR),limpiar(13,MOTOR),limpiar(14,MOTOR).
				
	limpiardatomotor(MOTOR,2):- retract(diagnostico(MOTOR,_)),retract(diagnostico(MOTOR,_)),retract(diagnostico(MOTOR,_)),retract(diagnostico(MOTOR,_)),retract(diagnostico(MOTOR,_)),
				    retract(diagnostico(MOTOR,_)),retract(diagnostico(MOTOR,_)),retract(diagnostico(MOTOR,_)),retract(diagnostico(MOTOR,_)),retract(diagnostico(MOTOR,_)),
				    retract(diagnostico(MOTOR,_)),retract(diagnostico(MOTOR,_)),retract(diagnostico(MOTOR,_)),retract(diagnostico(MOTOR,_)),retract(diagnostico(MOTOR,_));
	                            true.
	
	limpiar(1,MOTOR):-retract(motor(MOTOR,_));true.
	limpiar(2,MOTOR):-retract(olor(MOTOR,_));true.
	limpiar(3,MOTOR):-retract(sonidosquererencender(MOTOR));true.
	limpiar(4,MOTOR):-retract(secalienta(MOTOR));true.
	limpiar(5,MOTOR):-retract(facilfrenar(MOTOR));true.
	limpiar(6,MOTOR):-retract(fuerza(MOTOR,_));true.
	limpiar(7,MOTOR):-retract(ningunruido(MOTOR));true.
	limpiar(8,MOTOR):-retract(haceruido(MOTOR));true.
	limpiar(9,MOTOR):-retract(fallaelectrica(MOTOR));true.
	limpiar(10,MOTOR):-retract(seculebrea(MOTOR));true.
	limpiar(11,MOTOR):-retract(consumoder(MOTOR,_));true.
	limpiar(12,MOTOR):-retract(consumoizq(MOTOR,_));true.
	limpiar(13,MOTOR):-retract(pedalregresa(MOTOR));true.
	limpiar(14,MOTOR):-retract(puchado(MOTOR));true.
	
	imprimir(0,0,MOTOR):-imprimir(1,1,MOTOR).
	imprimir(1,N,MOTOR):-diagnostico(MOTOR,"quemado"),write("\n",N,". Motor Posiblemente Quemado\n"),M=N+1,imprimir(2,M,MOTOR);
			     true,imprimir(2,N,MOTOR).
	imprimir(2,N,MOTOR):-diagnostico(MOTOR,"sucio"),write("\n",N,". Motor Posiblemente Sucio\n"),M=N+1,imprimir(3,M,MOTOR);
			     true,imprimir(3,N,MOTOR).
	imprimir(3,N,MOTOR):-diagnostico(MOTOR,"Falla en capacitor"),write("\n",N,". Motor Con Posible Falla en Capacitor\n"),M=N+1,imprimir(4,M,MOTOR);
			     true,imprimir(4,N,MOTOR).
	imprimir(4,N,MOTOR):-diagnostico(MOTOR,"Falla en clost: Pedal Mal Ajustado"),write("\n",N,". Motor Con Posible Falla en el Clost: \n   a.Pedal Desajustado\n"),M=N+1,imprimir(5,M,MOTOR);
			     true,imprimir(5,N,MOTOR).
	imprimir(5,N,MOTOR):-diagnostico(MOTOR,"Falla en clost: Corchos"),write("\n",N,". Motor Con Posible Falla en el Clost: \n   a.Corchos\n"),M=N+1,imprimir(6,M,MOTOR);
			    true,imprimir(6,N,MOTOR).
	imprimir(6,N,MOTOR):-diagnostico(MOTOR,"Eje Torcido"),write("\n",N,". Motor Con el Eje Posiblemente Torcido\n"),M=N+1,imprimir(7,M,MOTOR);
			     true,imprimir(7,N,MOTOR).
	imprimir(7,N,MOTOR):-diagnostico(MOTOR,"\na. Falla En Fusibles \nb. CortoCircuito \n c. Quemado d. Falla Electrica\n"),
			     write("\n",N,". Motor Con Posible Fallass Electricas: \n  a. Falla En Fusibles \n  b. CortoCircuito c. Falla Electrica\n"),M=N+1,imprimir(8,M,MOTOR);
			     
			    true,imprimir(8,N,MOTOR).
	imprimir(8,N,MOTOR):-diagnostico(MOTOR,"Falla Electrica"),write("\n",N,".Motor Con Posible Falla Electrica\n"),M=N+1,imprimir(9,M,MOTOR);
			    true,imprimir(9,N,MOTOR).
	imprimir(9,N,MOTOR):-N<2,write("\n___MOTOR__",MOTOR,"__SIN__FALLAS___\n DATOS INSUFICIENTES PARA DETERMINAR UNA FALLA\n____FALLA____DESONOCIDA____\n");
			     true.
				  
	%%evaluacion del estado del motor
	examinarmotor(MOTOR):-fallam(1,MOTOR),fallam(2,MOTOR),fallam(3,MOTOR),fallam(4,MOTOR),fallam(5,MOTOR),fallam(6,MOTOR),fallam(7,MOTOR),fallam(8,MOTOR).
				
				
	fallam(1,MOTOR):-estado(MOTOR,"quemado");true.
	fallam(2,MOTOR):-estado(MOTOR,"sucio");true.
	fallam(3,MOTOR):-estado(MOTOR,"Falla en capacitor");true.
	fallam(4,MOTOR):-estado(MOTOR,"Falla en clost: Pedal Mal Ajustado");true.
	fallam(5,MOTOR):-estado(MOTOR,"Falla en clost: Corchos");true.
	fallam(6,MOTOR):-estado(MOTOR,"Eje Torcido");true.
	fallam(7,MOTOR):-estado(MOTOR,"\na. Falla En Fusibles \nb. CortoCircuito \n c. Quemado d. Falla Electrica\n");true.
	fallam(8,MOTOR):-estado(MOTOR,"Falla Electrica");true.
	
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
				  
	%%llenar los datos del Cabezote
	pasocabezote(CABEZOTE, 1):-write("\n¿SIENTE LA MAQUINA PEGADA? \n 1. SI \n 2. NO\n"),readint(PEGADA),ent(1,CABEZOTE,PEGADA).
	pasocabezote(CABEZOTE, 2):-write("\n¿LA MAQUINA NO LUBRICA BIEN? \n 1. NO LUBRICA BIEN \n 2. LUBRICA BIEN\n"),readint(LUBRICA),ent(2,CABEZOTE,LUBRICA).
	pasocabezote(CABEZOTE, 3):-write("\n¿LA PUNTADA INFERIOR ESTÁ FLOJA O CON NUDOS? \n 1. SI \n 2. NO\n"),readint(PUNTADAIF),ent(3,CABEZOTE,PUNTADAIF).
	pasocabezote(CABEZOTE, 4):-write("\n¿LA PUNTADA SUPERIOR ESTÁ FLOJA O CON NUDOS? \n 1. SI \n 2. NO\n"),readint(PUNTADASU),ent(4,CABEZOTE,PUNTADASU).
	pasocabezote(CABEZOTE, 5):-write("\n¿LA LA MAQUINA SALTA PUNTADA O PUNTADAS MUY LARGAS? \n 1. SI \n 2. NO\n"),readint(PUNTADASA),ent(5,CABEZOTE,PUNTADASA).
	pasocabezote(CABEZOTE, 6):-write("\n¿LA LA MAQUINA ROMPE EL HILO? \n 1. SI \n 2. NO\n"),readint(ROMPE),ent(6,CABEZOTE,ROMPE).
	pasocabezote(CABEZOTE, 7):-write("\n¿LA LA MAQUINA QUIEBRA LA AGUJA? \n 1. SI \n 2. NO\n"),readint(QUIEBRA),ent(7,CABEZOTE,QUIEBRA).
	pasocabezote(CABEZOTE, 8):-write("\n¿EL ARRASTRE DE LA MAQUINA ES REGULAR? \n 1. IRREGULAR \n 2. REGULAR\n"),readint(ARRASTRE),ent(8,CABEZOTE,ARRASTRE).
	pasocabezote(CABEZOTE, 9):-write("\n¿LA MAQUINA DAÑA EL MATERIAL? \n 1. DAÑA MATERIAL \n 2. NO DAÑA MATERIAL\n"),readint(DANA),ent(9,CABEZOTE,DANA).
	pasocabezote(CABEZOTE, 10):-examinarcabezote(CABEZOTE).
	
	%%Entradas cabezote
	ent(1,CABEZOTE,1):-limpiarc(1,CABEZOTE),assert(engranajespegados(CABEZOTE)),pasocabezote(CABEZOTE,2).
	ent(1,CABEZOTE,PEGADA):-PEGADA<>1,limpiarc(1,CABEZOTE),pasocabezote(CABEZOTE,2).
	ent(2,CABEZOTE,1):-limpiarc(2,CABEZOTE),assert(tuberiastapadas(CABEZOTE)),pasocabezote(CABEZOTE,3).
	ent(2,CABEZOTE,LUBRICA):-LUBRICA<>1,limpiarc(2,CABEZOTE),pasocabezote(CABEZOTE,3).
	ent(3,CABEZOTE,1):-limpiarc(3,CABEZOTE),assert(fallapuntadainferior(CABEZOTE)),pasocabezote(CABEZOTE,4).
	ent(3,CABEZOTE,PUNTADAIF):-PUNTADAIF<>1,limpiarc(3,CABEZOTE),pasocabezote(CABEZOTE,4).
	ent(4,CABEZOTE,1):-limpiarc(4,CABEZOTE),assert(fallapuntadasuperior(CABEZOTE)),pasocabezote(CABEZOTE,5).
	ent(4,CABEZOTE,PUNTADASU):-PUNTADASU<>1,limpiarc(4,CABEZOTE),pasocabezote(CABEZOTE,5).
	ent(5,CABEZOTE,1):-limpiarc(5,CABEZOTE),assert(saltapuntada(CABEZOTE)),pasocabezote(CABEZOTE,6).
	ent(5,CABEZOTE,PUNTADASA):-PUNTADASA<>1,limpiarc(5,CABEZOTE),pasocabezote(CABEZOTE,6).
	ent(6,CABEZOTE,1):-limpiarc(6,CABEZOTE),assert(rompehilo(CABEZOTE)),pasocabezote(CABEZOTE,7).
	ent(6,CABEZOTE,ROMPE):-ROMPE<>1,limpiarc(6,CABEZOTE),pasocabezote(CABEZOTE,7).
	ent(7,CABEZOTE,1):-limpiarc(7,CABEZOTE),assert(quiebraaguja(CABEZOTE)),pasocabezote(CABEZOTE,8).
	ent(7,CABEZOTE,	QUIEBRA):-QUIEBRA<>1,limpiarc(7,CABEZOTE),pasocabezote(CABEZOTE,8).
	ent(8,CABEZOTE,1):-limpiarc(8,CABEZOTE),assert(arrastremalo(CABEZOTE)),pasocabezote(CABEZOTE,9).
	ent(8,CABEZOTE,	ARRASTRE):-ARRASTRE<>1,limpiarc(8,CABEZOTE),pasocabezote(CABEZOTE,9).
	ent(9,CABEZOTE,1):-limpiarc(9,CABEZOTE),assert(danamaterial(CABEZOTE)),pasocabezote(CABEZOTE,10).
	ent(9,CABEZOTE,	DANA):-DANA<>1,limpiarc(9,CABEZOTE),pasocabezote(CABEZOTE,10).
	
	
	
	%%LIMPIAR DATOS DEL CABEZOTE CUANDO SE SOBREESCRIBAN
	limpiarcabezote(1,CABEZOTE):-limpiarc(1,CABEZOTE),limpiarc(2,CABEZOTE),limpiarc(3,CABEZOTE),limpiarc(4,CABEZOTE),limpiarc(5,CABEZOTE),limpiarc(6,CABEZOTE),
				     limpiarc(7,CABEZOTE),limpiarc(8,CABEZOTE),limpiarc(9,CABEZOTE).
	
	limpiarcabezote(2,CABEZOTE):-retract(diagnosticoc(CABEZOTE,_)),retract(diagnosticoc(CABEZOTE,_)),retract(diagnosticoc(CABEZOTE,_)),retract(diagnosticoc(CABEZOTE,_)),
				     retract(diagnosticoc(CABEZOTE,_)),retract(diagnosticoc(CABEZOTE,_)),retract(diagnosticoc(CABEZOTE,_)),retract(diagnosticoc(CABEZOTE,_)),
				     retract(diagnosticoc(CABEZOTE,_)),retract(diagnosticoc(CABEZOTE,_)),retract(diagnosticoc(CABEZOTE,_)),retract(diagnosticoc(CABEZOTE,_)),
				     retract(diagnosticoc(CABEZOTE,_));
				     
				     true.
	
	limpiarc(1,CABEZOTE):-retract(engranajespegados(CABEZOTE));true.
	limpiarc(2,CABEZOTE):-retract(tuberiastapadas(CABEZOTE));true.
	limpiarc(3,CABEZOTE):-retract(fallapuntadainferior(CABEZOTE));true.
	limpiarc(4,CABEZOTE):-retract(fallapuntadasuperior(CABEZOTE));true.
	limpiarc(5,CABEZOTE):-retract(saltapuntada(CABEZOTE));true.
	limpiarc(6,CABEZOTE):-retract(rompehilo(CABEZOTE));true.
	limpiarc(7,CABEZOTE):-retract(quiebraaguja(CABEZOTE));true.
	limpiarc(8,CABEZOTE):-retract(arrastremalo(CABEZOTE));true.
	limpiarc(9,CABEZOTE):-retract(danamaterial(CABEZOTE));true.
	
	%%examinar Cabezote
	examinarcabezote(CABEZOTE):-fallac(1,CABEZOTE),fallac(2,CABEZOTE),fallac(3,CABEZOTE),fallac(4,CABEZOTE),fallac(5,CABEZOTE),fallac(6,CABEZOTE),fallac(7,CABEZOTE),fallac(8,CABEZOTE).
				
	fallac(1,CABEZOTE):-estadoc(CABEZOTE,"sucio");true.
	fallac(2,CABEZOTE):-estadoc(CABEZOTE,"fpuntadainferior");true.
	fallac(3,CABEZOTE):-estadoc(CABEZOTE,"fpuntadasuperior");true.
	fallac(4,CABEZOTE):-estadoc(CABEZOTE,"saltapuntada");true.
	fallac(5,CABEZOTE):-estadoc(CABEZOTE,"rompehilo");true.
	fallac(6,CABEZOTE):-estadoc(CABEZOTE,"quiebraaguja");true.
	fallac(7,CABEZOTE):-estadoc(CABEZOTE,"arrastremalo");true.
	fallac(8,CABEZOTE):-estadoc(CABEZOTE,"danamaterial");true.
	
	
	%%imprimir cabezote
	imprimirc(0,0,CABEZOTE):-imprimirc(1,1,CABEZOTE).
	imprimirc(1,N,CABEZOTE):-diagnosticoc(CABEZOTE,"sucio"),write("\n",N,". El Cabezote probablemente está Sucio \n"),M=N+1,imprimirc(2,M,CABEZOTE);
			        true,imprimirc(2,N,CABEZOTE).
	imprimirc(2,N,CABEZOTE):-diagnosticoc(CABEZOTE,"fpuntadainferior"),write("\n",N,". El Cabezote probalbemente tiene las siguientes fallas de PUNTADA SUPERIOR: \n"),
				write("  a.POCA TENSION EN LOS PLATOS\n"),
				write("  b.POCA TENSION EN POSTE DE TENSION\n  c.TRANSPORTADOR DESAJUSTADO\n  d.PIE DE PRENSATELA CON POCA PRESION\n  e.GARFIO DESAJUSTADO  \n"),
				M=N+1,imprimirc(3,M,CABEZOTE);
				
			        true,imprimirc(3,N,CABEZOTE).
	imprimirc(3,N,CABEZOTE):-diagnosticoc(CABEZOTE,"fpuntadasuperior"),write("\n",N,". El Cabezote probalbemente tiene las siguientes fallas de PUNTADA INFERIOR: \n"),
				write("  a.BOBINA CON POCA TENSION EN LA LAINA\n"),
				write("  b.BOBINA DAÑADA O AGRIETADA\n"),M=N+1,imprimirc(4,M,CABEZOTE);
				
			        true,imprimirc(4,N,CABEZOTE).
	imprimirc(4,N,CABEZOTE):-diagnosticoc(CABEZOTE,"saltapuntada"),write("\n",N,". El Cabezote probalbemente tiene las siguientes fallas de SALTO DE PUNTADA: \n"),
				write("  a.AGUJA MAL COLOCADA\n"),
				write("  b.GARFIO MAL AJUSTADO\n"),
				write("  c.HILO SUPERIOR CON MUCHA TENSION\n"),
				write("  d.PLACA DE LA AGUJA MAL AJUSTADA\n"),
				write("  e.PIE DE PRENSATELAS DESAJUSTADO\n"),
				write("  f.HILO, TELA Y AGUJA DESALINEADOS\n"),
				M=N+1,imprimirc(5,M,CABEZOTE);
				
			        true,imprimirc(5,N,CABEZOTE).
	imprimirc(5,N,CABEZOTE):-diagnosticoc(CABEZOTE,"rompehilo"),write("\n",N,". El Cabezote probalbemente tiene las siguientes fallas de ROMPER HILO: \n"),
				write("  a.DEMACIADA TENCION EN EL HILO\n"),
				write("  b.BOBINA MAL AJUSTADA O DAÑADA\n"),
				write("  c.HILO SUPERIOR CON MUCHA TENSION\n"),
				write("  d.HILO VIEJO O MUY FRAGIL\n"),
				write("  e.GARFIO DAÑADO, GOLPEADO, DESAJUSTADO O CON GRIETAS\n"),
				write("  f.DEDO RETENEDOR DESAJUSTADO\n"),
				write("  g.AGUJA MAL COLOCADA, AGRIETADA, CHUECHA O RECALENTADA\n"),
				write("  h.GUIAS DE LOS HILOS MAL COLOCADAS, AGRIETADAS, CHUECHAS O GOLPEADAS\n"),
				M=N+1,imprimirc(6,M,CABEZOTE);
				
			        true,imprimirc(6,N,CABEZOTE).
	imprimirc(6,N,CABEZOTE):-diagnosticoc(CABEZOTE,"quiebraaguja"),write("\n",N,". El Cabezote probalbemente tiene las siguientes fallas de ROMPER AGUJAS: \n"),
				write("  a.GARFIO MAL AJUSTADO\n"),
				write("  b.ALTURA DE LA BARRA DE LA AGUJA INCORRECTA\n"),
				write("  c.TRANSPORTADOR DESAJUSTADO\n"),
				write("  d.MEDIDA O NUMERO DE AGUJA INADECUADO\n"),
				M=N+1,imprimirc(7,M,CABEZOTE);
				
			        true,imprimirc(7,N,CABEZOTE).
	imprimirc(7,N,CABEZOTE):-diagnosticoc(CABEZOTE,"arrastremalo"),write("\n",N,". El Cabezote probalbemente tiene las siguientes fallas ARRASTRE IRREGULAR: \n"),
				write("  a.GARFIO MAL AJUSTADO\n"),
				write("  b.ALTURA DE LA BARRA DE LA AGUJA INCORRECTA\n"),
				write("  c.TRANSPORTADOR DESAJUSTADO\n"),
				write("  d.MEDIDA O NUMERO DE AGUJA INADECUADO\n"),
				M=N+1,imprimirc(8,M,CABEZOTE);
				
			        true,imprimirc(8,N,CABEZOTE).
	imprimirc(8,N,CABEZOTE):-diagnosticoc(CABEZOTE,"danamaterial"),write("\n",N,". El Cabezote probalbemente tiene las siguientes fallas DAÑA EL MATERIAL: \n"),
				write("  a.TRANSPORTADOR MAL AJUSTADO\n"),
				write("  b.MUCHO ACEITE, SUCIO O CON POCA BISCOCIDAD\n"),
				write("  c.AGUJA ROTA, INADECUADA, CON FISURAS O MAL AJUSTADA\n"),
				write("  d.TRANSPORTADOR INADECUADO O DESAJUSTADO\n"),
				write("  d.DIENTES CON MUCHO FILO\n"),
				M=N+1,imprimirc(9,M,CABEZOTE);
				
			        true,imprimirc(9,N,CABEZOTE).
	imprimirc(9,M,CABEZOTE):-M<2,write("\n___CABEZOTE__",CABEZOTE,"__SIN__FALLAS___\n DATOS INSUFICIENTES PARA DETERMINAR UNA FALLA\n____FALLA____DESONOCIDA____\n");
			     true.
goal
consult("D:\\motores.txt",motores),
consult("D:\\cabezotes.txt",cabezotes),
menu.
