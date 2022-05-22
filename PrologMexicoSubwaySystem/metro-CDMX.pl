%======================================================================================
%  metro-CDMX.pl
%      Ejercicio modelado del metro de la CDMX.  
%
%  Diego Gallo Valenzuela.
%  Febrero, 2021
%======================================================================================
%--------------------------------------
% color de cada línea...
%--------------------------------------
color(linea_1, rosa).
color(linea_2, azul_marino).
color(linea_3, verde_olivo).
color(linea_4, azul_cielo).
color(linea_5, amarillo).
color(linea_6, rojo).
color(linea_7, naranja).
color(linea_8, verde_bandera).
color(linea_9, cafe).
color(linea_a, morado).
color(linea_b, gris).
color(linea_12, dorado).
%--------------------------------------
% trayectos...
%--------------------------------------
trayecto(linea_1, observatorio, pantitlan).
trayecto(linea_2, tasquena, cuatro_caminos).
trayecto(linea_3, indios_verdes, universidad).
trayecto(linea_4, martin_carrera, santa_anita).
trayecto(linea_5, pantitlan, politecnico).
trayecto(linea_6, el_rosario, martin_carrera).
trayecto(linea_7, el_rosario, barranca_del_muerto).
trayecto(linea_8, garibaldi_lagunilla, constitucion_de_1917).
trayecto(linea_9, pantitlan, tacubaya).
trayecto(linea_a, pantitlan, la_paz).
trayecto(linea_b, buenavista, ciudad_azteca).
trayecto(linea_12, mixcoac, tlahuac).
%--------------------------------------
%línea 1: observatorio - pantitlan
%--------------------------------------
sigue(observatorio, tacubaya, linea_1).
sigue(tacubaya, juanacatlan, linea_1).
sigue(juanacatlan, chapultepec, linea_1).
sigue(chapultepec, sevilla, linea_1).
sigue(sevilla, insurgentes, linea_1).
sigue(insurgentes, cuauhtemoc, linea_1).
sigue(cuauhtemoc, balderas, linea_1).
sigue(balderas, salto_del_agua, linea_1).
sigue(salto_del_agua, isabel_la_catolica, linea_1).
sigue(isabel_la_catolica, pino_suarez, linea_1).
sigue(pino_suarez, merced, linea_1).
sigue(merced, candelaria, linea_1).
sigue(candelaria, san_lazaro, linea_1).
sigue(san_lazaro, moctezuma, linea_1).
sigue(moctezuma, balbuena, linea_1).
sigue(balbuena, blvd_puerto_aereo, linea_1).
sigue(blvd_puerto_aereo, gomez_farias, linea_1).
sigue(gomez_farias, zaragoza, linea_1).
sigue(zaragoza, pantitlan, linea_1).
%--------------------------------------
%línea 2: cuatro_caminos - tasquena
%--------------------------------------
sigue(cuatro_caminos, panteones, linea_2).
sigue(panteones, tacuba, linea_2).
sigue(tacuba, cuitlahuac, linea_2).
sigue(cuitlahuac, popotla, linea_2).
sigue(popotla, colegio_militar, linea_2).
sigue(colegio_militar, normal, linea_2).
sigue(normal, san_cosme, linea_2).
sigue(san_cosme, revolucion, linea_2).
sigue(revolucion, hidalgo, linea_2).
sigue(hidalgo, bellas_artes, linea_2).
sigue(bellas_artes, allende, linea_2).
sigue(allende, zocalo, linea_2).
sigue(zocalo, pino_suarez, linea_2).
sigue(pino_suarez, san_antonio_abad, linea_2).
sigue(san_antonio_abad, chabacano, linea_2).
sigue(chabacano, viaducto, linea_2).
sigue(viaducto, xola, linea_2).
sigue(xola, villa_de_cortas, linea_2).
sigue(villa_de_cortas, nativitas, linea_2).
sigue(nativitas, portales, linea_2).
sigue(portales, ermita, linea_2).
sigue(ermita, general_anaya, linea_2).
sigue(general_anaya, tasquena, linea_2).
%--------------------------------------
%línea 3: indios_verdes - universidad
%--------------------------------------
sigue(indios_verdes, deportivo_18_de_marzo, linea_3).
sigue(deportivo_18_de_marzo, potrero, linea_3).
sigue(potrero, la_raza, linea_3).
sigue(la_raza, tlatelolco, linea_3).
sigue(tlatelolco, guerrero, linea_3).
sigue(guerrero, hidalgo, linea_3).
sigue(hidalgo, juarez, linea_3).
sigue(juarez, balderas, linea_3).
sigue(balderas, ninos_heroes, linea_3).
sigue(ninos_heroes, hospital_general, linea_3).
sigue(hospital_general, centro_medico, linea_3).
sigue(centro_medico, etiopia-plaza_de_la_transparencia, linea_3).
sigue(etiopia-plaza_de_la_transparencia, eugenia, linea_3).
sigue(eugenia, division_del_norte, linea_3).
sigue(division_del_norte, zapata, linea_3).
sigue(zapata, coyoacan, linea_3).
sigue(coyoacan, viveros_derechos_humanos, linea_3).
sigue(viveros_derechos_humanos, miguel_angel_de_quevedo, linea_3).
sigue(miguel_angel_de_quevedo, copilco, linea_3).
sigue(copilco, universidad, linea_3).
%--------------------------------------
%línea 4: martin_carrera - santa_anita
%--------------------------------------
sigue(martin_carrera, talisman, linea_4).
sigue(talisman, bondojito, linea_4).
sigue(bondojito, consulado, linea_4).
sigue(consulado, canal_del_norte, linea_4).
sigue(canal_del_norte, morelos, linea_4).
sigue(morelos, candelaria, linea_4).
sigue(candelaria, fray_servando, linea_4).
sigue(fray_servando, jamaica, linea_4).
sigue(jamaica, santa_anita, linea_4).
%--------------------------------------
%línea 5: politecnico - pantitlan
%--------------------------------------
sigue(politecnico, instituto_del_petroleo, linea_5).
sigue(instituto_del_petroleo, autobuses_del_norte, linea_5).
sigue(autobuses_del_norte, la_raza, linea_5).
sigue(la_raza, misterios, linea_5).
sigue(misterios, valle_gomez, linea_5).
sigue(valle_gomez, consulado, linea_5).
sigue(consulado, eduardo_molina, linea_5).
sigue(eduardo_molina, aragon, linea_5).
sigue(aragon, oceania, linea_5).
sigue(oceania, terminal_aerea, linea_5).
sigue(terminal_aerea, hangares, linea_5).
sigue(hangares, pantitlan, linea_5).
%--------------------------------------
%línea 6: el_rosario - martin_carrera
%--------------------------------------
sigue(el_rosario, tezozomoc, linea_6).
sigue(tezozomoc, uam-azcapotzalco, linea_6).
sigue(uam-azcapotzalco, ferreria/arena_ciudad_de_mexico, linea_6).
sigue(ferreria/arena_ciudad_de_mexico, norte_45, linea_6).
sigue(norte_45, vallejo, linea_6).
sigue(vallejo, instituto_del_petroleo, linea_6).
sigue(instituto_del_petroleo, lindavista, linea_6).
sigue(lindavista, deportivo_18_de_marzo, linea_6).
sigue(deportivo_18_de_marzo, la_villa-basilica, linea_6).
sigue(la_villa-basilica, martin_carrera, linea_6).
%--------------------------------------
%línea 7: el_rosario - barranca_del_muerto
%--------------------------------------
sigue(el_rosario, aquiles_serdan, linea_7).
sigue(aquiles_serdan, camarones, linea_7).
sigue(camarones, refineria, linea_7).
sigue(refineria, tacuba, linea_7).
sigue(tacuba, san_joaquin, linea_7).
sigue(san_joaquin, polanco, linea_7).
sigue(polanco, auditorio, linea_7).
sigue(auditorio, constituyentes, linea_7).
sigue(constituyentes, tacubaya, linea_7).
sigue(tacubaya, san_pedro_de_los_pinos, linea_7).
sigue(san_pedro_de_los_pinos, san_antonio, linea_7).
sigue(san_antonio, mixcoac, linea_7).
sigue(mixcoac, barranca_del_muerto, linea_7).
%--------------------------------------
%línea 8: garibaldi-lagunilla - constitucion_de_1917
%--------------------------------------
sigue(garibaldi-lagunilla, bellas_artes, linea_8).
sigue(bellas_artes, san_juan_de_letran, linea_8).
sigue(san_juan_de_letran, salto_del_agua, linea_8).
sigue(salto_del_agua, doctores, linea_8).
sigue(doctores, obrera, linea_8).
sigue(obrera, chabacano, linea_8).
sigue(chabacano, la_viga, linea_8).
sigue(la_viga, santa_anita, linea_8).
sigue(santa_anita, coyuya, linea_8).
sigue(coyuya, iztacalco, linea_8).
sigue(iztacalco, apatlaco, linea_8).
sigue(apatlaco, aculco, linea_8).
sigue(aculco, escuadron_201, linea_8).
sigue(escuadron_201, atlalilco, linea_8).
sigue(atlalilco, iztapalapa, linea_8).
sigue(iztapalapa, cerro_de_la_estrella, linea_8).
sigue(cerro_de_la_estrella, uam-i, linea_8).
sigue(uam-i, constitucion_de_1917, linea_8).
%--------------------------------------
%línea 9: tacubaya - pantitlan
%--------------------------------------
sigue(tacubaya, patriotismo, linea_9).
sigue(patriotismo, chilpancingo, linea_9).
sigue(chilpancingo, centro_medico, linea_9).
sigue(centro_medico, lazaro_cardenas, linea_9).
sigue(lazaro_cardenas, chabacano, linea_9).
sigue(chabacano, jamaica, linea_9).
sigue(jamaica, mixiuhca, linea_9).
sigue(mixiuhca, velodromo, linea_9).
sigue(velodromo, ciudad_deportiva, linea_9).
sigue(ciudad_deportiva, puebla, linea_9).
sigue(puebla, pantitlan, linea_9).
%--------------------------------------
%línea a: pantitlan - la_paz
%--------------------------------------
sigue(pantitlan, agricola_oriental, linea_a).
sigue(agricola_oriental, canal_de_san_juan, linea_a).
sigue(canal_de_san_juan, tepalcates, linea_a).
sigue(tepalcates, guelatao, linea_a).
sigue(guelatao, penon_viejo, linea_a).
sigue(penon_viejo, acatitla, linea_a).
sigue(acatitla, santa_marta, linea_a).
sigue(santa_marta, los_reyes, linea_a).
sigue(los_reyes, la_paz, linea_a).
%--------------------------------------
%línea b: buenavista - ciudad_azteca
%--------------------------------------
sigue(buenavista, guerrero, linea_b).
sigue(guerrero, garibaldi-lagunilla, linea_b).
sigue(garibaldi-lagunilla, lagunilla, linea_b).
sigue(lagunilla, tepito, linea_b).
sigue(tepito, morelos, linea_b).
sigue(morelos, san_lazaro, linea_b).
sigue(san_lazaro, ricardo_flores_magon, linea_b).
sigue(ricardo_flores_magon, romero_rubio, linea_b).
sigue(romero_rubio, oceania, linea_b).
sigue(oceania, deportivo_oceania, linea_b).
sigue(deportivo_oceania, bosque_de_aragon, linea_b).
sigue(bosque_de_aragon, villa_de_aragon, linea_b).
sigue(villa_de_aragon, nezahualcoyotl, linea_b).
sigue(nezahualcoyotl, impulsora, linea_b).
sigue(impulsora, rio_de_los_remedios, linea_b).
sigue(rio_de_los_remedios, muzquiz, linea_b).
sigue(muzquiz, ecatepec, linea_b).
sigue(ecatepec, olimpica, linea_b).
sigue(olimpica, plaza_aragon, linea_b).
sigue(plaza_aragon, ciudad_azteca, linea_b).
%--------------------------------------
%línea 12: tlahuac - mixcoac
%--------------------------------------
sigue(tlahuac, tlaltenco, linea_12).
sigue(tlaltenco, zapotitlan, linea_12).
sigue(zapotitlan, nopalera, linea_12).
sigue(nopalera, olivos, linea_12).
sigue(olivos, tezonco, linea_12).
sigue(tezonco, periferico_oriente, linea_12).
sigue(periferico_oriente, calle_11, linea_12).
sigue(calle_11, lomas_estrella, linea_12).
sigue(lomas_estrella, san_andres_tomatlan, linea_12).
sigue(san_andres_tomatlan, culhuacan, linea_12).
sigue(culhuacan, atlalilco, linea_12).
sigue(atlalilco, mexicaltzingo, linea_12).
sigue(mexicaltzingo, ermita, linea_12).
sigue(ermita, eje_central, linea_12).
sigue(eje_central, parque_de_los_venados, linea_12).
sigue(parque_de_los_venados, zapata, linea_12).
sigue(zapata, hospital_20_de_noviembre, linea_12).
sigue(hospital_20_de_noviembre, insurgentes_sur, linea_12).
sigue(insurgentes_sur, mixcoac, linea_12).
%--------------------------------------
% Reglas...
%--------------------------------------
%--------------------------------------
% Conecta...
%--------------------------------------
conecta(X, Y, L):- sigue(X, Y, L); sigue(Y, X, L).
%--------------------------------------
% Cerca...
%--------------------------------------
cerca(X, Y):- conecta(X, Y,_).
cerca(X, Y):- conecta(X, Z,_),conecta(Z, Y,_).
%--------------------------------------
% Alcanza: la primera o, a lo mas, hasta 
% dos estaciones...
%--------------------------------------
alcanza(X, Y):- conecta(X, Y,_).
alcanza(X, Y):- conecta(X, Z,_),conecta(Z, Y,_).
alcanza(X, Y):- conecta(X, Z1,_),
                conecta(Z1, Z2,_),
                conecta(Z2, Y,_).
alcanza(X, Y):- conecta(X, Z1,_),
                conecta(Z1, Z2,_),
                conecta(Z2, Z3,_),
                conecta(Z3, Y,_).
%--------------------------------------
% Alcanzable...
%--------------------------------------
alcanzable(X, Y):- conecta(X, Y, L).
alcanzable(X, Y):- conecta(X, Z, _), alcanzable(Z, Y).
% alcanzable(X, Y, [Z]):- conecta(X, Y, L).
% alcanzable(X, Y, [Z | Resto]):- conecta(X, Aux), alcanzable(Aux, Y, Resto).
%--------------------------------------
% Member...
%--------------------------------------
member(X, L)
% TEST member(chabacano, [chabacano, tacubaya, observatorio]).
% TEST L=[_,_,_], member(tacubaya, L),member(observatorio, L),member(juanacatlan, L).
%--------------------------------------
% append...
%--------------------------------------
% append([buenavista,guerrero],[tezozomoc,el_rosario,talisman],[buenavista,guerrero,tezozomoc,el_rosario,talisman]).