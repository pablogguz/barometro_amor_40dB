
set scheme cleanplots 
set graph off

* Load data
	use "raw/03_barometro_amor.dta", clear
	
* Generate relevant variables
	* Recuerdo de voto
	labelbook recuerdo
	g voto = 1 if recuerdo == 1
	replace voto = 2 if recuerdo == 2
	replace voto = 3 if recuerdo == 3
	replace voto = 4 if recuerdo == 4
	replace voto = 5 if recuerdo == 5

	lab define voto 1 "PSOE" 2 "PP" 3 "Ciudadanos" 4 "Unidas Podemos" 5 "Vox", replace
	lab values voto voto
	lab var voto "Recuerdo de voto"
	
	* Orientación sexual
	g sex_orientation = 1 if p11_7 == 1
	forv y=1/6 {
		replace sex_orientation = 2 if p11_`y' == 1
	}
	lab define sex_orientation 1 "Heterosexual" 2 "LGTBIQ+", replace
	lab values sex_orientation sex_orientation
	
	* Tendrías una pareja con...
	forv y=1/8 {
		g p7_`y'_bin = (inlist(p7_`y',1,2)) if p7_`y'!=.
	}
	
	* Mi pareja actual es el amor más importante ed mi vida
	g gran_amor_couple = (p36==1) if p36 !=.
	lab var gran_amor_couple "Mi pareja actual es el amor más importante ed mi vida"
	
	* Prob. ruptura
	g probabilidad_ruptura = p37/10 if inrange(p37,0,10) & p37 != .
	lab var probabilidad_ruptura "Probabilidad de ruptura"
	
	* Satisfacción con la pareja
	g satisfaccion_pareja = p30 if inrange(p30,0,10) & p37 != .
	lab var satisfaccion_pareja "Satisfacción con la pareja"
	
	* Dummy para parejas
	tab p13
	g couple = 1 if inlist(p13,1,2,3,4,7,9)
	replace couple = 0 if inlist(p13,5,6,8,10)
	lab define couple 0 "Solteros" 1 "En pareja", replace
	lab values couple couple
	
	* Nunca han tenido pareja 
	g no_pareja_nunca = (inlist(p14,3,4)) 
	lab var no_pareja_nunca "Nunca han tenido pareja"

	* Cuernos en la actual pareja
	g cuernos = (p33_5==1) if couple == 1 // relaciones sexuales
	replace cuernos = 1 if p33_3 == 1 & couple == 1 // seducción con contacto físico (e.g., beso)
	lab var cuernos "Cuernos en la pareja"
	
	* Monogamia
	g monogamia = (p32==1) if couple == 1
	lab var monogamia "Relación monógama"
	
	* Cree que se es más feliz con pareja
	g mas_feliz_pareja = (inlist(p2primera,1))
	replace mas_feliz_pareja = 1 if p2segunda == 1
	lab var mas_feliz_pareja "Cree que se es más feliz con pareja"
	
	* Qué cosas harías por tu pareja 
	g infidelidad = (p6_1==1) if p4 == 1
	g dejar_trabajo = (p6_2==1) if p4 == 1
	g cambiar_cond = (p6_3==1) if p4 == 1
	g tener_hijos = (p6_4==1) if p4 == 1
	g otra_ciudad_pais = (p6_5==1) if p4 == 1
	replace otra_ciudad_pais = 1 if p6_6 == 1
	g amigos_familiares = (p6_7==1) if p4 == 1 
	
	* Ideología
	labelbook ideologia_r
	g ideologia_bis = 1 if inlist(ideologia_r,1,2)
	replace ideologia_bis = 2 if inlist(ideologia_r,3,4,5)
	replace ideologia_bis = 3 if inlist(ideologia_r,6,7)
	lab define ideologia_bis 1 "De izquierdas" 2 "De centro" 3 "De derechas"
	lab values ideologia_bis ideologia_bis
	
	* En mi relación actual, experimento...
	g falta_ilusion = (inlist(p34_1,1,2)) if couple == 1
	g afecto_comprension = (inlist(p34_2,1,2)) if couple == 1
	g celos = (inlist(p34_3,1,2)) if couple == 1
	g escucha_activa = (inlist(p34_4,1,2)) if couple == 1
	g admiracion_respeto = (inlist(p34_5,1,2)) if couple == 1
	g relaciones_placenteras = (inlist(p34_7,1,2)) if couple == 1
	g relaciones_noplacenteras = (inlist(p34_8,1,2)) if couple == 1
	
save "proc/amor_40db_shiny.dta", replace
