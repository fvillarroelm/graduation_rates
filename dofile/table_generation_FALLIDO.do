clear all
set more off

*Directorios
global main "C:/Users/fvillarroel/Downloads/pega/pega_cristobal/tarea1/project"
global tables "${main}/tables"
global graphs "${main}/graphs"
global rawdata "${main}/raw_data"
global workingdata "${main}/working_data"

*directorio
cd $main

*instala paquetes que use y no tengas instalado.
foreach ado in gtools{
cap which `ado'
if _rc!=0 ssc install `ado'
}

**working dataset
use "${workingdata}/working_data.dta",replace

/*Dura = Duracion en años
Depe = Dependencia
Inst = Tipo institución
Area = Área del conocimiento
p = participación
*/


*N_area (denominador)
bys area_conocimiento : egen N_area = count(area_conocimiento)

*participacionAreaDuraDepeInst
bys area_conocimiento duracion_total_anos dependencia tipo_inst : egen pAreaDuraDepeInst = count(area_conocimiento)

*participacionAreaDuraDepe
bys area_conocimiento duracion_total_anos dependencia: egen pAreaDuraDepe = count(area_conocimiento)

*participacionAreaDepeInst
bys area_conocimiento dependencia tipo_inst: egen pAreaDepeInst = count(area_conocimiento)

*participacionAreaDepe
bys area_conocimiento dependencia: egen pAreaDepe = count(area_conocimiento)

*Porcentajes
foreach x of varlist pAreaDuraDepeInst pAreaDuraDepe pAreaDepeInst pAreaDepe{
replace `x' = (`x' / N_area) *100
}

**columna total
*participacionAreaDuraInst
bys area_conocimiento duracion_total_anos tipo_inst : egen pAreaDuraInst = count(area_conocimiento)

*participacionAreaDura
bys area_conocimiento duracion_total_anos: egen pAreaDura = count(area_conocimiento)

*participacionAreaInst
bys area_conocimiento tipo_inst: egen pAreaInst = count(area_conocimiento)

*TotalParticipacionArea
bys area_conocimiento: egen TotalPArea = count(area_conocimiento)

*Porcentajes
foreach x of varlist pAreaDuraInst pAreaDura pAreaInst TotalPArea{
replace `x' = (`x' / N_area) *100
}




egen depe_inst = concat(dependencia tipo_inst), punct(_)

egen area_duracion = concat(area_conocimiento duracion_total_anos), punct(_)

keep area_duracion depe_inst pAreaDuraInst pAreaDura pAreaInst TotalPArea pAreaDuraDepeInst pAreaDuraDepe pAreaDepeInst pAreaDepe

duplicates drop

sort area_duracion depe_inst
/* Acá es donde empieza a fallar

*Debo condensar todas las variables de participación en una sola (formato largo)
greshape gather p* Total*, values(participacion) keys(grupo)

egen depe_inst_grupo = concat(depe_inst grupo), punct(_)
drop depe_inst grupo

*Si no hago lo anterior, no puedo pasar nada a formato ancho (sólo puede pasar a formato ancho a partir de una columna de valores. No aguanta múltiples).
greshape spread participacion, keys(depe_inst_grupo) by(area_duracion)

*El comando sirve, pero la tabla no queda ordenada.

*/



**fila total
*denominador
egen N = count(area_conocimiento)

*participacionDuraDepeInst
bys duracion_total_anos dependencia tipo_inst: egen participacionDuraDepeInst = count(area_conocimiento)

*participacionDuraDepe
bys duracion_total_anos dependencia: egen participacionDuraDepe = count(area_conocimiento)

*participacionDepeInst
bys dependencia tipo_inst: egen participacionDepeInst = count(area_conocimiento)

*participacionDepe
bys dependencia: egen participacionDepe = count(area_conocimiento)

*Porcentajes
foreach x of varlist participacionDuraDepeInst participacionDuraDepe participacionDepeInst participacionDepe{
replace `x' = (`x' / N) *100
}

**columna total de fila total
*participacionDuraInst
bys duracion_total_anos tipo_inst: egen participacionDuraInst = count(area_conocimiento)

*participacionDura
bys duracion_total_anos: egen participacionDura = count(area_conocimiento)

*participacionInst
bys tipo_inst: egen participacionInst = count(area_conocimiento)

*Total del total
gen Total_Total = (N/N)*100

*Porcentajes
foreach x of varlist participacionDuraInst participacionDura participacionInst{
replace `x' = (`x' / N) *100
}