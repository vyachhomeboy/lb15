a<- list(
  name="ААААВТОМОБИЛЬ!",
  drive="ездит, еще как!",
  guditgudok="biip"
)
class(a)<-"employee"
print.employee<- function(wrkr){
  cat(wrkr$name, "\n")
  cat("Нормально", a$drive, "\n")
  cat("подает сигнал ", a$guditgudok, "\n")
  cat(wrkr$gas, "\n")
}

el<-list(
  name="Electrocar",
  gas="эко-френдли, нужно электричество"
)
class(el)<-c("gas", "employee")

sc<-list(
  name="Mercedesbenzscalss",
  gas="для бенза нужен бенз"
)
class(sc)<-c("gas", "employee")

kz<-list(
  name="Kamaz",
  gas="солярка"
)
class(kz)<-c("gas", "employee")
Vibor<-function(){
  z <- readline("Инфо о машинах:Electrocar, Mercedesbenzscalss, Kamaz. ")
  if(z == "Electrocar") print.employee(el)
  if(z == "Mercedesbenzscalss") print.employee(sc)
  if(z == "Kamaz") print.employee(kz)
}
Vibor()