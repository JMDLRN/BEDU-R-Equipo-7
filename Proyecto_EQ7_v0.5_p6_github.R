"6 Escribe tu análisis en un archivo README.MD y tu código en un script de R y
publica ambos en un repositorio de Github."
#Librería RStudioAPI
library(rstudioapi)

#Commit inicial
myTerm <- terminalCreate(caption = "GIT", shellType = "win-git-bash")
terminalSend(myTerm, "git config --global user.name 'JMDLRN'\n")
terminalSend(myTerm, "git config --global user.email 'manuel.delarosa.nava@bsci.com'\n")
terminalSend(myTerm, "git status\n")
terminalSend(myTerm, "git add .\n")
terminalSend(myTerm, "git commit -m 'Commit de proyecto final'\n")
terminalSend(myTerm, "git branch -M main\n")
terminalSend(myTerm, "git remote add origin https://github.com/JMDLRN/BEDU-R-Equipo-7.git\n")
terminalSend(myTerm, "git push -u origin main\n")

#Cambios desde Github
terminalSend(myTerm, "git pull origin main\n")