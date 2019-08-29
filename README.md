# **Hoboware**
Pasta criada para salvar scripts (em R) para coleta, processamento e organizacao de dados oriundos da estação micrometeorológica HOBOWARE. A estação é baseada no software homonimo cujos detalhes sobre seu uso, manuais de instalação, coleta e processamento de informações estão facilmente disponíveis na internet e no manual (impresso) no lab.

# **Scripts**

Os scripts presentes nesta pasta contem as funções:

*read.HOBO()* (leitura de arquivos .csv processados pelo HOBOWARE);

*process.HOBO()* (processamento inicial dos dados em escala diaria (o default programado da estacao está em escala horária)

*weatherReady()* (sim, é um plágio descarado do snpReady. Serve para gerar, de modo simples, os arquivos output meteorologicos).

**Atenção**: instale os pacotes *reshape2* e *plyr*

# **Gestão**
*2019- atualmente*: Germano Martins F.Costa-Neto
