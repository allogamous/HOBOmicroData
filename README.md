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

# **Considerações para processamento de informações meteorológicas**
Piracicaba, 25 de Fevereiro de 2019

# Frequência das leituras

As leituras são realizadas a cada 4 minutos, resultando numa estimativa média por hora (total de 15 leituras por hora). Assim, após o lançamento dessas médias para o computador, média diária pode ser contemplada de três modos:
1) média diária considerando periodo diurno e noturno;
2) média diária considerando horários chave (9:00h, 12:00h e 15:00h) comumente utilizados por órgãos como o INMET em leituras meteorológicas de estações convencionais.
3) processamento por critérios como máximo diário, mínimo diário, média absoluta (noturna e diurna) etc.
as leituras na estação começam a ser contabilizadas a partir do momento em que a estaçao é ativada (isto é, ocorre o "lançamento" da programaçao no software HOBO ware).
Edição do formato dos caracteres indicadores do horário de coleta
Como output da leitura dos dados da estação, são dados em coluna única o horário (hora, minutos e segundos) e dia em que foi realizada a coleta. Para processamento dos dados, essa coluna deve ser desmembrada em coluna de dia e coluna de hora.

# Sensor de radiação

A informação de radiação solar fotossinteticamente ativa (PAR, µmol/m²/s (LGR S/N: 20205293, SEN S/N: 20089792, LBL: Sensor Radiação PAR) assume valores 1,2 na ausência de radiação (isto é, no periodo noturno). Dá para explorar a radiação solar incidente convertendo PAR para SRAD (MJm2)

# Sensor de precipitação

A informação de precipitação acumulada (Chuva, mm (LGR S/N: 20205293, SEN S/N: 20327409) é obtida por um sensor que acumula até 2,00 mm por leitura. Portanto, é possível obter medidas horárias como acumulados da cada hora (e não média). Informações de irrigação não podem ser computadas, pois a altura do aspersor não alcança a boca do pluviômetro.

# Sensores de umidade no solo

A informação de umidade no solo (Teor de Água, m³/m³ (LGR S/N: 20205293, SEN S/N: 20316216) é obtida pela média de cada medida (15 medidas, uma a cada 4 minutos) em 1 hora. O sensor foi instalado na profundidade de 10cm. O sensor de temperatura do solo (Temp., °C (LGR S/N: 20205293, SEN S/N: 20269135, LBL: Sensor Temperatura) também foi instalado nessa mesma profundidade.

# Sensores de temperatura e umidade do ar

São obtidos pelos sensores [Temp., °C (LGR S/N: 20205293, SEN S/N: 20334393)] e [HR, % (LGR S/N: 20205293, SEN S/N: 20334393)]. Leituras a cada 4 minutos, média calculada por hora. Detalhe importante para instalação dos mesmos: devem pemanecer sempre em posição horizontal (perpendicular a torre da estação).

# Sensores de umidade de orvalho (ou ponto de orvalho)

A umidade de orvalho é estritamente relacionada a relação entre déficit de pressão de vapor atmosférico e temperatura atual da atmosfera.  Na micro estação são dadas informações em termos de temperatura de orvalho, isto é: temperatura mínima necessária para que a umidade presente no ar atmosférico se condense e forme orvalho sob as folhas. O sensor usado para este proposito é o PtCondensação, °C (LGR S/N: 20205293, SEN S/N: 20334393).

