1. Archivos entregados
----------------------
- Paquete Comete/
  Contiene funciones para:
  * Definir distribuciones y frecuencias de agentes.
  * Calcular medidas de polarización (rhoCMT_Gen).
  * Normalizar medidas de polarización.
  * Algoritmos auxiliares como min_p para optimización.

- Paquete Opinion/
  Contiene funciones para:
  * Definir creencias específicas de agentes (SpecificBelief).
  * Calcular contribuciones de influencia entre agentes.
  * Medidas de polarización secuenciales y paralelas (rho, rhoPar).
  * Actualización de creencias (confBiasUpdate y confBiasUpdatePar).
  * Simulación de evolución de opiniones (simulate).
  * Construcción y visualización de grafos de influencia.

  - INFORME PROYECTO FPFC.pdf  
  Documento escrito con la explicación teórica, objetivos y análisis del proyecto.

- pruebas.sc  
  Worksheet de Scala que:
  * Genera tablas comparativas de desempeño (rho vs rhoPar, confBiasUpdate vs confBiasUpdatePar).
  * Ejecuta simulaciones completas con distintos tamaños de red y tiempos.
  * Imprime resultados en consola y exporta a CSV para incluir en el informe.
  * Permite validar corrección y medir speedup de la versión concurrente frente a la secuencial.

  3. Instrucciones de ejecución
-----------------------------
1. Abrir una terminal en la carpeta raíz del proyecto.
2. Compilar los paquetes con:
   sbt compile
3. Ejecutar simulaciones con:
   sbt run
   Esto generará archivos HTML (ejemplo: simulEvol.html) con gráficas interactivas.
4. Para correr las pruebas:
   - Abrir el archivo pruebas.sc en IntelliJ (ubicado en test/scala/).
   - Ejecutar el worksheet para generar tablas comparativas y archivos CSV.
   - Los CSV se guardan en la carpeta del proyecto y pueden usarse directamente en el informe.
