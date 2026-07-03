

import Comete._
import Opinion._
import common._
import Benchmark._
import java.io._

// ----------------------------------------------------------------------------
// 0. UTILIDADES DE IMPRESIÓN Y EXPORTACIÓN
// ----------------------------------------------------------------------------

def imprimirFila(cols: Seq[Any], anchos: Seq[Int]): Unit = {
  val fila = cols.zip(anchos).map { case (v, w) =>
    val s = v match {
      case d: Double => f"$d%.4f"
      case q: org.scalameter.Quantity[Double] => f"${q.value}%.4f"
      case other => other.toString
    }
    s.padTo(w, ' ')
  }.mkString
  println(fila)
}

def exportarCSV(nombreArchivo: String, encabezados: Seq[String], filas: Seq[Seq[Any]]): Unit = {
  val pw = new PrintWriter(new File(nombreArchivo))
  pw.println(encabezados.mkString(","))
  filas.foreach { fila =>
    val vals = fila.map {
      case q: org.scalameter.Quantity[Double] => q.value.toString
      case other => other.toString
    }
    pw.println(vals.mkString(","))
  }
  pw.close()
  println(s"  -> exportado a $nombreArchivo")
}

// ----------------------------------------------------------------------------
// 1. CONFIGURACIÓN GENERAL
// ----------------------------------------------------------------------------

val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)
val rhoSec  = rho(1.2, 1.2)
val rhoConc = rhoPar(1.2, 1.2)

// Tamaños de red a evaluar (potencias de 2). Ajusta potenciaMaxPol /
// potenciaMaxAct según cuánto tiempo/RAM tenga tu máquina: rho es liviana,
// pero confBiasUpdate es O(n^2) en el peor caso (grafo denso tipo i2),
// así que conviene un rango más chico para esa comparación.
val potenciaMinPol = 2   // 2^2  = 4 agentes
val potenciaMaxPol = 15  // hasta 2^14 = 16384 agentes (exclusivo)

val potenciaMinAct = 2
val potenciaMaxAct = 11  // hasta 2^10 = 1024 agentes (exclusivo) -> subir con cuidado

def tamanos(potMin: Int, potMax: Int): Seq[Int] =
  for (n <- potMin until potMax) yield math.pow(2, n).toInt

// Generadores de creencias iniciales a comparar.
// NOTA: se califican explícitamente con "Benchmark." porque estas mismas
// funciones también existen (duplicadas) en el paquete Opinion, lo que
// genera un "cannot resolve overloaded method" si se dejan sin calificar.
// Lo ideal es eliminar la duplicación en Opinion/package.scala, ya que esas
// funciones no están en la plantilla del enunciado para ese paquete (sección
// 4.2) — pero mientras tanto esta calificación resuelve el error.
val generadores: Map[String, Int => SpecificBelief] = Map(
  "uniforme"  -> Benchmark.uniformBelief,
  "extrema"   -> Benchmark.allExtremeBelief,
  "triple"    -> Benchmark.allTripleBelief,
  "moderada"  -> Benchmark.midlyBelief
)

// ----------------------------------------------------------------------------
// 2. TABLA 1: rho vs rhoPar (medida de polarización)
// ----------------------------------------------------------------------------

println("\n" + "=" * 78)
println("TABLA 1: rho (secuencial) vs rhoPar (concurrente)")
println("=" * 78)

for ((nombreGen, gen) <- generadores) {
  val sbms = tamanos(potenciaMinPol, potenciaMaxPol).map(gen)
  val cmp  = compararMedidasPol(sbms, likert5, rhoSec, rhoConc)

  println(s"\n--- Creencia inicial: $nombreGen ---")
  imprimirFila(Seq("n", "pol_seq", "pol_par", "t_seq(ms)", "t_par(ms)", "speedup"),
    Seq(10, 12, 12, 14, 14, 10))

  // Validación de corrección: pol_seq y pol_par deben coincidir
  var inconsistencias = 0
  cmp.foreach { case (n, p1, p2, t1, t2, sp) =>
    imprimirFila(Seq(n, p1, p2, t1, t2, sp), Seq(10, 12, 12, 14, 14, 10))
    if (math.abs(p1 - p2) > 1e-9) inconsistencias += 1
  }
  if (inconsistencias > 0)
    println(s"  !! ADVERTENCIA: $inconsistencias caso(s) con pol_seq != pol_par (revisar corrección)")

  val filasCsv = cmp.map { case (n, p1, p2, t1, t2, sp) => Seq(n, p1, p2, t1, t2, sp) }
  exportarCSV(s"resultados_rho_$nombreGen.csv",
    Seq("n", "pol_seq", "pol_par", "t_seq_ms", "t_par_ms", "speedup"),
    filasCsv)

  val speedupProm = cmp.map(_._6).sum / cmp.length
  println(f"  Speedup promedio ($nombreGen): $speedupProm%.4f")
}

// ----------------------------------------------------------------------------
// 3. TABLA 2: confBiasUpdate vs confBiasUpdatePar
// ----------------------------------------------------------------------------

println("\n" + "=" * 78)
println("TABLA 2: confBiasUpdate (secuencial) vs confBiasUpdatePar (concurrente)")
println("=" * 78)

// Grafos de influencia a comparar: i1 (disperso) e i2 (denso)
val grafos: Map[String, Int => SpecificWeightedGraph] = Map("i1" -> i1, "i2" -> i2)

for ((nombreGrafo, gGen) <- grafos; (nombreGen, gen) <- generadores) {
  println(s"\n--- Creencia: $nombreGen | Grafo: $nombreGrafo ---")
  imprimirFila(Seq("n", "t_seq(ms)", "t_par(ms)", "speedup"), Seq(10, 14, 14, 10))

  val resultados = for (n <- tamanos(potenciaMinAct, potenciaMaxAct)) yield {
    val sb  = gen(n)
    val swg = gGen(n)                       // grafo del MISMO tamaño que sb
    val cmp = compararFuncionesAct(Seq(sb), swg, confBiasUpdate, confBiasUpdatePar)
    cmp.head                                 // (n, t1, t2, speedup)
  }

  resultados.foreach { case (n, t1, t2, sp) =>
    imprimirFila(Seq(n, t1, t2, sp), Seq(10, 14, 14, 10))
  }

  val filasCsv = resultados.map { case (n, t1, t2, sp) => Seq(n, t1, t2, sp) }
  exportarCSV(s"resultados_confBias_${nombreGen}_$nombreGrafo.csv",
    Seq("n", "t_seq_ms", "t_par_ms", "speedup"),
    filasCsv)

  val speedupProm = resultados.map(_._4).sum / resultados.length
  println(f"  Speedup promedio: $speedupProm%.4f")
}

// ----------------------------------------------------------------------------
// 4. TABLA 3: simulación completa (simulate con confBiasUpdate vs Par)
// ----------------------------------------------------------------------------

println("\n" + "=" * 78)
println("TABLA 3: simulate secuencial vs paralelo (t unidades de tiempo)")
println("=" * 78)

val tiempoSim = 10
val potenciaMinSim = 2
val potenciaMaxSim = 10  // hasta 2^9 = 512 agentes; una simulación repite confBiasUpdate t veces

for ((nombreGen, gen) <- generadores) {
  println(s"\n--- Creencia inicial: $nombreGen | grafo i1 | t=$tiempoSim ---")
  imprimirFila(Seq("n", "t_seq(ms)", "t_par(ms)", "speedup"), Seq(10, 14, 14, 10))

  val filas = for (n <- tamanos(potenciaMinSim, potenciaMaxSim)) yield {
    val sb  = gen(n)
    val swg = i1(n)
    val tSec = tiempoDe(simulate(confBiasUpdate, swg, sb, tiempoSim))
    val tPar = tiempoDe(simulate(confBiasUpdatePar, swg, sb, tiempoSim))
    val sp   = tSec.value / tPar.value
    imprimirFila(Seq(n, tSec, tPar, sp), Seq(10, 14, 14, 10))
    Seq(n, tSec, tPar, sp)
  }

  exportarCSV(s"resultados_simulate_$nombreGen.csv",
    Seq("n", "t_seq_ms", "t_par_ms", "speedup"),
    filas)
}

// ----------------------------------------------------------------------------
// 5. Gráficas HTML de evolución de la polarización
//    Usa simEvolucion del paquete Benchmark; genera un .html por creencia.
// ----------------------------------------------------------------------------

val sbmsGraf = tamanos(2, 8).map(midlyBelief) // tamaños pequeños para que el HTML sea legible
val grafoGraf = i1(sbmsGraf.map(_.length).max)

for (i <- sbmsGraf.indices) {
  simEvolucion(
    Seq(sbmsGraf(i)), grafoGraf, tiempoSim, rhoSec, confBiasUpdate, likert5,
    s"Evolucion-Secuencial-$i-${sbmsGraf(i).length}"
  )
}

println("\nAnálisis completo. Revisa los .csv generados en la carpeta del proyecto")
println("y los .html de evolución para incluir capturas en el informe.")

// ----------------------------------------------------------------------------
// 6. TABLA 3 (versión completa): simulate variando n Y t
// ----------------------------------------------------------------------------

val tamanosSim2 = tamanos(2, 10)      // 4, 8, 16, ..., 512 agentes
val tiemposSim2 = Seq(5, 10, 20)      // unidades de tiempo simuladas

println("\n" + "=" * 78)
println("TABLA 3 (n x t): simulate secuencial vs paralelo")
println("=" * 78)

for ((nombreGen, gen) <- generadores) {
  println(s"\n--- Creencia inicial: $nombreGen | grafo i1 ---")
  imprimirFila(Seq("n", "t", "t_seq(ms)", "t_par(ms)", "speedup"), Seq(8, 6, 14, 14, 10))

  val filasNT = for {
    n <- tamanosSim2
    t <- tiemposSim2
  } yield {
    val sb   = gen(n)
    val swg  = i1(n)
    val tSec = tiempoDe(simulate(confBiasUpdate, swg, sb, t))
    val tPar = tiempoDe(simulate(confBiasUpdatePar, swg, sb, t))
    val sp   = tSec.value / tPar.value
    imprimirFila(Seq(n, t, tSec, tPar, sp), Seq(8, 6, 14, 14, 10))
    Seq(n, t, tSec, tPar, sp)
  }

  exportarCSV(s"resultados_simulate_nt_$nombreGen.csv",
    Seq("n", "t", "t_seq_ms", "t_par_ms", "speedup"),
    filasNT)
}

// ----------------------------------------------------------------------------
// 6b. (OPCIONAL, si hay tiempo) Versión con repeticiones -> media +/- desv. std.
// Correr esta versión en vez de la 6 si quieres reportar variabilidad real
// entre corridas independientes, en lugar de un solo número por celda.
// ----------------------------------------------------------------------------

def mediaYDesv(valores: Seq[Double]): (Double, Double) = {
  val media = valores.sum / valores.length
  val varianza = valores.map(v => math.pow(v - media, 2)).sum / valores.length
  (media, math.sqrt(varianza))
}

val repeticiones = 5

for ((nombreGen, gen) <- generadores) {
  println(s"\n--- (robusto, $repeticiones corridas) Creencia: $nombreGen | grafo i1 ---")
  imprimirFila(Seq("n", "t", "t_seq_prom", "t_seq_std", "t_par_prom", "t_par_std", "speedup"),
    Seq(8, 6, 12, 10, 12, 10, 10))

  val filasRobustas = for {
    n <- tamanosSim2
    t <- tiemposSim2
  } yield {
    val sb  = gen(n)
    val swg = i1(n)
    val tsSec = (1 to repeticiones).map(_ => tiempoDe(simulate(confBiasUpdate, swg, sb, t)).value)
    val tsPar = (1 to repeticiones).map(_ => tiempoDe(simulate(confBiasUpdatePar, swg, sb, t)).value)
    val (mSec, sSec) = mediaYDesv(tsSec)
    val (mPar, sPar) = mediaYDesv(tsPar)
    val sp = mSec / mPar
    imprimirFila(Seq(n, t, mSec, sSec, mPar, sPar, sp), Seq(8, 6, 12, 10, 12, 10, 10))
    Seq(n, t, mSec, sSec, mPar, sPar, sp)
  }

  exportarCSV(s"resultados_simulate_nt_robusto_$nombreGen.csv",
    Seq("n", "t", "t_seq_prom_ms", "t_seq_std_ms", "t_par_prom_ms", "t_par_std_ms", "speedup_prom"),
    filasRobustas)
}
