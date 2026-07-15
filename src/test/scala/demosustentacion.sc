// DEMO EN VIVO - SUSTENTACIÓN
//
//   1) Se define la configuración: escala likert5, rho secuencial/paralelo,
//      un n chico y un n grande.
//   2) Se corre compararMedidasPol con n chico (8) y n grande (512) para
//      mostrar que rho y rhoPar dan EXACTAMENTE el mismo resultado.
//   3) Se corre compararFuncionesAct con confBiasUpdate vs confBiasUpdatePar,
//      mismo n chico y grande, usando el grafo denso i2 (el mismo que se
//      usó para generar la Tabla 2 del PDF).
//   4) Se imprime la columna de aceleración (speedup) y se compara contra
//      lo que dice la Tabla 2: zona muerta en n=64-128, gana desde n≈256.

import Comete._
import Opinion._
import Benchmark._

// configuración

val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)   // misma escala de 5 puntos detodo el proyecto
val rhoSec  = rho(1.2, 1.2)                        // medida de polarización secuencial
val rhoConc = rhoPar(1.2, 1.2)                     // misma medida, versión paralela (datos)

val nChico  = 8
val nGrande = 512

// Creencia inicial: comunidad moderadamente dividida (mitad en contra, mitad a favor)
val sbChico  = midlyBelief(nChico)
val sbGrande = midlyBelief(nGrande)

println(s"n chico  = $nChico  -> sb = $sbChico")
println(s"n grande = $nGrande -> primeros 5 valores de sb = ${sbGrande.take(5)} ...")

// rho vs rhoPar -> deben dar el MISMO resultado

println("PASO 2: compararMedidasPol -> rho (secuencial) vs rhoPar (concurrente)")

val resultadosPol = compararMedidasPol(Seq(sbChico, sbGrande), likert5, rhoSec, rhoConc)

println(f"${"n"}%8s ${"pol_seq"}%14s ${"pol_par"}%14s ${"t_seq(ms)"}%14s ${"t_par(ms)"}%14s ${"speedup"}%10s")
resultadosPol.foreach { case (n, p1, p2, t1, t2, sp) =>
  println(f"$n%8d ${p1}%14.8f ${p2}%14.8f ${t1.value}%14.4f ${t2.value}%14.4f ${sp}%10.3f")
}

// Verificación explícita de corrección
resultadosPol.foreach { case (n, p1, p2, _, _, _) =>
  val diferencia = math.abs(p1 - p2)
  println(f"  n=$n%-6d |pol_seq - pol_par| = $diferencia%.2e  -> " +
    (if (diferencia < 1e-9) "COINCIDEN ✔" else "!! NO COINCIDEN"))
}

// confBiasUpdate vs confBiasUpdatePar (grafo denso i2, igual que Tabla 2)
println("PASO 3: compararFuncionesAct -> confBiasUpdate vs confBiasUpdatePar (grafo i2)")

val swgChico  = i2(nChico)
val swgGrande = i2(nGrande)

val resActChico  = compararFuncionesAct(Seq(sbChico), swgChico, confBiasUpdate, confBiasUpdatePar)
val resActGrande = compararFuncionesAct(Seq(sbGrande), swgGrande, confBiasUpdate, confBiasUpdatePar)
val resultadosAct = resActChico ++ resActGrande

println(f"${"n"}%8s ${"t_seq(ms)"}%14s ${"t_par(ms)"}%14s ${"speedup"}%10s")
resultadosAct.foreach { case (n, t1, t2, sp) =>
  println(f"$n%8d ${t1.value}%14.4f ${t2.value}%14.4f ${sp}%10.3f")
}

//columna de aceleración y conectarla con la Tabla 2
println("PASO 4: lectura de resultados (para decir en voz alta)")

resultadosAct.foreach { case (n, _, _, sp) =>
  val lectura =
    if (sp < 1.0) "pierde frente al secuencial (overhead de hilos no se amortiza)"
    else "gana frente al secuencial"
  println(f"  n=$n%-6d speedup=$sp%.3f  -> $lectura")
}

println(
  """
    |Conexión con la Tabla 2 del PDF (grafo denso i2):
    | - Con n chico (8) el trabajo por agente es mínimo, así que el costo fijo
    |   de repartir en hilos NO se amortiza: se espera speedup <= 1 (zona muerta).
    | - Con n grande (512) ya estamos en la zona donde el umbral de partición (32)
    |   permite que confBiasUpdatePar compense el overhead y gane de forma sostenida,
    |   tal como se ve en la Tabla 2 desde n≈256 en adelante.
    |""".stripMargin)

println("Demo terminada. rho/rhoPar coinciden en valor; confBiasUpdatePar gana en n grande.")