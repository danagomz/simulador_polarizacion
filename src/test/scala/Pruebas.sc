// ============================================================
// PRUEBAS.SC — Archivo compartido de pruebas
// REGLA: cada integrante solo edita su sección
// ============================================================

import Comete._
import Opinion._

// ---- SECCIÓN INTEGRANTE A --------------------------------

println("=== Pruebas min_p ===")

val r1 = min_p(x => x * x, -1.0, 1.0, 0.0001)
println(s"r1 = $r1")
assert(math.abs(r1 - 0.0) < 0.0001)

val r2 = min_p(x => math.pow(x - 0.3, 2), 0.0, 1.0, 0.0001)
println(s"r2 = $r2")
assert(math.abs(r2 - 0.3) < 0.0001)

val r3 = min_p(x => math.pow(x - 5.0, 2), 0.0, 10.0, 0.0001)
println(s"r3 = $r3")
assert(math.abs(r3 - 5.0) < 0.0001)

val testMinP = min_p(x => x * x, -1.0, 1.0, 0.0001)
println(s"testMinP = $testMinP")
assert(math.abs(testMinP) < 0.0001)

println("=== Pruebas Fase 2 - Subtarea A ===")

// ----------------------------------------------------
// uniformBelief
// ----------------------------------------------------
val u = uniformBelief(5)

println(s"uniformBelief(5) = $u")

assert(u == Vector(0.2, 0.4, 0.6, 0.8, 1.0))

// ----------------------------------------------------
// allExtremeBelief
// ----------------------------------------------------
val e = allExtremeBelief(6)

println(s"allExtremeBelief(6) = $e")

assert(e == Vector(0.0, 0.0, 0.0, 1.0, 1.0, 1.0))

// ----------------------------------------------------
// midlyBelief
// ----------------------------------------------------
val m = midlyBelief(6)

println(s"midlyBelief(6) = $m")

assert(m.length == 6)
assert(m.forall(x => x >= 0.0 && x <= 1.0))

// ----------------------------------------------------
// allTripleBelief
// ----------------------------------------------------
val t = allTripleBelief(9)

println(s"allTripleBelief(9) = $t")

assert(
  t ==
    Vector(
      0.0, 0.0, 0.0,
      0.5, 0.5, 0.5,
      1.0, 1.0, 1.0
    )
)

// ----------------------------------------------------
// consensusBelief
// ----------------------------------------------------
val c = consensusBelief(0.42)(5)

println(s"consensusBelief(0.42)(5) = $c")

assert(
  c ==
    Vector(
      0.42,
      0.42,
      0.42,
      0.42,
      0.42
    )
)

println("Todas las pruebas de la Fase 2 -Subtarea A pasaron correctamente.")

// ----------------------------------------------------
// FASE 3 - SUBTAREA A
// betaFactor y contribucion
// ----------------------------------------------------

println("=== Pruebas Fase 3 - Subtarea A ===")

// ----------------------------------------------------
// betaFactor
// ----------------------------------------------------

val beta1 = betaFactor(0.5, 0.5)
println(s"betaFactor(0.5, 0.5) = $beta1")
assert(beta1 == 1.0)

val beta2 = betaFactor(0.0, 1.0)
println(s"betaFactor(0.0, 1.0) = $beta2")
assert(beta2 == 0.0)

val beta3 = betaFactor(0.2, 0.7)
println(s"betaFactor(0.2, 0.7) = $beta3")
assert(math.abs(beta3 - 0.5) < 0.0001)

val beta4 = betaFactor(0.3, 0.4)
println(s"betaFactor(0.3, 0.4) = $beta4")
assert(math.abs(beta4 - 0.9) < 0.0001)


// ----------------------------------------------------
// Grafo simple de prueba:
// toda influencia vale 1
// ----------------------------------------------------

val influencia: WeightedGraph =
  (_, _) => 1.0


// ----------------------------------------------------
// contribucion: caso normal
// ----------------------------------------------------

val sb1 = Vector(
  0.2,
  0.7
)

val c1 = contribucion(
  i = 0,
  j = 1,
  sb1,
  influencia
)

println(s"contribucion(0,1) = $c1")

// β = 0.5
// I = 1
// (bj-bi) = 0.5
// resultado = 0.25

assert(math.abs(c1 - 0.25) < 0.0001)


// ----------------------------------------------------
// contribucion: opiniones idénticas
// ----------------------------------------------------

val sb2 = Vector(
  0.5,
  0.5
)

val c2 = contribucion(
  i = 0,
  j = 1,
  sb2,
  influencia
)

println(s"contribucion iguales = $c2")

assert(math.abs(c2) < 0.0001)


// ----------------------------------------------------
// contribucion: opiniones opuestas
// ----------------------------------------------------

val sb3 = Vector(
  0.0,
  1.0
)

val c3 = contribucion(
  i = 0,
  j = 1,
  sb3,
  influencia
)

println(s"contribucion opuestas = $c3")

// β = 0
// resultado = 0

assert(math.abs(c3) < 0.0001)


// ----------------------------------------------------
// contribucion negativa
// ----------------------------------------------------

val sb4 = Vector(
  0.8,
  0.2
)

val c4 = contribucion(
  i = 0,
  j = 1,
  sb4,
  influencia
)

println(s"contribucion negativa = $c4")

// β = 0.4
// (bj-bi) = -0.6
// resultado = -0.24

assert(math.abs(c4 + 0.24) < 0.0001)

println("Todas las pruebas de Fase 3 - Subtarea A pasaron correctamente.")

// ---- SECCIÓN INTEGRANTE B --------------------------------

println("=== Pruebas rhoAuxValue ===")

val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)

val r = rhoAuxValue(
  1.2,
  1.2,
  Vector(0.5, 0.0, 0.0, 0.0, 0.5),
  likert5,
  0.5
)

println(s"r = $r")

// = 0.378929
assert(math.abs(r - 0.3789291416) < 0.0001)


// Toda la masa en el centro
val rCentro = rhoAuxValue(
  1.2,
  1.2,
  Vector(0.0, 0.0, 1.0, 0.0, 0.0),
  likert5,
  0.5
)

println(s"rCentro = $rCentro")
assert(math.abs(rCentro) < 0.0001)


// Verificar que cambia al mover p
val rExtremo = rhoAuxValue(
  1.2,
  1.2,
  Vector(0.5, 0.0, 0.0, 0.0, 0.5),
  likert5,
  0.0
)

println(s"rExtremo = $rExtremo")
assert(rExtremo > 0.0)

println("=== Pruebas calcularCajones ===")

val cajones = calcularCajones(Vector(0.0, 0.25, 0.5, 0.75, 1.0))

val cajonesEsperados = Vector(
  (0.0, 0.125),
  (0.125, 0.375),
  (0.375, 0.625),
  (0.625, 0.875),
  (0.875, 1.0)
)

println(s"cajones = $cajones")
assert(cajones == cajonesEsperados)

// ----------------------------------------------------
// FASE 3 - SUBTAREA B
// nuevaCreencia
// ----------------------------------------------------

println("=== Pruebas Fase 3 - Subtarea B ===")

// Caso 1: solo auto-influencia => no cambia la creencia
val swgSoloAuto: SpecificWeightedGraph =
  ((i: Int, j: Int) => if (i == j) 1.0 else 0.0, 2)

val sbBase1 = Vector(0.2, 0.7)

val nc1 = nuevaCreencia(0, sbBase1, swgSoloAuto)
println(s"nuevaCreencia(0, sbBase1, swgSoloAuto) = $nc1")
assert(math.abs(nc1 - 0.2) < 0.0001)

val nc2 = nuevaCreencia(1, sbBase1, swgSoloAuto)
println(s"nuevaCreencia(1, sbBase1, swgSoloAuto) = $nc2")
assert(math.abs(nc2 - 0.7) < 0.0001)

// Caso 2: grafo completo con influencia 1 en todas partes
val swgCompleto: SpecificWeightedGraph =
  ((_: Int, _: Int) => 1.0, 2)

// Para i = 0:
// vecinos = {0,1}
// contribución de 1 sobre 0 = 0.5 * 1 * (0.7 - 0.2) = 0.25
// suma = 0.25
// promedio = 0.25 / 2 = 0.125
// nueva = 0.2 + 0.125 = 0.325
val nc3 = nuevaCreencia(0, sbBase1, swgCompleto)
println(s"nuevaCreencia(0, sbBase1, swgCompleto) = $nc3")
assert(math.abs(nc3 - 0.325) < 0.0001)

// Para i = 1:
// contribución de 0 sobre 1 = 0.5 * 1 * (0.2 - 0.7) = -0.25
// promedio = -0.25 / 2 = -0.125
// nueva = 0.7 - 0.125 = 0.575
val nc4 = nuevaCreencia(1, sbBase1, swgCompleto)
println(s"nuevaCreencia(1, sbBase1, swgCompleto) = $nc4")
assert(math.abs(nc4 - 0.575) < 0.0001)

// Caso 3: consenso => no cambia nada
val sbConsenso = Vector(0.5, 0.5)
val nc5 = nuevaCreencia(0, sbConsenso, swgCompleto)
println(s"nuevaCreencia consenso = $nc5")
assert(math.abs(nc5 - 0.5) < 0.0001)

println("Todas las pruebas de Fase 3 - Subtarea B pasaron correctamente.")


// ---- SECCIÓN INTEGRANTE C --------------------------------


// ---- SECCIÓN INTEGRANTE D --------------------------------

println("=== Pruebas normalizar ===")

val cmt1norm = normalizar(rhoCMT_Gen(1.2, 1.2))

// Caso 1: máxima polarización → esperado 1.0
val dNorm1 = cmt1norm((Vector(0.5, 0.0, 0.0, 0.0, 0.5), likert5))
println(s"dNorm1 = $dNorm1")
assert(math.abs(dNorm1 - 1.0) < 0.001, s"Esperado ~1.0, obtenido $dNorm1")

// Caso 2: toda la masa en el centro → esperado 0.0
val dNorm2 = cmt1norm((Vector(0.0, 0.0, 1.0, 0.0, 0.0), likert5))
println(s"dNorm2 = $dNorm2")
assert(math.abs(dNorm2 - 0.0) < 0.001, s"Esperado ~0.0, obtenido $dNorm2")

// Caso 3: distribución asimétrica → esperado ~0.863
val dNorm3 = cmt1norm((Vector(0.4, 0.0, 0.0, 0.0, 0.6), likert5))
println(s"dNorm3 = $dNorm3")
assert(math.abs(dNorm3 - 0.863) < 0.01, s"Esperado ~0.863, obtenido $dNorm3")