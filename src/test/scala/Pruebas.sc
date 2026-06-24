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