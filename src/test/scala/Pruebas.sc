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