// ============================================================
// PRUEBAS.SC — Archivo compartido de pruebas
// REGLA: cada integrante solo edita su sección
// ============================================================

import Comete._
import Opinion._

// ---- SECCIÓN INTEGRANTE A --------------------------------
// Fase 1: pruebas de min_p
// min_p: buscar mínimo de x^2 en [-1, 1], resultado esperado ≈ 0
println("=== Pruebas min_p ===")

val r1 = min_p(x => x * x, -1.0, 1.0, 0.0001)
println(s"r1 = $r1")
assert(math.abs(r1 - 0.0) < 0.0001)         // Debe Pasar

val r2 = min_p(x => math.pow(x - 0.3, 2), 0.0, 1.0, 0.0001)
println(s"r2 = $r2")
assert(math.abs(r2 - 0.3) < 0.0001)         // Debe Pasar

val r3 = min_p(x => math.pow(x - 5.0, 2), 0.0, 10.0, 0.0001)
println(s"r3 = $r3")
assert(math.abs(r3 - 5.0) < 0.0001)         // Debe Pasar

val testMinP = min_p(x => x * x, -1.0, 1.0, 0.0001)
println(s"testMinP = $testMinP")
assert(math.abs(testMinP) < 0.0001)         // Debe Pasar



// ---- SECCIÓN INTEGRANTE B --------------------------------


// ---- SECCIÓN INTEGRANTE C --------------------------------


// ---- SECCIÓN INTEGRANTE D --------------------------------
