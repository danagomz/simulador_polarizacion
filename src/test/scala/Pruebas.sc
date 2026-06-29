import Comete._
import Opinion._

// ============================================================
// SECCIÓN 1 — DATOS DE DISTRIBUCIÓN
// ============================================================
val pi_max         = Vector(0.5, 0.0, 0.0, 0.0, 0.5)
val pi_min         = Vector(0.0, 0.0, 1.0, 0.0, 0.0)
val pi_der         = Vector(0.4, 0.0, 0.0, 0.0, 0.6)
val pi_izq         = Vector(0.6, 0.0, 0.0, 0.0, 0.4)
val pi_int1        = Vector(0.0, 0.5, 0.0, 0.5, 0.0)
val pi_int2        = Vector(0.25, 0.0, 0.5, 0.0, 0.25)
val pi_int3        = Vector(0.25, 0.25, 0.0, 0.25, 0.25)
val pi_cons_centro = pi_min
val pi_cons_der    = Vector(0.0, 0.0, 0.0, 0.0, 1.0)
val pi_cons_izq    = Vector(1.0, 0.0, 0.0, 0.0, 0.0)
val likert5        = Vector(0.0, 0.25, 0.5, 0.75, 1.0)

// ============================================================
// SECCIÓN 2 — rhoCMT_Gen (sin normalizar)
// ============================================================
val cmt1 = rhoCMT_Gen(1.2, 1.2)

val res0  = cmt1((pi_max,         likert5)) // esperado: 0.379
val res2  = cmt1((pi_min,         likert5)) // esperado: 0.0
val res4  = cmt1((pi_der,         likert5)) // esperado: 0.327
val res6  = cmt1((pi_izq,         likert5)) // esperado: 0.327
val res8  = cmt1((pi_int1,        likert5)) // esperado: 0.165
val res10 = cmt1((pi_int2,        likert5)) // esperado: 0.165
val res12 = cmt1((pi_int3,        likert5)) // esperado: 0.237
val res14 = cmt1((pi_cons_centro, likert5)) // esperado: 0.0
val res16 = cmt1((pi_cons_der,    likert5)) // esperado: 0.0
val res18 = cmt1((pi_cons_izq,    likert5)) // esperado: 0.0

// ============================================================
// SECCIÓN 3 — normalizar
// ============================================================
val cmt1_norm = normalizar(cmt1)

val res1  = cmt1_norm((pi_max,         likert5)) // esperado: 1.0
val res3  = cmt1_norm((pi_min,         likert5)) // esperado: 0.0
val res5  = cmt1_norm((pi_der,         likert5)) // esperado: 0.863
val res7  = cmt1_norm((pi_izq,         likert5)) // esperado: 0.863
val res9  = cmt1_norm((pi_int1,        likert5)) // esperado: 0.435
val res11 = cmt1_norm((pi_int2,        likert5)) // esperado: 0.435
val res13 = cmt1_norm((pi_int3,        likert5)) // esperado: 0.625
val res15 = cmt1_norm((pi_cons_centro, likert5)) // esperado: 0.0
val res17 = cmt1_norm((pi_cons_der,    likert5)) // esperado: 0.0
val res19 = cmt1_norm((pi_cons_izq,    likert5)) // esperado: 0.0

// ============================================================
// SECCIÓN 4 — rho
// ============================================================
val sbext    = allExtremeBelief(100)
val sbcons   = consensusBelief(0.2)(100)
val sbunif   = uniformBelief(100)
val sbtriple = allTripleBelief(100)
val sbmidly  = midlyBelief(100)

val rho1 = rho(1.2, 1.2)
val rho2 = rho(2.0, 1.0)

val dist1 = Vector(0.0, 0.25, 0.50, 0.75, 1.0)
val dist2 = Vector(0.0, 0.2,  0.4,  0.6,  0.8, 1.0)

val res29 = rho1(sbext,    dist1) // esperado: 1.0
val res30 = rho2(sbext,    dist1) // esperado: 1.0
val res31 = rho1(sbext,    dist2) // esperado: 1.0
val res32 = rho2(sbext,    dist2) // esperado: 1.0
val res33 = rho1(sbcons,   dist1) // esperado: 0.0
val res34 = rho2(sbcons,   dist1) // esperado: 0.0
val res35 = rho1(sbcons,   dist2) // esperado: 0.0
val res36 = rho2(sbcons,   dist2) // esperado: 0.0
val res37 = rho1(sbunif,   dist1) // esperado: 0.38
val res38 = rho2(sbunif,   dist1) // esperado: 0.188
val res39 = rho1(sbunif,   dist2) // esperado: 0.377
val res40 = rho2(sbunif,   dist2) // esperado: 0.172
val res41 = rho1(sbtriple, dist1) // esperado: 0.617
val res42 = rho2(sbtriple, dist1) // esperado: 0.448
val res43 = rho1(sbtriple, dist2) // esperado: 0.617
val res44 = rho2(sbtriple, dist2) // esperado: 0.448
val res45 = rho1(sbmidly,  dist1) // esperado: 0.784
val res46 = rho2(sbmidly,  dist1) // esperado: 0.58
val res47 = rho1(sbmidly,  dist2) // esperado: 0.773
val res48 = rho2(sbmidly,  dist2) // esperado: 0.528

// ============================================================
// SECCIÓN 5 — confBiasUpdate
// ============================================================
def i1(nags: Int): SpecificWeightedGraph =
  ((i: Int, j: Int) =>
    if (i == j) 1.0
    else if (i < j) 1.0 / (j - i).toDouble
    else 0.0,
    nags)

def i2(nags: Int): SpecificWeightedGraph =
  ((i: Int, j: Int) =>
    if (i == j) 1.0
    else if (i < j) (j - i).toDouble / nags.toDouble
    else (nags - (i - j)).toDouble / nags.toDouble,
    nags)

val i1_10 = i1(10)
val i2_10 = i2(10)
val sbu_10 = uniformBelief(10)
val sbm_10 = midlyBelief(10)

val res51 = confBiasUpdate(sbu_10, i1_10)
// esperado: Vector(0.1, 0.155, 0.24333..., 0.34, 0.44,
//           0.54166..., 0.64428..., 0.7475, 0.85111..., 0.955)

val res54 = confBiasUpdate(sbm_10, i1_10)
// esperado: Vector(0.21, 0.21505, 0.22343..., 0.23265, 0.2422,
//           0.65498..., 0.70696..., 0.73358..., 0.75239..., 0.76771...)

val res52 = rho1(sbu_10, dist1)                        // esperado: 0.383
val res53 = rho1(confBiasUpdate(sbu_10, i1_10), dist1) // esperado: 0.38
val res55 = rho1(sbm_10, dist1)                        // esperado: 0.435
val res56 = rho1(confBiasUpdate(sbm_10, i1_10), dist1) // esperado: 0.435

// ============================================================
// SECCIÓN 6 — simulate
// ============================================================
val res57 = for {
  b <- simulate(confBiasUpdate, i1_10, sbu_10, 2)
} yield (b, rho1(b, dist1))
// esperado: pols 0.383, 0.38, 0.335

val res58 = for {
  b <- simulate(confBiasUpdate, i1_10, sbm_10, 2)
} yield (b, rho1(b, dist1))
// esperado: pols 0.435, 0.435, 0.377

// ============================================================
// SECCIÓN 7 — rhoPar (debe dar mismo resultado que rho)
// ============================================================
// Se usa nombre distinto a rho1/rho2 para evitar cualquier
// sombra de nombre. AgentsPolMeasure viene de import Opinion._
val medidaParalela: AgentsPolMeasure = rhoPar(1.2, 1.2)

val diffExt    = math.abs(medidaParalela(sbext,    dist1) - rho1(sbext,    dist1)) // esperado: 0.0
val diffCons   = math.abs(medidaParalela(sbcons,   dist1) - rho1(sbcons,   dist1)) // esperado: 0.0
val diffUnif   = math.abs(medidaParalela(sbunif,   dist1) - rho1(sbunif,   dist1)) // esperado: 0.0
val diffTriple = math.abs(medidaParalela(sbtriple, dist1) - rho1(sbtriple, dist1)) // esperado: 0.0
val diffMidly  = math.abs(medidaParalela(sbmidly,  dist1) - rho1(sbmidly,  dist1)) // esperado: 0.0

// ============================================================
// SECCIÓN 8 — confBiasUpdatePar (debe dar mismo resultado)
// ============================================================
val res_par_u = confBiasUpdatePar(sbu_10, i1_10)
val res_seq_u = confBiasUpdate(sbu_10, i1_10)
val okU = res_par_u.zip(res_seq_u).forall { case (a, b) => math.abs(a - b) < 1e-9 }
// esperado: true

val res_par_m = confBiasUpdatePar(sbm_10, i1_10)
val res_seq_m = confBiasUpdate(sbm_10, i1_10)
val okM = res_par_m.zip(res_seq_m).forall { case (a, b) => math.abs(a - b) < 1e-9 }
// esperado: true

// ============================================================
// SECCIÓN 9 — simulate con confBiasUpdatePar
// ============================================================
val res57_par = for {
  b <- simulate(confBiasUpdatePar, i1_10, sbu_10, 2)
} yield (b, medidaParalela(b, dist1))

val okSim = res57_par.map(_._2).zip(res57.map(_._2))
  .forall { case (a, b) => math.abs(a - b) < 1e-9 }
// esperado: true