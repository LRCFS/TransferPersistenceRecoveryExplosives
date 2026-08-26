FUTURE PhD RESEARCH DIRECTIONS
Building on Amy Bruce's PhD Work on Explosive Trace Recovery

Consolidated from three source documents (Future_PhD_Research_Directions.txt,
_Summary.txt, Future_Research_Questions_List.txt) into a single document.
Original Generated: July 15, 2026. Consolidated: 25 August 2026.

================================================================================
TABLE OF CONTENTS
================================================================================
1. Established Baseline
2. Executive Summary (condensed overview of all 7 directions)
3. Full Research Directions (detailed) 3.1-3.7
4. Cross-Cutting Themes and Integration Opportunities
5. Recommended Prioritization for Next PhD Student
6. Suggested PhD Structure (3-4 Year Timeline)
7. Final Recommendation
Appendix A. Consolidated Index of All 72 Research Questions

================================================================================
1. ESTABLISHED BASELINE (What Amy's PhD Will Have Proven)
================================================================================

ESTABLISHED BASELINE (What Amy's PhD Will Have Proven)
================================================================================

1. Operator variability quantified: 42-52% of total variance (n=60 operators, 37 labs)
2. Surface chemistry effects validated: SFE predictions confirmed (glass > steel > ABS)
3. ASTRA device eliminates operator variance: CV <5% (vs 12-30% manual)
4. Key parameters optimized via DoE:
   - Pressure effect quantified (50g vs 200g)
   - Solvent necessity: ~30× improvement (wet vs dry)
   - Surface × pressure × solvent interactions mapped

SCOPE OF COMPLETED WORK:
- Analytes: PETN, RDX only
- Surfaces: Glass, steel, ABS (smooth + textured) - all non-porous
- Swab type: Cotton Q-tips
- Solvent: Ethanol (100µL)
- Method: Single pass ASTRA serpentine pattern
- Conditions: Clean, pre-spiked laboratory surfaces


================================================================================
2. EXECUTIVE SUMMARY
================================================================================
(Condensed overview of Research Directions 1-7 plus cross-cutting integration
opportunities and prioritization. See Section 3 for full detail -- experimental
designs, sample sizes, procedures and outcome scenarios are given there, not here.)

RESEARCH DIRECTION 1: ANALYTE EXPANSION
================================================================================

RATIONALE: 
Current work establishes methodology for PETN/RDX only. Operational forensics 
encounters diverse explosive classes with different chemical properties. 
Critical question: Do optimal parameters generalize or are they analyte-specific?

PRIORITY ANALYTES:

HIGH PRIORITY (Forensically Critical):

1. TNT (2,4,6-trinitrotoluene)
   - Most common military explosive
   - Different chemistry: aromatic nitro compound
   - Lower polarity than PETN/RDX
   - Questions: Does ethanol remain optimal? Are SFE rankings consistent?

2. TATP (triacetone triperoxide)
   - Common improvised explosive (terrorism-related)
   - Highly thermally labile (potentially worse than PETN)
   - Peroxide functional groups → different surface interactions
   - Questions: Does TATP require lower GC inlet temperature? Is crushing/pressure beneficial?

3. HMTD (hexamethylene triperoxide diamine)
   - Another peroxide explosive, even more unstable than TATP
   - Rarely studied due to safety concerns
   - Questions: Too unstable for swab recovery? Safe surface loading limits?

4. Black Powder Components (KNO₃, charcoal, sulfur)
   - Most common in improvised devices
   - PARTICULATE residue, not dissolved traces
   - Questions: Does ASTRA work for particles vs dissolved? Is dry swabbing better for particles?

MEDIUM PRIORITY:
5. ANFO Components (ammonium nitrate + fuel oil)
6. Smokeless Powder (nitrocellulose/nitroglycerin mixtures)
7. Ammonium Nitrate-Based Explosives
8. Chlorate/Perchlorate Explosives

CORE RESEARCH QUESTIONS:

Q1: Is ethanol the optimal solvent for all explosive classes, or do alternatives perform better?
    - Hypothesis: Ethanol best for nitrate esters/nitramines, acetone best for aromatics (TNT)

Q2: Do different explosive classes require different solvents?
    - Hypothesis: Solvent selectivity by analyte chemistry (polar vs non-polar)

Q3: Do SFE surface rankings (glass > steel > ABS) hold for all analytes?
    - Test generalizability of surface chemistry predictions

Q4: Is there a thermal lability hierarchy (TATP > PETN > RDX > TNT)?
    - Correlate with drift correction requirements

Q5: Does pressure optimization differ for particulate vs dissolved residues?
    - Hypothesis: Particles need higher pressure for mechanical pickup

Q6: Can single optimal conditions recover multiple analytes simultaneously?
    - Or need analyte-specific protocols?

OVERARCHING HYPOTHESIS:
"Optimal recovery parameters are ANALYTE-CLASS SPECIFIC, not universal."

Expected Duration: 6-9 months per analyte (18-24 months for 3-4 analytes)
Expected Publications: 2-3 papers (analyte-specific optimizations + comparative study)

================================================================================
RESEARCH DIRECTION 2: SWAB MATERIAL OPTIMIZATION
================================================================================

RATIONALE:
Cotton Q-tips are standard but largely untested. Swab material surface free 
energy affects both pickup (surface → swab) and release (swab → solvent extraction).

THEORETICAL FRAMEWORK:

Two-Stage Recovery Model:
- Stage 1 (Pickup): Surface → Swab (maximize transfer)
- Stage 2 (Extraction): Swab → Solvent (maximize release)

HYPOTHESIS: Optimal swab SFE is INTERMEDIATE between surface and solvent
- Glass SFE: ~84 mJ/m²
- Cotton SFE: ~57 mJ/m² (current)
- Ethanol SFE: ~22 mJ/m²

CANDIDATE SWAB MATERIALS:

Natural Fibers:
1. Cotton (baseline, SFE ~57 mJ/m²) - current standard
2. Muslin (woven cotton, ~57 mJ/m²) - larger surface area
3. Rayon (regenerated cellulose, ~60 mJ/m²) - less lint

Synthetic Fibers:
4. Polyester (~35-43 mJ/m²) - lower SFE, easier release?
5. Nylon (~40-46 mJ/m²) - intermediate polarity
6. Polypropylene (~30 mJ/m²) - very non-polar
7. PTFE/Teflon (~18 mJ/m²) - ultra-low adhesion

Specialized Materials:
8. Flocked Swabs (DNA collection standard) - high surface area, rapid release
9. Foam Swabs (polyurethane) - high absorbency
10. Surface-Treated Swabs (silicone-coated, surfactant-treated)

Novel Designs:
11. Dual-Layer Swabs - outer layer for pickup, inner layer for release

CORE RESEARCH QUESTIONS:

Q1: Does swab material significantly affect recovery efficiency?
    - Expected variation: 10-30% between swab types

Q2: Is optimal swab material surface-specific or universal?
    - Test Swab × Surface interaction

Q3: Does swab SFE predict recovery efficiency?
    - Test linear, quadratic, or threshold models

Q4: Can we separate pickup vs extraction efficiency?
    - Two-stage experiment to identify bottleneck

Q5: Does swab fiber structure matter beyond SFE?
    - Compare materials with similar SFE but different morphology

Q6: Do synthetic swabs reduce GC-MS contamination?
    - Cotton lint causes inlet buildup - do synthetics help?

Q7: Does swab cost-effectiveness matter operationally?
    - Cotton: $0.05/swab vs Flocked: $0.50-1.00/swab

Q8: Can swabs be reused after cleaning?
    - Performance degradation with reuse cycles

Expected Duration: 6-9 months
Expected Publications: 1-2 papers (screening study + SFE model validation)

================================================================================
RESEARCH DIRECTION 3: SOLVENT OPTIMIZATION
================================================================================

RATIONALE:
Ethanol demonstrated ~30× improvement vs dry swabbing, but IS ETHANOL OPTIMAL?
Different solvents have different polarities, volatilities, and extraction 
efficiencies. Optimal solvent likely depends on analyte chemistry.

THEORETICAL CRITERIA:
1. Polarity Matching ("like dissolves like")
2. Solubility (explosive must dissolve during swabbing)
3. Volatility (balance: stays wet vs dries quickly)
4. GC-MS Compatibility (no interference)
5. Surface Wetting (low contact angle)
6. Safety (low toxicity, non-flammable preferred)

CANDIDATE SOLVENTS:

Alcohols:
1. Ethanol (current, BP 78°C, polarity 5.2) - baseline
2. Methanol (BP 65°C, polarity 6.6) - more polar, more volatile
3. Isopropanol (BP 82°C, polarity 4.3) - less polar, less volatile
4. Butanol (BP 117°C, polarity 4.0) - low volatility

Ketones:
5. Acetone (BP 56°C, polarity 5.4) - excellent for aromatics, very volatile
6. Methyl Ethyl Ketone (BP 80°C, polarity 4.5) - less volatile

Nitriles:
7. Acetonitrile (BP 82°C, polarity 6.2) - LC-MS standard, expensive

Water-Based:
8. Water (BP 100°C, polarity 9.0) - best for ionic explosives
9. Water + Surfactant (Tween-20) - improved wetting
10. Water:Methanol (50:50) - intermediate polarity

Mixed Solvents:
11. Ethanol:Acetone (1:1) - broad polarity range
12. Ethanol:Water (1:1) - increased polarity
13. Acetonitrile:Water (1:1) - LC-MS compatible

CORE RESEARCH QUESTIONS:

Q1: Is ethanol optimal for PETN/RDX, or do alternatives perform better?
    - Hypothesis: Ethanol is near-optimal (within 10% of best)

Q2: Do different explosive classes require different solvents?
    - Ethanol for PETN/RDX, acetone for TNT, water for ionic?

Q3: Does solvent volume affect recovery efficiency?
    - Current 100µL is arbitrary - test 50µL to 500µL range

Q4: Does solvent volatility matter (swab dry-out during swabbing)?
    - Compare acetone (very volatile) vs ethanol vs IPA vs water

Q5: Can surfactants improve water-based swabbing?
    - Reduce surface tension for better wetting

Q6: Do mixed solvents offer advantages for multi-analyte recovery?
    - Test on PETN+TNT mixtures (different polarities)

Q7: Does solvent choice interact with surface type?
    - High-SFE surfaces need polar solvents?

Q8: Does solvent affect GC-MS performance (inlet contamination)?
    - Different boiling points affect drift rate?

Q9: What is the cost-benefit trade-off?
    - Ethanol ~$0.50/L vs Acetonitrile ~$50/L

Q10: Can solvent be eliminated with extreme pressure?
     - Test dry swabbing at 1000-2000g pressure

Expected Duration: 6-9 months
Expected Publications: 1-2 papers (solvent screening + analyte-specific matching)

================================================================================
RESEARCH DIRECTION 4: SWABBING PATTERN AND MULTI-PASS OPTIMIZATION
================================================================================

RATIONALE:
Amy selected serpentine pattern with 5mm spacing as "optimal" based on UV powder 
studies, but was this rigorously tested? Multi-pass strategies may improve recovery.

CORE RESEARCH QUESTIONS:

Q1: Does swabbing pattern affect recovery efficiency?
    Patterns to test:
    - Serpentine (current - continuous back-and-forth)
    - Raster (parallel lines, lift between lines)
    - Spiral (inside-out or outside-in)
    - Cross-hatch (orthogonal passes, 0° then 90°)
    - Perimeter-first (edges then interior)
    - Random walk (stochastic pattern)
    Hypothesis: Continuous patterns better than lift-return patterns

Q2: Does line spacing affect recovery?
    - Current: 5mm spacing (empirical)
    - Test: 2mm, 5mm, 10mm, 15mm
    - Balance: Coverage vs time

Q3: Do edge effects matter?
    - Explosive accumulates at edges operationally
    - Test: Perimeter-first vs standard serpentine

Q4: Does swabbing velocity affect recovery?
    - Test: 10mm/s (slow), 30mm/s (current), 50mm/s (fast)
    - Balance: Contact time vs solvent evaporation

Q5: Do multiple passes improve recovery?
    - Single pass (current) vs double pass vs orthogonal (cross-hatch)
    - Question: Is improvement worth 2× time cost?

Q6: Does dwell time matter (pressure hold vs continuous motion)?
    - Alternative: Pause at grid points (1-2s hold)
    - May help aged/embedded deposits (diffusion-limited)

Q7: Does coverage percentage correlate with recovery?
    - Use UV powder to visualize coverage
    - Test: Linear relationship (90% coverage = 90% recovery)?
    - Or threshold effect (need >80% coverage)?

Expected Duration: 4-6 months
Expected Publications: 1-2 papers (pattern optimization + multi-pass strategies)

================================================================================
RESEARCH DIRECTION 5: REAL-WORLD CONTAMINATION SCENARIOS
================================================================================

RATIONALE:
Amy's work uses clean, pre-spiked laboratory surfaces with known quantities. 
Operational samples differ: low quantities, irregular distribution, aged deposits, 
interfering contaminants. How do lab-optimized parameters perform operationally?

CORE RESEARCH QUESTIONS:

Q1: How does recovery efficiency scale with contamination level?
    - Lab studies: 10µg PETN (high, easy to detect)
    - Operational range: 10ng to 1µg (1-1000× lower)
    - Test recovery at: 10µg, 1µg, 100ng, 10ng, 1ng
    - Determine operational detection limit

Q2: Does aging affect recovery efficiency?
    - Lab: Fresh deposits (same day)
    - Operational: Days to months old
    - Test: Recovery after 1 day, 1 week, 1 month, 3 months, 6 months
    - Conditions: Indoor vs outdoor (UV, temperature, humidity)
    - Hypothesis: PETN degrades (UV photolysis), RDX more stable

Q3: Do surface contaminants interfere with recovery?
    - Lab: Clean surfaces
    - Operational: Oils, soils, dust, cosmetics, hand lotion
    - Test: Clean vs oil vs soil vs lotion vs fingerprint residue
    - Hypothesis: Organic films may block or enhance recovery

Q4: Does contact transfer simulate realistic contamination?
    - Lab: Pipette deposition (uniform)
    - Operational: Fingerprint-like transfer (non-uniform, localized)
    - Compare: Pipette vs fingerprint vs palm transfer
    - Question: Does non-uniformity affect recovery?

Q5: Does secondary/tertiary transfer reduce recoverability?
    - Primary: Direct deposition
    - Secondary: Touch explosive, then touch surface
    - Tertiary: Touch secondary surface, then touch third surface
    - Model transfer dynamics (exponential decay?)

Q6: Can ASTRA recover post-blast residue?
    - Lab: Pre-blast (intact molecules)
    - Post-blast: Decomposition products, particles, soot
    - Test: Swab surfaces from controlled detonations
    - Challenge: Requires specialized facility, safety protocols

Expected Duration: 9-15 months (aging study requires 6 months waiting time)
Expected Publications: 2-3 papers (concentration/aging + contamination + transfer dynamics)

OPERATIONAL GUIDANCE OUTPUTS:
- Detection limit guidance ("ASTRA reliable for >50ng/cm²")
- Aging recommendations ("Swab PETN within 1 week")
- Surface pre-treatment protocols
- Transfer models for evidence interpretation

================================================================================
RESEARCH DIRECTION 6: STATISTICAL METHODS AND DATA ANALYSIS
================================================================================

RATIONALE:
Amy developed drift correction and QC methods for PETN/RDX. Future analytes 
may have different degradation profiles. Can methods generalize? Are there 
better statistical approaches?

CORE RESEARCH QUESTIONS:

Q1: Do drift correction methods generalize to other analytes?
    - Amy's power law model works for PETN
    - Does it work for TNT, TATP, others?
    - Or need analyte-specific models (exponential, piecewise)?

Q2: Can machine learning improve drift correction?
    - Current: Parametric models (power law, polynomial)
    - Alternative: ML (random forest, neural networks)
    - Input features: Injection order, QC history, time, temp
    - Question: Does ML significantly outperform parametric models?

Q3: Is there a universal internal standard?
    - 15N-RDX works for RDX only
    - NG failed for PETN (different RT, lability)
    - Can ONE IS correct drift for ALL analytes?

Q4: Can QC strategy be optimized?
    - Current: QC every 10 samples (10% overhead)
    - Too frequent: Wasted time
    - Too infrequent: Poor drift model
    - Find optimal frequency

Q5: Can real-time drift detection trigger automatic QC insertion?
    - Adaptive QC: Insert QC when drift detected
    - Reduces overhead when drift is stable
    - Increases frequency when drift accelerates

Q6: Can experimental design be further optimized?
    - Amy used 2³ factorial (categorical factors)
    - Alternative: Response surface methodology (continuous factors)
    - Example: Pressure 50-200g continuous vs low/high
    - May find true optimum (e.g., 175g not tested in factorial)

Expected Duration: 3-6 months (mostly retrospective analysis)
Expected Publications: 1-2 papers (methods comparison + adaptive QC)

================================================================================
RESEARCH DIRECTION 7: DEPLOYMENT AND VALIDATION STUDIES
================================================================================

RATIONALE:
ASTRA is a lab prototype. Operational use requires ruggedization, validation, 
training, and cost-benefit demonstration. Lower fundamental science novelty, 
but high operational impact.

CORE RESEARCH QUESTIONS:

Q1: Can ASTRA be miniaturized for field deployment?
    - Current: Bench-top 3D printer (~30kg, 50×50×50cm)
    - Target: Suitcase-sized (<10kg, <40×30×20cm)
    - Challenges: Maintain accuracy, battery power

Q2: Is ASTRA cost-effective vs manual swabbing?
    - ASTRA cost: ~£700 per device
    - Training cost: Manual requires 2-4 weeks training
    - Break-even point (number of samples)?

Q3: Do operators accept ASTRA (human factors)?
    - Ease of use, trust in automated results
    - Workflow integration

Q4: Does ASTRA reduce inter-lab variability?
    - Hypothesis: Operator variance eliminated → inter-lab variance <10%
    - Test: Replicate FINEX study (37 labs) but with ASTRA
    - Expected: Variance drops from 42-52% to <10%

Q5: Can ASTRA be validated to forensic standards?
    - ISO 17025 accreditation requirements
    - Proficiency testing (blind samples)
    - Inter-lab comparison
    - Uncertainty quantification

Expected Duration: 18-24 months (engineering + validation)
Expected Publications: 1-2 papers (inter-lab validation + cost-benefit analysis)

DELIVERABLES:
- Portable ASTRA prototype
- User manual and training materials
- Cost-benefit analysis report
- Validation data package (ISO 17025 support)
- Inter-lab study (high-impact publication)

================================================================================
CROSS-CUTTING INTEGRATION OPPORTUNITIES
================================================================================

INTEGRATION 1: Comprehensive Multi-Factor Optimization
- Combine analyte + solvent + swab material into single study
- 4 analytes × 3 solvents × 3 swabs × 2 surfaces = 72 conditions
- Identify optimal combinations per analyte/surface
- Duration: 12-15 months
- Publication: "Comprehensive optimization across explosive classes"

INTEGRATION 2: Real-World Validation of Lab Parameters
- Use optimal parameters from Amy's DoE
- Test on realistic samples (aged, contaminated, low concentration)
- Question: Do lab optima hold operationally?
- Duration: 9-12 months

INTEGRATION 3: Machine Learning for Adaptive Optimization
- ML model predicts optimal pressure/solvent based on sample characteristics
- Input: Surface type, analyte, contamination level, age
- Output: Recommended pressure, solvent volume
- Deploy as "smart ASTRA" (auto-adjusts parameters)
- Duration: 2-3 years (requires large training dataset)

================================================================================
RECOMMENDED PRIORITIZATION FOR NEXT PhD STUDENT
================================================================================

TIER 1 (ESSENTIAL - High Impact, Logical Extension):

1. ANALYTE EXPANSION (Direction 1)
   - TNT and TATP are must-haves
   - Tests generalizability of Amy's findings
   - Duration: 12-18 months
   - Publications: 2-3 papers

2. REAL-WORLD SCENARIOS (Direction 5)
   - Aging, low concentration, surface contamination
   - Bridges lab → operational gap
   - Duration: 9-12 months
   - Publications: 2-3 papers

TIER 2 (VALUABLE - Medium-High Impact):

3. SWAB MATERIAL OPTIMIZATION (Direction 2)
   - Tests fundamental SFE hypothesis
   - May find significant improvements
   - Duration: 6-9 months
   - Publications: 1-2 papers

4. SOLVENT OPTIMIZATION (Direction 3)
   - Especially important if TNT added
   - Duration: 6-9 months
   - Publications: 1-2 papers

TIER 3 (OPTIONAL - Situational Value):

5. PATTERN OPTIMIZATION (Direction 4)
   - Current serpentine likely adequate
   - Duration: 4-6 months
   - Publications: 1-2 papers

6. STATISTICAL METHODS (Direction 6)
   - Refinement rather than new science
   - Duration: 3-6 months
   - Publications: 1-2 papers

7. DEPLOYMENT/VALIDATION (Direction 7)
   - Lower science novelty, high operational impact
   - Better for post-PhD or industry collaboration
   - Duration: 18-24 months
   - Publications: 1-2 papers


================================================================================
3. FULL RESEARCH DIRECTIONS (DETAILED)
================================================================================

RESEARCH DIRECTION 1: ANALYTE EXPANSION
================================================================================

RATIONALE: 
Current work establishes methodology for PETN/RDX only. Operational forensics 
encounters diverse explosive classes with different chemical properties. 
Critical question: Do optimal parameters generalize or are they analyte-specific?

--------------------------------------------------------------------------------
PRIORITY ANALYTES
--------------------------------------------------------------------------------

HIGH PRIORITY (Forensically Critical):

1. TNT (2,4,6-trinitrotoluene)
   - Most common military explosive
   - Different chemistry: aromatic nitro compound
   - Lower polarity than PETN/RDX
   - Research questions:
     * Does ethanol remain optimal solvent, or is acetone/methanol better?
     * Are SFE surface rankings consistent (glass > steel > ABS)?
     * Does pressure optimization (200g) hold for TNT?
     * Is TNT less thermally labile than PETN (better GC-MS recovery)?

2. TATP (triacetone triperoxide)
   - Common improvised explosive (terrorism-related)
   - Highly thermally labile (potentially worse than PETN)
   - Peroxide functional groups → different surface interactions
   - Research questions:
     * Does TATP require lower GC inlet temperature than PETN?
     * Does crushing/particle disruption matter (higher pressure beneficial)?
     * Is TATP surface adhesion weaker than PETN (easier recovery)?
     * Does TATP degrade during swabbing (oxidation on surfaces)?
     * Can drift correction methods developed for PETN apply to TATP?

3. HMTD (hexamethylene triperoxide diamine)
   - Another peroxide explosive
   - Even more unstable than TATP
   - Rarely studied due to safety concerns
   - Research questions:
     * Is HMTD too unstable for swab-based recovery (degradation losses)?
     * Does ASTRA mechanical agitation cause detonation risk?
     * What is maximum safe surface loading for HMTD experiments?
     * Can HMTD be analyzed via same GC-MS method as TATP?

4. Black Powder Components (KNO₃, charcoal, sulfur)
   - Most common in improvised devices
   - PARTICULATE residue, not dissolved traces
   - Fundamentally different recovery mechanism
   - Research questions:
     * Does ASTRA work for particles vs dissolved traces?
     * Is mechanical pickup (dry swabbing) better than wet for particles?
     * Does high pressure embed particles deeper into surface (counterproductive)?
     * Should extraction use different solvents (water for KNO₃, organic for sulfur)?
     * Does particle size affect recovery (fine powder vs coarse grains)?

MEDIUM PRIORITY (Emerging/Specialized):

5. ANFO Components (ammonium nitrate + fuel oil)
   - Common in large-scale bombings
   - Ionic (AN) + non-polar (fuel oil) mixture
   - Research questions:
     * Does single solvent recover both components, or need dual extraction?
     * Is water better for AN, hydrocarbon better for fuel oil?
     * Does ASTRA pressure separate oil film from AN crystals?

6. Smokeless Powder (nitrocellulose/nitroglycerin mixtures)
   - Gun propellant, found in firearms incidents
   - Partially dissolved, partially particulate
   - Research questions:
     * Do NC and NG require different recovery methods?
     * Is NG recovery similar to PETN (both nitrate esters)?
     * Does powder aging (stabilizer depletion) affect swab recovery?

7. Ammonium Nitrate-Based Explosives
   - Urea nitrate, ammonium nitrate/sugar mixtures
   - Highly water-soluble, ionic
   - Research questions:
     * Is water a better swab solvent than ethanol for AN-based explosives?
     * Does surface type matter less (ionic compounds weakly bound)?
     * Can ASTRA recover water-soluble explosives without re-deposition?

8. Chlorate/Perchlorate Explosives
   - Potassium chlorate, potassium perchlorate
   - Oxidizer-based mixtures
   - Research questions:
     * Similar to AN - is water optimal?
     * Does surface chemistry affect ionic explosive adhesion?

LOW PRIORITY (Rare/Specialized):

9. Nitroglycerine (NG) - already tested as internal standard for PETN
10. Picric acid - rare, mainly historical
11. Lead azide - detonator compound, safety concerns
12. PETN/RDX mixtures (Semtex-type) - already covered by individual components

--------------------------------------------------------------------------------
CORE RESEARCH QUESTIONS (Analyte Expansion)
--------------------------------------------------------------------------------

OVERARCHING HYPOTHESIS:
"Optimal recovery parameters (pressure, solvent, surface effects) are 
ANALYTE-CLASS SPECIFIC, not universal."

Specific Hypotheses to Test:

H1: Solvent Selectivity
    - Ethanol optimal for nitrate esters (PETN) and nitramines (RDX)
    - Acetone optimal for aromatic nitro compounds (TNT)
    - Water optimal for ionic explosives (AN, KNO₃, urea nitrate)
    - Test: 3 solvents × 6 analyte classes → identify best matches

H2: Pressure Effects by Physical Form
    - Dissolved traces: Pressure effect small (current finding)
    - Particulate residues: Pressure effect large (mechanical pickup critical)
    - Viscous residues (oils): Pressure effect moderate (surface spreading)
    - Test: 3 pressure levels × 3 residue types → quantify effect sizes

H3: Surface Free Energy Predictions
    - SFE rankings (glass > steel > ABS) hold for polar explosives (PETN, RDX, AN)
    - SFE rankings reverse or flatten for non-polar explosives (TNT, fuel oil)
    - Test: 4 surfaces × 6 analytes → cluster analysis of surface preferences

H4: Thermal Lability Hierarchy
    - TATP > PETN > RDX > TNT (predicted from structure)
    - Thermal lability correlates with drift correction requirement
    - Test: Parallel GC-MS analysis, measure degradation rates
    - Question: Is PETN a "worst case" or is TATP worse?

H5: Multi-Analyte Recovery Trade-offs
    - Single optimal condition does NOT exist for all analytes
    - Pressure/solvent optimized for PETN may be suboptimal for TNT
    - Need compromise conditions or multi-step protocols
    - Test: Optimization for PETN+RDX+TNT mixture vs individual optima

--------------------------------------------------------------------------------
PROPOSED EXPERIMENTAL DESIGNS (Analyte Expansion)
--------------------------------------------------------------------------------

DESIGN 1: Sequential Analyte Studies (Conservative Approach)

Structure:
- Repeat full DoE main study for EACH new analyte individually
- Same design: 2³ factorial (Surface × Pressure × Solvent)
- Same surfaces: Steel, ABS (drop glass if budget constrained)
- Same ASTRA conditions: Serpentine pattern, controlled pressure
- Timeline: 6-9 months per analyte

Analyte Priority Order:
1. TNT (Year 1, Phase 1) - most common military explosive
2. TATP (Year 1, Phase 2) - peroxide class, high-threat
3. Black powder (Year 2, Phase 1) - particulate mechanism test
4. ANFO/AN-based (Year 2, Phase 2) - ionic explosive class

Advantages:
- Direct comparison to PETN/RDX baseline
- Each analyte = independent publication
- Lower risk (proven methodology)

Disadvantages:
- Time-consuming (4 analytes = 24-36 months)
- Inefficient (many replicated conditions)

DESIGN 2: Comparative Multi-Analyte Study (Efficient Approach)

Structure:
- Test multiple analytes simultaneously in same experiment
- Factorial design: 4 analytes × 3 solvents × 2 pressures × 2 surfaces = 48 conditions
- Timeline: 12-15 months total

Selected Analytes (representative classes):
- PETN (baseline, nitrate ester)
- RDX (baseline, nitramine)
- TNT (aromatic nitro compound)
- TATP or Black powder (peroxide or particulate)

Advantages:
- More efficient (18 months vs 36 months for 4 analytes)
- Direct statistical comparison across analytes
- Tests interaction effects (Analyte × Solvent, Analyte × Surface)

Disadvantages:
- Complex experimental design (48 conditions)
- GC-MS method must handle all analytes (may need separate methods)
- Less depth per analyte

DESIGN 3: Targeted Mechanism Studies (High-Risk, High-Novelty)

Focus on specific hypotheses rather than full optimization:

Study 3a: Particulate vs Dissolved Recovery Mechanisms
- Compare black powder (particles) vs PETN (dissolved) recovery
- Test: Dry swabbing at multiple pressures (50g, 200g, 500g, 1000g)
- Hypothesis: Particles need high pressure, dissolved traces do not
- Duration: 3-4 months

Study 3b: Thermal Lability Comparison (PETN vs TATP)
- Parallel GC-MS analysis with identical drift patterns
- Measure degradation kinetics for both analytes
- Test drift correction models (power law, exponential, linear)
- Hypothesis: TATP degrades faster than PETN, requires different correction
- Duration: 2-3 months

Study 3c: Multi-Analyte Compromise Optimization
- Surface contaminated with PETN+RDX+TNT mixture (operational scenario)
- Test recovery with different solvents (ethanol, acetone, 50:50 mix)
- Question: Does mixture require different conditions than pure analytes?
- Duration: 3-4 months

--------------------------------------------------------------------------------
EXPECTED OUTCOMES (Analyte Expansion)
--------------------------------------------------------------------------------

PUBLICATIONS (Estimated):
- Paper 1: "TNT recovery optimization via automated swabbing" (Year 1)
- Paper 2: "Peroxide explosive recovery (TATP/HMTD): thermal stability challenges" (Year 2)
- Paper 3: "Particulate vs dissolved explosive traces: recovery mechanism comparison" (Year 2)
- Paper 4: "Universal vs analyte-specific swabbing protocols: systematic comparison" (Year 3)

OPERATIONAL IMPACT:
- Evidence-based protocols for each explosive class
- Solvent selection guide (ethanol for X, acetone for Y, water for Z)
- Pressure recommendations by analyte type
- Training materials for forensic practitioners

SCIENTIFIC CONTRIBUTIONS:
- Test generalizability of surface chemistry predictions (SFE model)
- Establish thermal lability hierarchy for explosive analytes
- Quantify particulate vs dissolved recovery mechanisms
- Validate ASTRA for diverse analyte classes

================================================================================
RESEARCH DIRECTION 2: SWAB MATERIAL OPTIMIZATION
================================================================================

RATIONALE:
Cotton Q-tips are standard but largely untested. Swab material surface free 
energy affects both pickup (surface → swab) and release (swab → solvent extraction).
Optimal swab should maximize both stages.

--------------------------------------------------------------------------------
THEORETICAL FRAMEWORK
--------------------------------------------------------------------------------

Two-Stage Recovery Model:

Stage 1: Pickup Efficiency (Surface → Swab)
- Transfer occurs when swab SFE > surface SFE (thermodynamic driving force)
- Mechanical action (pressure, rubbing) overcomes adhesion
- Solvent wetting aids transfer by dissolving explosive at interface

Stage 2: Extraction Efficiency (Swab → Solvent)
- Transfer occurs when solvent SFE > swab SFE
- Sonication/agitation overcomes swab-analyte binding
- Swab fiber structure affects diffusion rate

HYPOTHESIS: Optimal swab SFE is INTERMEDIATE between surface and solvent

Example:
- Glass SFE: ~84 mJ/m²
- Cotton SFE: ~57 mJ/m² (current swab)
- Ethanol SFE: ~22 mJ/m²
- Prediction: Cotton should work (57 between 84 and 22)

BUT: Is 57 optimal, or would 40 or 70 be better?

ALTERNATIVE HYPOTHESIS: Different swabs optimal for different surfaces

- High-SFE surfaces (glass) → need high-SFE swabs (cotton, cellulose)
- Low-SFE surfaces (ABS) → need low-SFE swabs (polyester, nylon)

--------------------------------------------------------------------------------
CANDIDATE SWAB MATERIALS
--------------------------------------------------------------------------------

NATURAL FIBERS:

1. Cotton (current baseline)
   - SFE: ~57 mJ/m² (polar, -OH groups)
   - Fiber structure: Twisted hollow fibers
   - Advantages: High absorbency, widely available, low cost
   - Disadvantages: Lint contamination (GC-MS issue), variable quality

2. Muslin (woven cotton fabric)
   - SFE: ~57 mJ/m² (same as cotton)
   - Fiber structure: Tightly woven, larger surface area
   - Advantages: Used by some agencies, reusable (if cleaned properly)
   - Disadvantages: Thicker (harder to handle), extraction more difficult

3. Rayon (regenerated cellulose)
   - SFE: ~60 mJ/m² (slightly higher than cotton)
   - Fiber structure: Smooth, continuous filaments
   - Advantages: Less lint than cotton, more uniform
   - Disadvantages: Cost, availability

SYNTHETIC FIBERS:

4. Polyester
   - SFE: ~35-43 mJ/m² (non-polar, aromatic rings)
   - Fiber structure: Smooth, low absorbency
   - Advantages: Should RELEASE analytes more easily (weak binding)
   - Disadvantages: May pick up less initially (low absorbency)
   - Hypothesis: Better for low-SFE surfaces (ABS, plastics)

5. Nylon (polyamide)
   - SFE: ~40-46 mJ/m² (moderate polarity, amide groups)
   - Fiber structure: Strong, elastic
   - Advantages: Intermediate SFE (compromise between cotton and polyester)
   - Disadvantages: May bind explosives too strongly (amide H-bonding)

6. Polypropylene
   - SFE: ~30 mJ/m² (very non-polar, hydrocarbon only)
   - Fiber structure: Hydrophobic, low surface energy
   - Advantages: Lowest binding → easiest extraction?
   - Disadvantages: Won't absorb ethanol well (needs different solvent?)

7. Teflon/PTFE (low-adhesion swabs)
   - SFE: ~18 mJ/m² (extremely low, fluorinated surface)
   - Advantages: Nothing sticks to Teflon (ultimate release efficiency?)
   - Disadvantages: Nothing sticks to Teflon (poor pickup?)

SPECIALIZED/TREATED MATERIALS:

8. Flocked Swabs (forensic DNA collection standard)
   - Short fibers perpendicular to stick (like velvet)
   - Advantages: High surface area, rapid release during extraction
   - Used for DNA/biological trace evidence (proven in forensics)
   - Question: Do they work for explosives?

9. Foam Swabs (cosmetic applicators)
   - Polyurethane foam (open cell structure)
   - Advantages: High absorbency, consistent quality
   - Used in electronics cleaning (particle pickup)

10. Surface-Treated Swabs
    - Silicone-coated cotton (hydrophobic treatment)
    - Surfactant-treated polyester (enhanced wetting)
    - Hypothesis: Coating changes SFE, alters pickup/release balance

COMPOSITE/LAYERED SWABS (Novel):

11. Dual-Layer Design
    - Outer layer: High-SFE material for pickup (cotton)
    - Inner layer: Low-SFE material for release (polyester)
    - Hypothesis: Combines best of both worlds
    - Challenge: Fabrication, cost

--------------------------------------------------------------------------------
RESEARCH QUESTIONS (Swab Materials)
--------------------------------------------------------------------------------

PRIMARY QUESTIONS:

Q1: Does swab material significantly affect recovery efficiency?
    - Test: 5 swab types × 2 analytes × 2 surfaces = 20 conditions
    - Fixed: Optimal pressure (200g), wet swabbing (100µL ethanol)
    - Metric: Recovery % (compare to cotton baseline)
    - Expected outcome: 10-30% variation between swab types

Q2: Is optimal swab material surface-specific or universal?
    - Test: Swab × Surface interaction term significance
    - Hypothesis: Cotton best for glass, polyester best for ABS
    - Statistical model: Recovery ~ Swab * Surface + (random effects)

Q3: Does swab SFE predict recovery efficiency?
    - Measure SFE for all swab materials (contact angle goniometry)
    - Correlate swab SFE with recovery %
    - Test models:
      * Linear: Recovery ∝ Swab_SFE
      * Quadratic: Recovery ∝ (Swab_SFE - Optimal)²
      * Threshold: Recovery constant above/below critical SFE

Q4: Can we separate pickup vs extraction efficiency?
    - Two-stage experiment:
      Stage 1: Swab surface (collect on swab) → measure via secondary extraction
      Stage 2: Extract swab → measure recovered amount
      Pickup_Efficiency = Stage1 / Known_Amount
      Extraction_Efficiency = Stage2 / Stage1
      Overall_Recovery = Pickup × Extraction

Q5: Does swab fiber structure matter beyond SFE?
    - Compare materials with similar SFE but different structures:
      * Cotton (twisted hollow fibers) vs rayon (smooth filaments)
      * Both ~57-60 mJ/m² but different physical properties
    - Test: Does fiber morphology affect mechanical pickup?

SECONDARY QUESTIONS:

Q6: Do synthetic swabs reduce GC-MS contamination?
    - Cotton lint causes inlet buildup (Amy's finding)
    - Test: Compare inlet degradation (PETN signal drift) for:
      * Cotton swabs (current baseline)
      * Polyester swabs (no cellulose)
      * Flocked swabs (minimal shedding)
    - Metric: Number of samples before 20% signal loss

Q7: Does swab cost-effectiveness matter operationally?
    - Cotton Q-tips: ~$0.05 per swab
    - Flocked swabs: ~$0.50-1.00 per swab (10-20× more expensive)
    - If flocked swabs give 15% better recovery, is cost justified?
    - Economic analysis: Cost per µg explosive recovered

Q8: Can swabs be reused after cleaning?
    - Some agencies reuse muslin swabs (cost savings)
    - Question: Does cleaning restore swab performance?
    - Test: Fresh swab vs cleaned swab (1×, 5×, 10× reused)
    - Metric: Recovery % degradation with reuse cycles

--------------------------------------------------------------------------------
PROPOSED EXPERIMENTAL DESIGNS (Swab Materials)
--------------------------------------------------------------------------------

DESIGN 1: Comprehensive Swab Screening

Objective: Identify top 2-3 swab materials for deeper study

Factors:
- Swab material: 8 types (cotton, muslin, rayon, polyester, nylon, polypropylene, flocked, foam)
- Surface: 2 types (steel, ABS) - drop glass to reduce complexity
- Analyte: 2 types (PETN, RDX) - established baseline
- Fixed: Pressure 200g, solvent 100µL ethanol, ASTRA serpentine pattern

Design: 8 × 2 × 2 = 32 conditions × 3 replicates = 96 samples
Timeline: 4-6 weeks (sample prep + GC-MS analysis)

Analysis:
- ANOVA: Recovery ~ Swab + Surface + Analyte + Swab:Surface + (random)
- Post-hoc: Tukey HSD to rank swabs
- Select top 3 performers + cotton (baseline) for Design 2

DESIGN 2: Mechanism Study (Pickup vs Extraction)

Objective: Understand WHY certain swabs perform better

Selected swabs (based on Design 1):
- Cotton (baseline)
- Best performer from Design 1
- Worst performer from Design 1
- Polyester (low-SFE hypothesis test)

Procedure:
Stage 1 - Pickup Test:
  1. Swab PETN-contaminated steel surface with ASTRA (200g, dry swab)
  2. Extract swab in 1 mL ethanol (sonication 2 min)
  3. Measure PETN recovered = Pickup amount
  4. Calculate Pickup_Efficiency = Recovered / Expected

Stage 2 - Extraction Test:
  1. Pre-load clean swabs with known PETN amount (10 µg)
  2. Extract in 1 mL ethanol (sonication 2 min)
  3. Measure PETN recovered = Extraction amount
  4. Calculate Extraction_Efficiency = Recovered / Loaded

Analysis:
- Compare Pickup vs Extraction efficiency for each swab type
- Identify bottleneck: Is pickup or extraction limiting?
- Test hypothesis: Cotton high pickup, low extraction? Polyester opposite?

Timeline: 3-4 weeks

DESIGN 3: Surface Free Energy Correlation Study

Objective: Test thermodynamic predictions (SFE model)

Swab SFE Measurement:
- Contact angle goniometry (sessile drop method)
- Test liquids: Water, diiodomethane, ethylene glycol
- Calculate SFE via Owens-Wendt method
- Measure for all 8 swab materials

Surface SFE (already established):
- Steel: ~44 mJ/m²
- ABS: ~35-42 mJ/m²
- Glass: ~84 mJ/m² (if included)

Recovery Testing:
- Use data from Design 1 (recovery % for each swab × surface combination)

Analysis:
- Plot Recovery vs Swab_SFE (separate curves for each surface)
- Fit models:
  Model 1: Linear relationship
  Model 2: Optimal intermediate SFE (quadratic)
  Model 3: Threshold effect (sigmoid)
- Test prediction: Swab_SFE should be between Surface_SFE and Solvent_SFE

Timeline: 2-3 weeks (SFE measurements) + use existing recovery data

DESIGN 4: Practical Validation Study

Objective: Test top-performing swabs in realistic scenarios

Selected swabs: Top 2 from Design 1 + cotton baseline

Testing conditions:
- Variable contamination levels: 10µg, 1µg, 100ng, 10ng (operational range)
- Aged deposits: Fresh, 24h, 1 week, 1 month
- Mixed contamination: PETN+RDX+soil+oil (realistic soiling)
- Multiple surfaces: Steel, ABS, glass, painted metal

Metrics:
- Recovery % at each contamination level (limit of detection)
- Recovery % vs aging time (stability assessment)
- Recovery in presence of interferences (robustness)
- Operator ease-of-use (qualitative feedback from lab technicians)

Timeline: 8-10 weeks

Analysis:
- Compare performance curves for each swab type
- Determine if ranking (Design 1) holds under operational conditions
- Recommendation: Best swab for operational deployment

--------------------------------------------------------------------------------
EXPECTED OUTCOMES (Swab Materials)
--------------------------------------------------------------------------------

SCENARIO A: Material matters significantly (>20% difference)

Outcome:
- Identify optimal swab material(s) for each surface type
- Publish: "Swab material optimization for explosive trace recovery"
- Operational impact: Update SOPs to specify swab type
- Potential: Patent novel swab designs (dual-layer, treated surfaces)

SCENARIO B: Material matters marginally (5-15% difference)

Outcome:
- Current cotton Q-tips are adequate (cost-effective choice)
- Synthetic alternatives offer slight improvement but not worth cost
- Publish: "Systematic comparison of swab materials: cotton remains standard"
- Operational impact: Validate current practice (evidence-based)

SCENARIO C: Unexpected findings

Possible surprises:
- Flocked swabs dramatically better (>50% improvement) → paradigm shift
- Polyester better for extraction but worse for pickup → need two-swab protocol?
- Swab material interacts with solvent choice → combined optimization needed

PUBLICATIONS (Estimated):
- Paper 1: "Comprehensive swab material comparison for explosive trace recovery" (Year 1)
- Paper 2: "Surface free energy model for swab material selection" (Year 2, if correlation strong)
- Paper 3: "Dual-layer swab design for optimized pickup and release" (Year 2-3, if novel design pursued)

================================================================================
RESEARCH DIRECTION 3: SOLVENT OPTIMIZATION
================================================================================

RATIONALE:
Ethanol demonstrated ~30× improvement vs dry swabbing, but IS ETHANOL OPTIMAL?
Different solvents have different polarities, volatilities, and extraction 
efficiencies. Optimal solvent likely depends on analyte chemistry.

--------------------------------------------------------------------------------
THEORETICAL FRAMEWORK
--------------------------------------------------------------------------------

Solvent Selection Criteria:

1. Polarity Matching ("Like Dissolves Like")
   - Polar solvents (water, methanol, ethanol) → polar explosives (AN, RDX, PETN)
   - Non-polar solvents (hexane, toluene) → non-polar explosives (fuel oil)
   - Intermediate solvents (acetone, acetonitrile) → broad range

2. Solubility
   - Explosive must dissolve in solvent during swabbing (surface wetting)
   - Must remain dissolved during extraction (no precipitation)
   - Check literature: PETN solubility in various solvents

3. Volatility
   - High volatility (acetone, methanol) → evaporates during swabbing (swab dries out)
   - Low volatility (water, DMF) → stays wet, but longer GC-MS drying time
   - Ethanol intermediate (good compromise)

4. GC-MS Compatibility
   - Solvent must not interfere with analyte peaks
   - Low boiling point preferred (easy to remove in inlet)
   - Ethanol, acetone, methanol all GC-compatible

5. Surface Wetting
   - Solvent must spread on surface (low contact angle)
   - Surface tension matters: Water high (~72 mN/m), ethanol low (~22 mN/m)
   - Surfactants can improve wetting (Tween-20, Triton X-100)

6. Safety/Operational
   - Low toxicity (operator exposure during field sampling)
   - Non-flammable preferred (but most organic solvents flammable)
   - Stability (doesn't degrade explosives or swab)

--------------------------------------------------------------------------------
CANDIDATE SOLVENTS
--------------------------------------------------------------------------------

CATEGORY 1: ALCOHOLS (Current baseline)

1. Ethanol (current standard)
   - Polarity: 5.2 (intermediate)
   - BP: 78°C
   - Advantages: Good all-around performance, low toxicity, validated
   - Disadvantages: Flammable, moderate volatility

2. Methanol
   - Polarity: 6.6 (more polar than ethanol)
   - BP: 65°C (lower than ethanol, more volatile)
   - Advantages: Better for polar explosives (RDX, AN)?
   - Disadvantages: Toxic (operator hazard), evaporates faster

3. Isopropanol (IPA)
   - Polarity: 4.3 (less polar than ethanol)
   - BP: 82°C (higher than ethanol, less volatile)
   - Advantages: Common disinfectant (readily available), stays wet longer
   - Disadvantages: Less polar (worse for PETN/RDX?)

4. Butanol
   - Polarity: 4.0 (low polarity)
   - BP: 117°C (much higher, very low volatility)
   - Advantages: Doesn't evaporate during swabbing
   - Disadvantages: Too non-polar? Higher BP (GC-MS issues?)

CATEGORY 2: KETONES

5. Acetone
   - Polarity: 5.4 (similar to ethanol)
   - BP: 56°C (very volatile!)
   - Advantages: Excellent for TNT and aromatic compounds, fast drying for GC-MS
   - Disadvantages: Evaporates in seconds (swab dries before finishing), flammable

6. Methyl Ethyl Ketone (MEK)
   - Polarity: 4.5
   - BP: 80°C (less volatile than acetone)
   - Advantages: Ketone benefits without extreme volatility
   - Disadvantages: Toxic, less common in labs

CATEGORY 3: NITRILES

7. Acetonitrile
   - Polarity: 6.2 (high polarity)
   - BP: 82°C
   - Advantages: Standard LC-MS solvent, good for wide polarity range, mixes with water
   - Disadvantages: Toxic, expensive, less common in field operations

CATEGORY 4: WATER-BASED

8. Water (deionized)
   - Polarity: 9.0 (highest polarity)
   - BP: 100°C
   - Advantages: Non-toxic, non-flammable, best for ionic explosives (AN, KNO₃, urea nitrate)
   - Disadvantages: High surface tension (poor wetting), not GC-compatible (needs solvent exchange)

9. Water + Surfactant
   - Water + 0.1% Tween-20 or Triton X-100
   - Advantages: Reduced surface tension (better wetting), maintains water polarity
   - Question: Does surfactant interfere with GC-MS?

10. Water + Methanol (50:50)
    - Mixed solvent system
    - Advantages: Intermediate polarity, good wetting, LC-MS compatible
    - Disadvantages: Two-component system (more complex)

CATEGORY 5: HALOGENATED (Specialized)

11. Dichloromethane (DCM)
    - Polarity: 3.4 (low)
    - BP: 40°C (very volatile)
    - Advantages: Excellent for non-polar compounds, used in EPA methods
    - Disadvantages: Toxic, carcinogen, very volatile

12. Chloroform
    - Polarity: 4.4
    - BP: 61°C
    - Advantages: Good for intermediate polarity compounds
    - Disadvantages: Toxic, carcinogen, banned in many jurisdictions

CATEGORY 6: MIXED SOLVENTS (Novel)

13. Ethanol:Acetone (1:1)
    - Combines alcohol and ketone benefits
    - Advantages: Broad polarity range, good for mixed explosive contamination
    - Disadvantages: Intermediate volatility (moderate evaporation)

14. Ethanol:Water (1:1)
    - Increased polarity vs pure ethanol
    - Advantages: Better for ionic explosives, reduced flammability
    - Disadvantages: High BP (GC-MS drying time)

15. Acetonitrile:Water (1:1)
    - Standard LC-MS mobile phase
    - Advantages: Excellent for polar explosives, good wetting
    - Disadvantages: Requires LC-MS analysis (not GC-compatible)

--------------------------------------------------------------------------------
RESEARCH QUESTIONS (Solvent Optimization)
--------------------------------------------------------------------------------

PRIMARY QUESTIONS:

Q1: Is ethanol the optimal solvent for PETN and RDX, or do alternatives perform better?
    - Test: 5-8 solvents × 2 analytes × 2 surfaces = 20-32 conditions
    - Fixed: Optimal pressure (200g), cotton swabs, ASTRA pattern
    - Hypothesis: Ethanol is near-optimal (within 10% of best performer)

Q2: Do different explosive classes require different solvents?
    - Test: 4 solvents × 4 analytes (PETN, RDX, TNT, TATP) = 16 conditions
    - Hypothesis: 
      * Ethanol best for nitrate esters (PETN)
      * Acetone best for aromatics (TNT)
      * Water best for ionic (if AN tested)
      * Acetonitrile best for nitramines (RDX)

Q3: Does solvent volume affect recovery efficiency?
    - Current: 100µL (arbitrary choice)
    - Test: 50µL, 100µL, 200µL, 500µL × 2 analytes × 2 surfaces = 16 conditions
    - Hypothesis: Diminishing returns above ~150µL (excess solvent dilutes extract)

Q4: Does solvent volatility matter? (Does swab dry out during swabbing?)
    - Compare acetone (very volatile), ethanol (moderate), IPA (low)
    - Same volume applied (100µL)
    - Question: Does acetone evaporate before swabbing completes (~30s)?
    - Measure: Swab mass before/after swabbing (quantify evaporation loss)

Q5: Can surfactants improve water-based swabbing?
    - Pure water vs water + 0.1% Tween-20 vs ethanol (control)
    - Measure contact angle on surfaces (wetting improvement)
    - Test recovery efficiency for PETN, RDX, and AN (if available)
    - Hypothesis: Surfactant improves recovery by 20-40% vs pure water

Q6: Do mixed solvents offer advantages for multi-analyte recovery?
    - Scenario: Surface contaminated with PETN+TNT mixture (different polarities)
    - Test: Pure ethanol, pure acetone, 1:1 mixture
    - Hypothesis: Mixture recovers both analytes better than single solvent

SECONDARY QUESTIONS:

Q7: Does solvent choice interact with surface type?
    - High-SFE surfaces (glass) → need polar solvents?
    - Low-SFE surfaces (ABS) → need non-polar solvents?
    - Test: Solvent × Surface interaction term in ANOVA

Q8: Does solvent affect GC-MS performance (beyond explosive recovery)?
    - Acetone (BP 56°C) vs ethanol (BP 78°C) vs IPA (BP 82°C)
    - Question: Does solvent choice affect inlet contamination rate?
    - Metric: PETN drift rate (signal degradation per sample)

Q9: What is the cost-benefit trade-off?
    - Ethanol: ~$0.50/L
    - Acetone: ~$0.30/L
    - Acetonitrile: ~$50/L (100× more expensive!)
    - If acetonitrile gives 15% better recovery, is cost justified?

Q10: Can solvent be eliminated entirely with better pressure? (Extreme test)
    - Current finding: Dry swabbing gives ~0.5% recovery (terrible)
    - Question: Does 1000g pressure improve dry swabbing to useful levels?
    - Test: Dry swabbing at 200g, 500g, 1000g, 2000g (ASTRA modification needed)
    - Hypothesis: Even extreme pressure insufficient without solvent

--------------------------------------------------------------------------------
PROPOSED EXPERIMENTAL DESIGNS (Solvent Optimization)
--------------------------------------------------------------------------------

DESIGN 1: Solvent Screening Study

Objective: Rank solvents for PETN/RDX recovery

Selected Solvents (8 total):
- Ethanol (baseline)
- Methanol (more polar alcohol)
- Isopropanol (less polar alcohol)
- Acetone (ketone)
- Acetonitrile (nitrile)
- Water (extreme polar)
- Ethanol:Acetone 1:1 (mixed)
- Ethanol:Water 1:1 (mixed)

Design: 8 solvents × 2 analytes × 2 surfaces = 32 conditions × 3 replicates = 96 samples

Fixed Parameters:
- Pressure: 200g (optimal from Amy's DoE)
- Swab: Cotton Q-tip
- Volume: 100µL per swab
- Surface loading: 10µg PETN or RDX
- ASTRA serpentine pattern

Timeline: 4-6 weeks

Analysis:
- ANOVA: Recovery ~ Solvent + Analyte + Surface + Solvent:Analyte + (random)
- Post-hoc comparisons: Tukey HSD
- Rank solvents by recovery %
- Identify if ranking changes by analyte (interaction effect)

Expected Outcome:
- Top 3 solvents identified for deeper study
- Confirm ethanol performance (top tier or suboptimal?)
- Test hypotheses: Acetone better for TNT? Water better for ionic explosives?

DESIGN 2: Solvent Volume Optimization

Objective: Determine optimal volume (balance dissolution vs dilution)

Selected Solvents: Ethanol (baseline) + best performer from Design 1

Volumes Tested: 25µL, 50µL, 100µL, 200µL, 500µL

Design: 2 solvents × 5 volumes × 2 analytes × 2 surfaces = 40 conditions

Analysis:
- Plot Recovery vs Volume (identify plateau or maximum)
- Fit models: 
  * Saturation curve: Recovery = Max × (1 - e^(-k*Volume))
  * Linear-plateau: Recovery increases then plateaus
- Determine minimum effective volume (cost savings)
- Determine maximum useful volume (no benefit beyond X µL)

Timeline: 3-4 weeks

Expected Outcome:
- Optimal volume likely 100-200µL (current 100µL is reasonable)
- Below 50µL: Incomplete dissolution, swab dries out
- Above 200µL: Dilution effects, no recovery improvement

DESIGN 3: Multi-Analyte Mixed Solvent Study

Objective: Test if mixed solvents better for diverse explosive mixtures

Contamination: PETN + TNT co-deposited (simulate Semtex-like mixture)

Solvents Tested:
- Pure ethanol (polar, good for PETN)
- Pure acetone (less polar, good for TNT)
- Ethanol:Acetone 1:1 (compromise)
- Ethanol:Acetone 2:1 (ethanol-rich)
- Ethanol:Acetone 1:2 (acetone-rich)

Design: 5 solvent mixtures × 2 surfaces × 3 replicates = 30 samples

Metrics:
- PETN recovery %
- TNT recovery %
- Combined recovery (sum or weighted average)

Analysis:
- Does mixture optimize BOTH analytes vs single solvents?
- Identify optimal ratio (1:1, 2:1, or 1:2?)
- Compare to single-solvent performance for each analyte

Timeline: 2-3 weeks

DESIGN 4: Solvent Volatility Test (Evaporation Study)

Objective: Quantify solvent loss during swabbing, test if it affects recovery

Solvents: Acetone (BP 56°C), Ethanol (BP 78°C), IPA (BP 82°C), Water (BP 100°C)

Procedure:
1. Pre-weigh dry swab
2. Add 100µL solvent, weigh immediately (T=0)
3. Swab surface with ASTRA (~30s), weigh immediately (T=30s)
4. Measure mass loss = evaporated volume
5. Correlate evaporation loss with recovery efficiency

Design: 4 solvents × 2 surfaces × 3 replicates = 24 samples (for weighing)
         4 solvents × 2 surfaces × 2 analytes × 3 replicates = 48 samples (for recovery)

Analysis:
- Calculate % solvent retained after swabbing
- Test hypothesis: Recovery correlates with retained solvent
- Identify threshold: Below X% retention, recovery drops significantly

Expected Findings:
- Acetone: ~20-30% evaporates during swabbing (major loss)
- Ethanol: ~10-15% evaporates (moderate loss)
- IPA: ~5-10% evaporates (minor loss)
- Water: <5% evaporates (negligible loss)

Question: Does acetone's evaporation negate its polarity advantage?

Timeline: 3-4 weeks

DESIGN 5: Analyte-Specific Solvent Matching

Objective: Test hypothesis that optimal solvent depends on analyte chemistry

Analytes (4 classes):
- PETN (nitrate ester, moderately polar)
- RDX (nitramine, polar)
- TNT (aromatic nitro, less polar)
- TATP or AN (if available - peroxide or ionic, very polar)

Solvents (5 selected from Design 1):
- Methanol (high polarity)
- Ethanol (intermediate polarity)
- Acetone (intermediate-low polarity)
- Acetonitrile (high polarity)
- Water (extreme polarity)

Design: 4 analytes × 5 solvents × 2 surfaces = 40 conditions × 3 replicates = 120 samples

Analysis:
- Two-way ANOVA: Recovery ~ Analyte * Solvent + Surface + (random)
- Test Analyte:Solvent interaction (key hypothesis)
- Create heatmap: Rows=analytes, Columns=solvents, Color=recovery%
- Identify optimal solvent per analyte

Expected Patterns:
- PETN, RDX: Ethanol, methanol, acetonitrile best (polar)
- TNT: Acetone best (less polar)
- TATP/AN: Water, acetonitrile best (very polar/ionic)

Timeline: 6-8 weeks

Publication: "Analyte-specific solvent selection for explosive trace recovery: 
              systematic optimization study"

--------------------------------------------------------------------------------
EXPECTED OUTCOMES (Solvent Optimization)
--------------------------------------------------------------------------------

SCENARIO A: Ethanol is near-optimal (within 10% of best)

Outcome:
- Validate current practice (ethanol is good compromise)
- Identify specialized alternatives (acetone for TNT-specific operations)
- Publish: "Systematic solvent evaluation confirms ethanol as general-purpose standard"
- Operational impact: Continue ethanol use, update SOPs with analyte-specific alternatives

SCENARIO B: Better alternatives identified (>20% improvement)

Outcome:
- Recommend solvent switch for general use
- Example: If acetonitrile gives 25% better recovery for PETN/RDX, justify cost
- Publish: "Acetonitrile replaces ethanol as optimal swabbing solvent"
- Operational impact: Major SOP revision, procurement changes

SCENARIO C: Mixed solvents optimal (no single best)

Outcome:
- Develop universal mixed solvent (e.g., ethanol:acetone 1:1)
- Addresses multi-analyte scenarios (operational reality)
- Publish: "Optimized solvent mixture for broad-spectrum explosive recovery"
- Operational impact: Field kits contain pre-mixed solvent

SCENARIO D: Analyte-specific optima (interaction significant)

Outcome:
- Create decision matrix: "Use solvent X for analyte Y"
- Training emphasizes solvent selection based on intelligence/context
- Publish: "Analyte-specific solvent selection guide for forensic explosives"
- Operational impact: Multiple solvents in field kits, training required

PUBLICATIONS (Estimated):
- Paper 1: "Comprehensive solvent screening for PETN/RDX recovery" (Year 1)
- Paper 2: "Solvent volume optimization for explosive trace swabbing" (Year 1-2)
- Paper 3: "Multi-analyte solvent selection: systematic comparison across explosive classes" (Year 2)

================================================================================
RESEARCH DIRECTION 4: SWABBING PATTERN AND MULTI-PASS OPTIMIZATION
================================================================================

RATIONALE:
Amy selected serpentine pattern with 5mm spacing as "optimal" based on UV powder 
studies, but was this rigorously tested? Operational contamination is non-uniform, 
and multi-pass strategies may improve recovery.

--------------------------------------------------------------------------------
RESEARCH QUESTIONS (Swabbing Patterns)
--------------------------------------------------------------------------------

Q1: Does swabbing pattern affect recovery efficiency?
    - Patterns to test:
      * Serpentine (current - continuous back-and-forth)
      * Raster (parallel lines, lift between lines)
      * Spiral (inside-out or outside-in)
      * Cross-hatch (orthogonal passes, 0° then 90°)
      * Perimeter-first (edges then interior)
      * Random walk (stochastic pattern)
    - Hypothesis: Continuous patterns (serpentine, spiral) better than lift-return patterns (raster)

Q2: Does line spacing affect recovery?
    - Current: 5mm spacing (empirical choice)
    - Test: 2mm, 5mm, 10mm, 15mm spacing
    - Hypothesis: Tighter spacing = better coverage = better recovery (but diminishing returns)
    - Balance: Coverage vs time (2mm spacing takes 3× longer than 10mm)

Q3: Do edge effects matter?
    - Observation: Explosive particles accumulate at edges (surface tension, handling)
    - Question: Do current patterns miss edges?
    - Test: Standard serpentine vs perimeter-first serpentine
    - Measure: Edge recovery (outer 1cm) vs interior recovery (central area)

Q4: Does swabbing velocity affect recovery?
    - Current: ~30mm/s (not optimized)
    - Test: 10mm/s (slow), 30mm/s (moderate), 50mm/s (fast)
    - Hypothesis: Slower = more contact time = better pickup
    - BUT: Slower = swab dries out (solvent evaporates)
    - Find optimal balance

Q5: Do multiple passes improve recovery?
    - Single pass (current): One swabbing motion over surface
    - Double pass: Swab same area twice (same swab, continuous motion)
    - Orthogonal pass: Swab at 0° then 90° (cross-hatch)
    - Hypothesis: Second pass recovers residual explosive missed by first pass
    - Question: Is improvement worth 2× time cost?

Q6: Does dwell time matter? (Pressure hold vs continuous motion)
    - Standard: Continuous motion at constant velocity
    - Alternative: Pause at grid points (apply pressure for 1-2s, then move)
    - Hypothesis: Dwell allows solvent to dissolve explosive (diffusion-limited)
    - Question: Does this improve recovery for aged/embedded deposits?

Q7: Does coverage percentage correlate with recovery?
    - Methodology: Use UV powder (Amy's Study 1 approach) to visualize coverage
    - Vary spacing, pattern, velocity → measure coverage %
    - Then test explosive recovery under same conditions
    - Test hypothesis: Linear relationship (90% coverage = 90% recovery)?
    - Or threshold effect (>80% coverage needed for good recovery)?

--------------------------------------------------------------------------------
PROPOSED EXPERIMENTAL DESIGNS (Pattern Optimization)
--------------------------------------------------------------------------------

DESIGN 1: Pattern Comparison Study

Objective: Identify optimal swabbing pattern for recovery

Patterns Tested (6 total):
1. Serpentine (current baseline)
2. Raster (parallel lines)
3. Spiral (inside-out)
4. Cross-hatch (orthogonal, 0° + 90°)
5. Perimeter-first (edges then serpentine interior)
6. Random walk (computer-generated, >90% coverage)

Design: 6 patterns × 2 analytes × 2 surfaces = 24 conditions × 3 replicates = 72 samples

Fixed Parameters:
- Pressure: 200g
- Solvent: 100µL ethanol
- Spacing: 5mm (for patterns where applicable)
- Velocity: 30mm/s
- Swab: Cotton Q-tip

Metrics:
- Recovery % (primary outcome)
- Time per swab (efficiency)
- Coverage % (via UV powder parallel study)
- Recovery/Time ratio (cost-effectiveness)

Timeline: 4-5 weeks

Analysis:
- ANOVA: Recovery ~ Pattern + Analyte + Surface + (random)
- Post-hoc: Rank patterns by recovery %
- Correlation: Coverage % vs Recovery % (test linearity)
- Trade-off analysis: Is 10% more recovery worth 50% more time?

DESIGN 2: Line Spacing Optimization

Objective: Determine optimal line spacing (balance coverage vs time)

Spacings Tested: 2mm, 5mm, 10mm, 15mm, 20mm

Pattern: Serpentine (best from Design 1)

Design: 5 spacings × 2 analytes × 2 surfaces = 20 conditions × 3 replicates = 60 samples

Parallel UV Powder Study:
- Same 5 spacings on UV-powder contaminated surfaces
- Photograph under UV (like Amy's Study 1)
- ImageJ analysis: Calculate coverage %
- Correlate coverage % with explosive recovery %

Metrics:
- Recovery % vs spacing (expect declining recovery with wider spacing)
- Time per swab vs spacing (linear decrease with wider spacing)
- Coverage % vs spacing (from UV powder)

Timeline: 3-4 weeks

Analysis:
- Fit models to Recovery vs Spacing data:
  * Exponential decay: Recovery = Max × e^(-k * Spacing)
  * Power law: Recovery = a * Spacing^(-b)
- Identify spacing where recovery drops below 90% of maximum
- Cost-benefit: Select spacing that maximizes Recovery/Time ratio

Expected Outcome:
- 2mm spacing: ~95% recovery, 60s per swab
- 5mm spacing: ~90% recovery, 30s per swab (current)
- 10mm spacing: ~75% recovery, 15s per swab
- Recommendation: 5mm or tighter (current choice validated or tightened)

DESIGN 3: Multi-Pass Strategy Study

Objective: Test if multiple swab passes improve recovery

Strategies:
1. Single pass (0°, serpentine) - baseline
2. Double pass (same swab, 0° twice)
3. Orthogonal pass (cross-hatch, 0° then 90°)
4. Fresh swab second pass (new swab for second pass)

Design: 4 strategies × 2 analytes × 2 surfaces = 16 conditions × 3 replicates = 48 samples

Special Analysis:
- For double-pass and orthogonal pass: Analyze first pass swab separately from second pass swab
- Measure:
  * First pass recovery (standard)
  * Second pass recovery (residual on surface)
  * Total recovery (sum)
- Calculate: Second pass improvement = (Total - First) / First × 100%

Hypothesis Testing:
- H1: Second pass recovers <10% additional (not worth time)
- H2: Second pass recovers 20-40% additional (worthwhile for critical samples)
- H3: Orthogonal pass better than same-direction repeat

Timeline: 3-4 weeks

Expected Outcome:
- Single pass: ~16% recovery (Amy's baseline)
- Double pass (same direction): ~20% recovery (+25% improvement)
- Orthogonal pass: ~24% recovery (+50% improvement)
- Fresh swab second pass: ~28% recovery (+75% improvement)

Recommendation: 
- Orthogonal pass for critical samples (crime scenes)
- Single pass for routine screening (cost-effective)

DESIGN 4: Velocity Optimization

Objective: Determine optimal swabbing velocity

Velocities Tested: 10mm/s (slow), 30mm/s (moderate), 50mm/s (fast), 100mm/s (very fast)

Design: 4 velocities × 2 analytes × 2 surfaces = 16 conditions × 3 replicates = 48 samples

Fixed: Serpentine pattern, 5mm spacing, 200g pressure, 100µL ethanol

Parallel Study: Solvent Retention vs Velocity
- Weigh swabs before/after swabbing at each velocity
- Measure evaporation loss
- Correlate retained solvent with recovery %

Hypothesis:
- 10mm/s: High recovery (long contact time), but long duration (~90s)
- 30mm/s: Good recovery (current), moderate duration (~30s)
- 50mm/s: Lower recovery (less contact time), fast (~15s)
- 100mm/s: Poor recovery (insufficient contact), very fast (~8s)

Timeline: 3-4 weeks

Analysis:
- Plot Recovery vs Velocity (expect declining curve)
- Plot Retained_Solvent vs Velocity (expect declining curve)
- Test correlation: Recovery ~ Retained_Solvent (mediation analysis)
- Identify optimal velocity: Maximize Recovery/Time ratio

DESIGN 5: Edge vs Interior Recovery

Objective: Test if edges are under-sampled by current pattern

Procedure:
1. Deposit PETN in two patterns:
   a) Uniform across entire 10×10cm surface (current method)
   b) Edge-only (outer 1cm perimeter)
   c) Interior-only (central 8×8cm area)
2. Swab with standard serpentine pattern
3. Compare recovery % for each deposition pattern

Design: 3 deposition patterns × 2 surfaces = 6 conditions × 5 replicates = 30 samples

Analysis:
- Compare recovery %: Uniform vs Edge vs Interior
- If Edge < Interior: Edges are missed (need perimeter-first pattern)
- If Edge = Interior: Current pattern adequate

Alternate Test: Perimeter-First vs Standard
- Uniform contamination (10µg PETN)
- Test: Standard serpentine vs perimeter-first serpentine
- Measure: Does perimeter-first improve recovery?

Timeline: 2-3 weeks

--------------------------------------------------------------------------------
EXPECTED OUTCOMES (Pattern Optimization)
--------------------------------------------------------------------------------

SCENARIO A: Current pattern is near-optimal

Outcome:
- Serpentine with 5mm spacing validated (Amy's choice was good)
- Minor improvements: Tighten spacing to 3-4mm for critical samples
- Publish: "Systematic swabbing pattern optimization validates serpentine approach"
- Operational impact: Minimal changes to current ASTRA programming

SCENARIO B: Significant improvements identified

Outcome:
- Example: Orthogonal pass gives 50% more recovery
- Recommendation: Implement cross-hatch pattern for all samples
- ASTRA software update: Add cross-hatch mode
- Publish: "Cross-hatch swabbing pattern improves explosive recovery by 50%"
- Operational impact: Update SOPs, reprogram ASTRA

SCENARIO C: Context-dependent optimization

Outcome:
- High-priority samples: Orthogonal pass, 2mm spacing, slow velocity (maximize recovery)
- Routine screening: Single pass, 10mm spacing, fast velocity (maximize throughput)
- Publish: "Risk-based swabbing strategies: optimizing pattern for sample priority"
- Operational impact: Training on pattern selection based on case context

PUBLICATIONS (Estimated):
- Paper 1: "Swabbing pattern optimization for automated explosive trace recovery" (Year 1)
- Paper 2: "Multi-pass strategies for maximizing trace explosive recovery" (Year 1-2)

================================================================================
RESEARCH DIRECTION 5: REAL-WORLD CONTAMINATION SCENARIOS
================================================================================

RATIONALE:
Amy's work uses clean, pre-spiked laboratory surfaces with known quantities. 
Operational samples differ: low quantities, irregular distribution, aged deposits, 
interfering contaminants. How do lab-optimized parameters perform in realistic scenarios?

--------------------------------------------------------------------------------
RESEARCH QUESTIONS (Real-World Scenarios)
--------------------------------------------------------------------------------

Q1: How does recovery efficiency scale with contamination level?
    - Lab studies: 10µg PETN (relatively high, easy to detect)
    - Operational range: 10ng to 1µg (1-1000× lower)
    - Test: Recovery % at 10µg, 1µg, 100ng, 10ng, 1ng
    - Hypothesis: Recovery % constant (mass-independent) OR decreases at low levels (detection limit)

Q2: Does aging affect recovery efficiency?
    - Lab studies: Fresh deposits (analyzed same day)
    - Operational: Days to weeks to months old
    - Test: Recovery after 1 day, 1 week, 1 month, 3 months, 6 months aging
    - Aging conditions: Indoor (dark, 20°C, 50% RH) vs outdoor (sunlight, temperature cycling)
    - Hypothesis: PETN degrades over time (UV photolysis, hydrolysis) → lower recovery

Q3: Do surface contaminants interfere with recovery?
    - Lab studies: Clean surfaces
    - Operational: Dirty surfaces (oils, soils, dust, cosmetics, hand lotion)
    - Test: Clean steel vs steel + oil vs steel + soil vs steel + lotion
    - Hypothesis: Organic films (oil, lotion) block explosive-surface contact → reduce adhesion → INCREASE recovery?
    - Or: Contaminants absorbed into swab → dilute explosive → DECREASE recovery?

Q4: Does contact transfer simulate realistic contamination?
    - Lab studies: Pipette deposition (uniform film)
    - Operational: Fingerprint-like transfer (non-uniform, localized)
    - Test: 
      a) Contaminate hands with PETN
      b) Touch steel surface (5s contact)
      c) Swab contact area with ASTRA
    - Compare: Contact transfer vs pipette deposition (same total mass)
    - Hypothesis: Non-uniform deposits harder to recover (localized high concentration)

Q5: Does secondary/tertiary transfer reduce recoverability?
    - Primary: Explosive directly deposited on surface
    - Secondary: Touch explosive, then touch surface
    - Tertiary: Touch secondary surface, then touch third surface
    - Test: Recovery % at each transfer level
    - Hypothesis: Exponential decay with transfer number (90% loss per transfer?)

Q6: Can ASTRA recover post-blast residue?
    - Lab studies: Pre-blast (intact explosive molecules)
    - Post-blast: Decomposition products, particles, soot
    - Challenge: Obtain authentic post-blast samples (controlled detonation)
    - Test: Swab metal fragments, concrete, etc. from blast chamber
    - Hypothesis: Particulate residue easier to recover (not chemically bound)?
    - Or: Embedded in surface (harder to recover)?

--------------------------------------------------------------------------------
PROPOSED EXPERIMENTAL DESIGNS (Real-World Scenarios)
--------------------------------------------------------------------------------

DESIGN 1: Concentration Range Study (Limit of Detection)

Objective: Determine lowest contamination level where recovery is reliable

Contamination Levels: 10µg, 1µg, 100ng, 10ng, 1ng PETN per 10×10cm surface

Design: 5 levels × 2 surfaces (steel, ABS) × 3 replicates = 30 samples

Fixed: Optimal parameters from Amy's work (200g, 100µL ethanol, serpentine)

Controls:
- Negative controls: Clean surfaces (no PETN)
- Blanks: Ethanol analyzed directly (no swabbing)

Metrics:
- Recovery % (expected to be constant if method is robust)
- Absolute mass recovered (ng)
- Signal-to-noise ratio (GC-MS peak height / baseline noise)
- Detection rate (% samples above LOD)

Analysis:
- Plot: Recovery % vs Contamination Level (log scale)
- Determine LOD (3× baseline noise) and LOQ (10× baseline noise)
- Compare: Amy's lab LOD vs operational LOD (are they same?)

Expected Results:
- 10µg, 1µg, 100ng: ~16% recovery (constant %, method is mass-independent)
- 10ng: ~10-14% recovery (approaching LOD, some samples below quantitation)
- 1ng: <50% detected (below reliable detection limit)

Outcome: Define operational detection limit for ASTRA method

Timeline: 3-4 weeks

DESIGN 2: Aging Study (Environmental Stability)

Objective: Quantify effect of aging on recovery efficiency

Aging Conditions:
- Indoor (dark, 20±2°C, 50±10% RH)
- Outdoor (sunlight, natural temperature/humidity cycling)

Time Points: 0 (fresh), 1 day, 1 week, 1 month, 3 months, 6 months

Design: 2 conditions × 6 time points × 2 analytes (PETN, RDX) × 2 surfaces = 48 samples

Preparation:
- Spike 48 steel coupons with PETN or RDX (10µg each)
- Store half indoors (dark cabinet), half outdoors (weathering rack)
- Swab at each time point (destructive sampling, not same coupons)

Metrics:
- Recovery % vs time (expect decline for PETN, stable for RDX?)
- Visual inspection: Discoloration, surface changes
- Analyte degradation products: Check GC-MS chromatograms for decomposition peaks

Analysis:
- Fit decay curves: Recovery(t) = Recovery(0) × e^(-k*t)
- Calculate half-life: t_50 = ln(2) / k
- Compare indoor vs outdoor (expect faster outdoor degradation)
- Compare PETN vs RDX (PETN less stable?)

Expected Results:
- PETN indoor: 10-20% decline over 6 months (hydrolysis, minor)
- PETN outdoor: 40-60% decline over 6 months (UV photolysis, significant)
- RDX indoor: <5% decline (very stable)
- RDX outdoor: 10-20% decline (more stable than PETN)

Outcome: Establishes time limits for swab sampling ("within 1 month" guidance)

Timeline: 6 months study duration (but samples can be analyzed in batches)

DESIGN 3: Surface Contamination Interference Study

Objective: Test recovery in presence of realistic surface contaminants

Contaminants Tested:
1. None (clean steel, baseline)
2. Mineral oil (simulate greasy surfaces)
3. Hand lotion (cosmetic contamination)
4. Soil (dust/dirt, simulate outdoor surfaces)
5. Fingerprint residue (touch surface with clean hands)
6. Mixed (oil + soil, realistic combo)

Contamination Protocol:
- Apply contaminant to steel surface (thin film, ~10 µL oil or smear of lotion)
- Let dry/settle (10 min)
- Apply PETN (10µg) on top of contaminant
- Swab with ASTRA (standard parameters)

Design: 6 conditions × 2 analytes × 3 replicates = 36 samples

Metrics:
- Recovery % (compare to clean baseline)
- GC-MS chromatogram quality (contaminants cause interference?)
- Visual observation: Does swab pick up visible soil/oil?

Analysis:
- ANOVA: Recovery ~ Contaminant + Analyte + (random)
- Post-hoc: Compare each contaminant to clean baseline
- Hypothesis tests:
  * Oil reduces recovery (organic film barrier)
  * Soil increases recovery (mechanical abrasion helps pickup?)
  * Lotion reduces recovery (sticky residue traps explosive)

Expected Results:
- Clean: 16% recovery (baseline)
- Mineral oil: 10-12% recovery (organic film interferes)
- Hand lotion: 8-10% recovery (worst case, sticky trap)
- Soil: 14-18% recovery (minimal impact or slight improvement)
- Fingerprint: 15-17% recovery (minimal impact)
- Mixed: 10-12% recovery (similar to oil alone)

Outcome: Guidance on pre-cleaning surfaces (or not) before swabbing

Timeline: 2-3 weeks

DESIGN 4: Contact Transfer Study (Realistic Deposition)

Objective: Compare pipette deposition (lab) vs contact transfer (operational)

Deposition Methods:
1. Pipette (current lab method): 10 × 10µL drops, uniform distribution
2. Fingerprint transfer: Contaminate fingertip, press on surface (5s contact)
3. Palm transfer: Contaminate palm, press on surface (3s contact)
4. Object transfer: Contaminate small object (coin), press on surface

Experimental Procedure:
Method 1 (Pipette): Standard Amy method
Method 2 (Fingerprint): 
  - Dissolve 100µg PETN in 100µL ethanol
  - Pipette 10µL onto fingertip (= 10µg PETN)
  - Let ethanol evaporate (1 min)
  - Press fingertip onto steel coupon (5s, moderate pressure)
  - Swab with ASTRA

Design: 4 methods × 2 surfaces × 3 replicates = 24 samples

Parallel Study: Image transfer patterns
- Use UV powder instead of PETN
- Photograph deposits under UV light
- Quantify uniformity (coefficient of variation in pixel intensity)

Metrics:
- Recovery % (compare methods)
- Deposit uniformity (from UV images)
- Correlation: Uniformity vs Recovery

Hypothesis:
- Pipette: High uniformity, baseline recovery (~16%)
- Fingerprint: Low uniformity (ridge patterns), similar recovery
- Palm: Very non-uniform (localized pressure points), lower recovery?
- Object: Depends on object texture

Timeline: 3-4 weeks

DESIGN 5: Transfer Chain Study (Secondary/Tertiary)

Objective: Model transfer dynamics (how much explosive moves per contact)

Procedure:
- Primary surface (donor): Load with 100µg PETN
- Touch with clean fingertip (5s contact)
- Secondary surface: Touch with contaminated fingertip → swab → measure (= Secondary transfer)
- Tertiary surface: Touch secondary surface with clean fingertip, then touch tertiary → swab → measure

Design: 3 transfer levels × 2 contact times (5s, 15s) × 3 replicates = 18 samples

Metrics:
- Primary: 100µg deposited (known)
- Secondary: X µg transferred (measure)
- Tertiary: Y µg transferred (measure)
- Calculate transfer efficiency: Secondary/Primary, Tertiary/Secondary

Analysis:
- Exponential decay model: Amount(n) = Amount(0) × r^n, where r = transfer rate
- Determine r (expected ~0.1-0.3, i.e., 10-30% transfers per contact)
- Test contact time effect: Does longer contact increase transfer?

Expected Results:
- Primary → Secondary: 10-30% transfer (70-90% remains on primary)
- Secondary → Tertiary: 10-30% of secondary (1-9% of original)
- Contact time: Longer contact = slightly more transfer (20% vs 15%)

Outcome: Models for trace transfer in forensic reconstructions

Timeline: 2-3 weeks

DESIGN 6: Post-Blast Residue Recovery (Advanced/Optional)

Objective: Test ASTRA on authentic post-blast samples (if access available)

Requirements:
- Controlled detonation facility (Dstl, military range, etc.)
- Small-scale blasts (10-100g PETN or RDX)
- Surfaces positioned at known distances (1m, 2m, 5m)
- Post-blast: Collect surfaces, swab with ASTRA

Design: 
- 3 explosives (PETN, RDX, TNT)
- 3 distances (1m, 2m, 5m from blast center)
- 2 surface types (steel, ABS)
- = 18 post-blast samples

Controls:
- Pre-blast reference: Pipette same explosive on surfaces (not detonated)
- Negative: Surfaces not exposed to blast

Metrics:
- Detect explosive residue (yes/no)
- Quantify mass recovered (if above LOD)
- Compare pre-blast vs post-blast recovery (are they different?)

Challenges:
- Safety (explosive handling, detonation)
- Access (requires specialized facility)
- Decomposition products (may need different GC-MS method)

Expected Results:
- Close range (1m): Heavy particulate deposition, high recovery
- Medium range (2m): Moderate deposition, detectable
- Far range (5m): Trace levels, near LOD
- Post-blast recovery similar to pre-blast (particles easier to pick up?)

Timeline: 6-12 months (coordination, permissions, execution)

Publication Value: HIGH (very limited literature on post-blast swabbing)

--------------------------------------------------------------------------------
EXPECTED OUTCOMES (Real-World Scenarios)
--------------------------------------------------------------------------------

OPERATIONAL GUIDANCE OUTPUTS:

1. Detection Limit Guidance
   - "ASTRA reliable for surface loadings >50ng/cm²"
   - "Below 5ng/cm², recovery becomes inconsistent"

2. Aging Recommendations
   - "Swab samples within 1 week for PETN (UV degradation risk)"
   - "RDX stable for 6+ months outdoors"

3. Surface Pre-Treatment
   - "Do NOT clean oily surfaces before swabbing (reduces recovery by 30%)"
   - "Remove loose soil with air blast before swabbing"

4. Transfer Models
   - "Secondary transfer: Expect 10-30% of primary contamination"
   - "Tertiary transfer: Rarely detectable (<1-5% of primary)"

PUBLICATIONS (Estimated):
- Paper 1: "Operational detection limits for automated explosive swabbing" (Year 1)
- Paper 2: "Environmental stability of surface-deposited explosives: aging study" (Year 1-2)
- Paper 3: "Contact transfer dynamics of explosive traces: forensic implications" (Year 2)
- Paper 4: "Post-blast residue recovery via automated swabbing" (Year 2-3, if conducted)

IMPACT:
- Bridges lab validation → operational deployment
- Provides realistic performance expectations for practitioners
- Supports evidence interpretation (transfer vs direct contamination)

================================================================================
RESEARCH DIRECTION 6: STATISTICAL METHODS AND DATA ANALYSIS
================================================================================

RATIONALE:
Amy developed drift correction and QC methods for PETN/RDX. Future analytes 
(TATP, TNT, etc.) may have different degradation profiles. Can methods generalize?
Are there better statistical approaches?

--------------------------------------------------------------------------------
RESEARCH QUESTIONS (Statistical Methods)
--------------------------------------------------------------------------------

Q1: Do drift correction methods generalize to other analytes?
    - Amy's power law model works for PETN
    - 15N-RDX internal standard works for RDX
    - Test: Does power law work for TNT, TATP, other analytes?
    - Or: Need analyte-specific models (exponential, linear, piecewise)?

Q2: Can machine learning improve drift correction?
    - Current: Parametric models (power law, polynomial)
    - Alternative: ML models (random forest, neural networks)
    - Input features: Injection order, QC history, time, inlet temp, etc.
    - Hypothesis: ML captures complex non-linear drift patterns better

Q3: Is there a universal internal standard?
    - 15N-RDX works for RDX only
    - NG failed for PETN (different RT, different lability)
    - Question: Can ONE internal standard correct drift for ALL analytes?
    - Candidate: Deuterated standard with intermediate properties?

Q4: Can QC strategy be optimized?
    - Current: QC every 10 samples (10% overhead)
    - Question: Is this frequency optimal?
    - Too frequent: Wasted instrument time
    - Too infrequent: Drift model poorly constrained
    - Test: QC every 5 vs 10 vs 20 samples → compare correction accuracy

Q5: Can real-time drift detection trigger automatic QC insertion?
    - Adaptive QC: Algorithm monitors peak areas, inserts QC when drift detected
    - Advantage: Reduces QC overhead when drift is stable
    - Increases QC when drift accelerates (inlet contamination buildup)

Q6: Can experimental design be further optimized?
    - Amy used 2³ factorial (8 conditions)
    - Alternative: Response surface methodology (RSM) for continuous factors
    - Example: Pressure (50-200g continuous) vs categorical (50g vs 200g)
    - Advantage: Maps response surface, finds true optimum
    - Disadvantage: More complex, requires more samples

--------------------------------------------------------------------------------
PROPOSED STUDIES (Statistical Methods)
--------------------------------------------------------------------------------

STUDY 1: Drift Correction Method Comparison

Test Drift Models:
1. Power law: Signal(n) = a × n^b (Amy's current method)
2. Exponential: Signal(n) = a × exp(-b × n)
3. Linear: Signal(n) = a - b × n
4. Polynomial (cubic): Signal(n) = a + b×n + c×n² + d×n³
5. Piecewise linear (breakpoint at injection 20)
6. Random forest regression (ML)
7. Neural network (ML)

Test Dataset:
- Use Amy's existing FINEX QC data (hundreds of QC injections)
- Split: 70% training, 30% validation
- Fit each model on training data
- Predict validation data
- Compare: RMSE, MAE, R²

Metrics:
- Prediction accuracy (validation RMSE)
- Model complexity (number of parameters)
- Computational cost (fitting time)

Expected Outcome:
- Power law likely near-optimal (Amy's choice validated)
- ML may improve by 5-10% but adds complexity
- Recommendation: Stick with power law unless ML improvement >20%

Timeline: 1-2 weeks (retrospective analysis, no new data collection)

STUDY 2: Universal Internal Standard Search

Objective: Find one IS that works for multiple analytes

Candidates:
1. Deuterated explosives (if commercially available):
   - d5-TNT (deuterated TNT)
   - d6-RDX (fully deuterated RDX)
   - d5-PETN (if it exists)
2. Stable compounds with intermediate RT:
   - Naphthalene-d8 (common GC-MS IS)
   - Phenanthrene-d10
   - Triphenyl phosphate
3. Polarity-matched compounds:
   - For PETN/RDX: Polar nitro compounds
   - For TNT: Aromatic compounds

Test Procedure:
- Prepare mixed standard: PETN + RDX + TNT + candidate IS
- Run degradation sequence (like Amy's inlet contamination study)
- Measure drift for each analyte
- Calculate IS-corrected ratios
- Compare: Which IS best corrects all three analytes?

Success Criteria:
- IS-corrected drift <5% for all analytes (vs >20% uncorrected)
- Single IS eliminates need for multiple standards (cost savings)

Timeline: 4-6 weeks

Expected Outcome:
- Likely NO universal IS exists (chemistry too different)
- But may find pairs: One IS for PETN+RDX, another for TNT+aromatics

STUDY 3: Adaptive QC Strategy

Objective: Develop algorithm for real-time QC insertion

Algorithm Design:
1. Baseline: Run QC every 10 samples (fixed schedule)
2. Adaptive: 
   - Monitor running average of analyte peak area (last 5 samples)
   - If drift >10% from expected → insert QC immediately
   - If drift <5% → extend interval to 15 samples
   - Update drift model after each QC

Simulation Study:
- Use Amy's existing FINEX data (known drift patterns)
- Simulate adaptive algorithm → count QC insertions
- Compare: Fixed 10% QC vs adaptive QC
- Metrics: Number of QCs used, drift correction accuracy

Expected Results:
- Adaptive uses 20-30% fewer QCs when drift is stable
- But inserts extra QCs when drift accelerates (critical periods)
- Overall: Similar accuracy, less overhead

Implementation:
- Requires software integration (GC-MS control software)
- May be challenging with legacy instruments

Timeline: 2-3 weeks (simulation and algorithm development)

STUDY 4: Response Surface Methodology for Continuous Optimization

Objective: Map continuous response surface for pressure and solvent volume

Current Design: 2³ factorial (categorical: low/high pressure, dry/wet solvent)

RSM Design: Central Composite Design (CCD)
- Pressure: 50g, 125g, 200g, 275g, 350g (5 levels)
- Solvent volume: 0µL, 50µL, 100µL, 150µL, 200µL (5 levels)
- Surface: Steel only (reduce complexity)
- Analyte: PETN only

Design: 13 conditions per surface (CCD star design) × 3 replicates = 39 samples

Analysis:
- Fit quadratic response surface: 
  Recovery = β0 + β1×P + β2×V + β11×P² + β22×V² + β12×P×V
- Visualize: 3D surface plot or contour plot
- Optimize: Find (P*, V*) that maximizes Recovery

Advantages:
- Identifies true optimum (may be 175g, 125µL, not tested in factorial)
- Maps curvature (diminishing returns, thresholds)
- Better for continuous factors

Disadvantages:
- More samples required (39 vs 24 for 2³ factorial)
- More complex analysis

Timeline: 5-6 weeks

Expected Outcome:
- Confirms 200g pressure is near-optimal (Amy's choice)
- May find optimal solvent volume is 150µL (50% more than current 100µL)

--------------------------------------------------------------------------------
EXPECTED OUTCOMES (Statistical Methods)
--------------------------------------------------------------------------------

PUBLICATIONS (Estimated):
- Paper 1: "Comparative evaluation of drift correction methods for thermally labile explosives" (Year 1)
- Paper 2: "Adaptive quality control strategies for high-throughput GC-MS analysis" (Year 2)
- Paper 3: "Response surface optimization of automated swabbing parameters" (Year 2, if RSM pursued)

IMPACT:
- Validates Amy's methodological choices (power law, QC frequency)
- Provides generalizable framework for future analytes
- Reduces analysis costs (fewer QCs via adaptive strategy)

================================================================================
RESEARCH DIRECTION 7: DEPLOYMENT AND VALIDATION STUDIES
================================================================================

RATIONALE:
ASTRA is a lab prototype. Operational use requires ruggedization, validation, 
training, and cost-benefit demonstration. Lower fundamental science novelty, 
but high operational impact.

--------------------------------------------------------------------------------
RESEARCH QUESTIONS (Deployment)
--------------------------------------------------------------------------------

Q1: Can ASTRA be miniaturized for field deployment?
    - Current: Bench-top 3D printer (~30kg, 50×50×50cm)
    - Target: Suitcase-sized (<10kg, <40×30×20cm)
    - Challenges: Maintain pressure accuracy, reduce weight, battery power

Q2: Is ASTRA cost-effective vs manual swabbing?
    - ASTRA cost: ~£500 (hardware) + £200 (software) = £700 per device
    - Training cost: Manual swabbing requires 2-4 weeks training, ongoing proficiency testing
    - Question: Break-even point (number of samples)?

Q3: Do operators accept ASTRA? (Human factors)
    - Ease of use: Touchscreen interface vs G-code programming
    - Trust: Do operators trust automated results?
    - Workflow: Does ASTRA fit into existing lab procedures?

Q4: Does ASTRA reduce inter-lab variability?
    - Hypothesis: Operator variance eliminated → inter-lab variance reduced
    - Test: Replicate FINEX study (37 labs) but with ASTRA
    - Expected: Variance drops from 42-52% to <10% (only analytical variance remains)

Q5: Can ASTRA be validated to forensic standards?
    - ISO 17025 accreditation requirements
    - Proficiency testing (blind samples)
    - Inter-lab comparison
    - Uncertainty quantification

--------------------------------------------------------------------------------
PROPOSED STUDIES (Deployment)
--------------------------------------------------------------------------------

STUDY 1: Portable ASTRA Design and Testing

Engineering Phase (6-9 months):
- Redesign ASTRA with portable components
- Battery-powered (12V Li-ion, 4-6 hour runtime)
- Touchscreen interface (Android tablet, Bluetooth control)
- Ruggedized case (impact-resistant, weatherproof)
- Weight target: <8kg

Field Testing Phase (3 months):
- Deploy to 3-5 operational labs
- Test in realistic conditions (not just clean lab bench)
- Collect feedback: Ease of use, reliability, battery life
- Iterate design based on feedback

Validation:
- Drop test (survives 1m fall)
- Temperature range (5°C to 40°C)
- Humidity range (20% to 80% RH)
- Vibration test (simulated transport)

Timeline: 12-15 months (engineering + testing)

Expected Outcome:
- Functional portable ASTRA prototype
- User manual and training materials
- Deployment-ready for operational trials

STUDY 2: Cost-Benefit Analysis

Data Collection:
- ASTRA costs: Hardware (£500), software (£200), maintenance (£50/year)
- Manual swabbing costs: Training (£1000 per operator), proficiency testing (£200/year per operator)
- Consumables: Swabs (£0.05), solvents (£0.02), same for both methods
- Labor: Manual swabbing 2 min per sample, ASTRA 30s setup + 30s swabbing = 1 min (2× faster?)

Break-Even Analysis:
- Cost per sample: Manual vs ASTRA
- Amortize ASTRA cost over 5 years, 1000 samples/year
- Calculate: ASTRA cost = £700 / (5 years × 1000 samples) = £0.14 per sample (capital)
- Manual training: £1000 / (5 years × 1000 samples) = £0.20 per sample (training)
- Break-even: ASTRA cheaper if >5000 samples over lifetime

Intangible Benefits:
- Reduced operator variance → stronger evidence (value?)
- Consistency → fewer proficiency test failures (cost savings)
- Operator safety → less repetitive strain injury (health benefit)

Timeline: 2-3 months (data collection and analysis)

Publication: "Cost-effectiveness of automated vs manual explosive trace swabbing"

STUDY 3: Inter-Lab Validation Study (Replicate FINEX with ASTRA)

Objective: Demonstrate that ASTRA eliminates inter-operator and inter-lab variance

Design:
- Recruit 10-15 labs (subset of original FINEX participants)
- Provide each lab with ASTRA device (standardized)
- Prepare contaminated surfaces centrally (same as FINEX)
- Each lab swabs samples with ASTRA (not manually)
- Return swabs to central lab (Dundee) for GC-MS analysis

Comparison:
- FINEX manual data (Amy's dataset): Operator variance 42-52%
- ASTRA data: Predicted operator variance <5%

Design: 15 labs × 4 surfaces × 3 replicates = 180 samples

Analysis:
- Variance components:
  * Manual (FINEX): σ²_operator = 42-52%, σ²_analytical = 10-15%
  * ASTRA: σ²_operator = 0%, σ²_lab = <5%, σ²_analytical = 10-15%
- Hypothesis test: Is σ²_lab(ASTRA) significantly lower than σ²_operator(FINEX)?

Timeline: 12-18 months (coordination, execution, analysis)

Publication: HIGH IMPACT - "Automated swabbing eliminates inter-operator variance: 
               international validation study"

STUDY 4: Proficiency Testing

Objective: Validate ASTRA performance via blind testing

Design:
- External provider prepares blind samples (unknown contamination levels)
- 20 samples: 15 contaminated (range 10ng to 10µg), 5 blanks
- Lab swabs with ASTRA, analyzes via GC-MS, reports results
- Provider scores results (accuracy, false positives/negatives)

Forensic Standards:
- ISO 17025 requires proficiency testing for accreditation
- z-score: z = (Lab_result - Reference_value) / σ
- Acceptable: |z| < 2 (within 2 standard deviations of consensus)

Timeline: 3-4 months (obtain samples, analyze, reporting)

Expected Outcome:
- ASTRA achieves |z| < 2 for >90% of samples (acceptable performance)
- Demonstrates reliability for forensic casework

STUDY 5: User Acceptance and Training

Objective: Evaluate operator experience with ASTRA

Method: Qualitative study
- Recruit 20 forensic scientists (mix of FINEX participants and new users)
- Provide 1-hour ASTRA training
- Each user completes 10 swabbing tasks (varied surfaces, analytes)
- Post-study survey:
  * Ease of use (Likert scale 1-5)
  * Confidence in results (Likert scale 1-5)
  * Preference (ASTRA vs manual)
  * Open feedback (qualitative)

Analysis:
- Descriptive statistics (mean ease-of-use score)
- Thematic analysis (qualitative feedback)
- Correlation: Prior experience vs acceptance

Timeline: 2-3 months

Expected Outcome:
- High acceptance (mean score >4/5)
- Concerns: "Loss of operator control", "Black box method"
- Recommendation: Emphasize transparent data logging (pressure traces, etc.)

--------------------------------------------------------------------------------
EXPECTED OUTCOMES (Deployment)
--------------------------------------------------------------------------------

DELIVERABLES:
1. Portable ASTRA prototype (TRL 7-8, field-ready)
2. User manual and training materials
3. Cost-benefit analysis report (evidence for procurement decisions)
4. Validation data package (ISO 17025 accreditation support)
5. Inter-lab study publication (high-impact demonstration)

OPERATIONAL IMPACT:
- ASTRA adopted by 10-20 forensic labs worldwide
- SOPs updated to include automated swabbing option
- Proficiency testing programs incorporate ASTRA
- Commercial development (licensing technology to manufacturers)

TIMELINE: 2-3 years for full deployment and validation

================================================================================

CROSS-CUTTING THEMES AND INTEGRATION OPPORTUNITIES
================================================================================

The research directions above are somewhat independent, but there are synergies:

INTEGRATION 1: Analyte + Solvent + Swab Material (Comprehensive Optimization)

Combine Directions 1, 2, 3 into single mega-study:
- 4 analytes × 3 solvents × 3 swab materials × 2 surfaces = 72 conditions
- Factorial ANOVA: Test all main effects and interactions
- Identify optimal combinations: "For TNT on ABS, use polyester swab with acetone"

Advantage: Efficient (tests interactions)
Disadvantage: Very large study (200+ samples)
Timeline: 12-15 months

Publication: "Comprehensive optimization of swabbing parameters across explosive 
              classes: a factorial approach"

INTEGRATION 2: Real-World Validation of Lab-Optimized Parameters

Combine Directions 4 and 5:
- Use optimal parameters from Amy's DoE (200g, 100µL ethanol, serpentine)
- Test on realistic samples (aged, contaminated, low concentration)
- Question: Do lab optima hold operationally?

Outcome: Either validates lab findings, or identifies need for context-specific protocols

INTEGRATION 3: Machine Learning for Adaptive Optimization

Combine Directions 6 (statistical) and 7 (deployment):
- Develop ML model: Predicts optimal pressure/solvent based on sample characteristics
- Input features: Surface type, analyte, contamination level, age
- Output: Recommended pressure, solvent volume
- Deploy as "smart ASTRA" (auto-adjusts parameters)

Advantage: Context-adaptive swabbing (maximizes recovery for each sample)
Challenge: Requires large training dataset (1000+ samples)
Timeline: 2-3 years (data collection + model development)

Publication: "Machine learning-guided adaptive swabbing for explosive trace recovery"

================================================================================

RECOMMENDED PRIORITIZATION FOR NEXT PhD STUDENT
================================================================================

TIER 1 (ESSENTIAL - High Impact, Logical Extension):

1. Analyte Expansion (Direction 1)
   - TNT and TATP are must-haves
   - Tests generalizability of Amy's findings
   - 12-18 months

2. Real-World Scenarios (Direction 5)
   - Aging, low concentration, surface contamination studies
   - Bridges lab → operational gap
   - 9-12 months

TIER 2 (VALUABLE - Medium-High Impact):

3. Swab Material Optimization (Direction 2)
   - Tests fundamental SFE hypothesis
   - May find significant improvement (>20%)
   - 6-9 months

4. Solvent Optimization (Direction 3)
   - Especially important if TNT added (acetone may be better)
   - 6-9 months

TIER 3 (OPTIONAL - Situational Value):

5. Pattern Optimization (Direction 4)
   - Current serpentine likely adequate
   - Pursue if preliminary data shows large effects
   - 4-6 months

6. Statistical Methods (Direction 6)
   - Refinement rather than new science
   - Pursue if drift issues arise with new analytes
   - 3-6 months

7. Deployment/Validation (Direction 7)
   - Lower science novelty, high operational impact
   - Better suited for post-PhD or industry collaboration
   - 18-24 months (full deployment)

================================================================================

SUGGESTED PhD STRUCTURE (3-4 YEAR TIMELINE)
================================================================================

YEAR 1:
- Literature review and method establishment
- Analyte expansion: TNT optimization (DoE study)
- Solvent screening for TNT
- Publications: 1 methods paper

YEAR 2:
- Analyte expansion: TATP optimization
- Swab material study (comprehensive screening)
- Real-world scenarios: Aging and concentration range studies
- Publications: 2 papers (TATP + swab materials)

YEAR 3:
- Real-world scenarios: Surface contamination, transfer studies
- Comprehensive comparison: All analytes, optimal conditions
- Integration study: Multi-analyte protocol development
- Publications: 2 papers (real-world + comparative)

YEAR 4 (if needed):
- Thesis writing
- Deployment validation (optional)
- Final paper(s)

TOTAL EXPECTED PUBLICATIONS: 5-7 papers (strong PhD output)

================================================================================

FINAL RECOMMENDATION
================================================================================

The STRONGEST future PhD project would be:

"Multi-Analyte Swabbing Optimization: Extending Automated Methods to Diverse 
 Explosive Classes with Operational Validation"

Focus:
1. Add TNT and TATP (2-3 more analytes)
2. Optimize solvent and swab material for each
3. Validate on realistic samples (aged, low concentration, contaminated)
4. Develop multi-analyte protocol recommendations

This builds directly on Amy's foundation, tests generalizability, and delivers 
high operational impact without requiring ASTRA redesign or extensive deployment 
infrastructure.

Expected outcome: Definitive guide for explosive trace swabbing (all major classes)

================================================================================
END OF DOCUMENT
================================================================================

================================================================================
APPENDIX A: CONSOLIDATED INDEX OF ALL 72 RESEARCH QUESTIONS
================================================================================

This is a scanning aid, not a restatement -- the full text of every question
already appears inline within its Research Direction in Section 3 below (and
the Top 7 Priority Questions are given in full in Section 7, Final
Recommendation). Question numbering below follows the original flat 1-72
numbering from the source question list.

  Q1-Q15   -> Section 3.1  Research Direction 1: Analyte Expansion
  Q16-Q26  -> Section 3.2  Research Direction 2: Swab Material Optimization
  Q27-Q36  -> Section 3.3  Research Direction 3: Solvent Optimization
  Q37-Q44  -> Section 3.4  Research Direction 4: Swabbing Pattern and Multi-Pass Optimization
  Q45-Q56  -> Section 3.5  Research Direction 5: Real-World Contamination Scenarios
  Q57-Q62  -> Section 3.6  Research Direction 6: Statistical Methods and Data Analysis
  Q63-Q68  -> Section 3.7  Research Direction 7: Deployment and Validation Studies
  Q69-Q72  -> Section 4    Cross-Cutting Themes and Integration Opportunities

  Top 7 Priority Questions (Tier 1) -> Section 7, Final Recommendation
  (Key Research Questions, Top Priority)

TOTAL: 72 Research Questions

================================================================================
END OF CONSOLIDATED DOCUMENT
================================================================================
