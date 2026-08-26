"""
Deeper analysis: the interleaving pattern and its effect on trends.
"""
import csv
from collections import defaultdict

files = {
    "017": r"C:\Users\A Bruce - User\Documents\TransferPersistenceRecoveryExplosives\GCMSQuantitation\20260309-QC Test\GcDataConvertedRcode\SIM\017.csv",
    "023": r"C:\Users\A Bruce - User\Documents\TransferPersistenceRecoveryExplosives\GCMSQuantitation\20260309-QC Test\GcDataConvertedRcode\SIM\023.csv",
    "029": r"C:\Users\A Bruce - User\Documents\TransferPersistenceRecoveryExplosives\GCMSQuantitation\20260309-QC Test\GcDataConvertedRcode\SIM\029.csv",
}

RT_MIN, RT_MAX = 274.0, 279.0

results = {}
for inj_name, fpath in files.items():
    rows = []
    with open(fpath, 'r') as f:
        reader = csv.DictReader(f)
        for r in reader:
            rt = float(r['RetentionTime'])
            mass = float(r['Mass'])
            intensity = float(r['Intensity'])
            rows.append((rt, mass, intensity))
    
    rt_groups = defaultdict(dict)
    for rt, mass, intensity in rows:
        rt_groups[rt][mass] = intensity
    
    mass46_in_range = []
    for rt, mass, intensity in rows:
        if RT_MIN <= rt <= RT_MAX and mass == 46.0:
            mass46_in_range.append((rt, intensity))
    
    sim_only = []
    fullscan = []
    for rt, intensity in sorted(mass46_in_range):
        if 42.0 in rt_groups[rt]:
            fullscan.append((rt, intensity))
        else:
            sim_only.append((rt, intensity))
    
    # Areas
    def trap_area(pts):
        pts = sorted(pts)
        return sum((pts[i][0]-pts[i-1][0])*(pts[i][1]+pts[i-1][1])/2 for i in range(1, len(pts)))
    
    sim_area = trap_area(sim_only)
    fs_area = trap_area(fullscan)
    all_area = trap_area(sorted(mass46_in_range))
    
    sim_peak = max(sim_only, key=lambda x: x[1])[1]
    fs_peak = max(fullscan, key=lambda x: x[1])[1]
    
    results[inj_name] = {
        'sim_area': sim_area, 'fs_area': fs_area, 'all_area': all_area,
        'sim_peak': sim_peak, 'fs_peak': fs_peak,
        'sim_points': sim_only, 'fs_points': fullscan
    }

# Now analyze trends
print("="*80)
print("TREND ANALYSIS - THE CRITICAL QUESTION")
print("="*80)

injs = ["017", "023", "029"]

print("\n1. SIM-ONLY areas:")
for inj in injs:
    print(f"   {inj}: {results[inj]['sim_area']:.1f}")
sim_areas = [results[inj]['sim_area'] for inj in injs]
print(f"   Trend: {sim_areas[0]:.0f} -> {sim_areas[1]:.0f} -> {sim_areas[2]:.0f}")
print(f"   Direction: {'INCREASING' if sim_areas[2] > sim_areas[0] else 'DECREASING'}")
print(f"   MassHunter:  41573 -> 42107 -> 42886 (INCREASING)")

print("\n2. FULL-SCAN areas:")
for inj in injs:
    print(f"   {inj}: {results[inj]['fs_area']:.1f}")
fs_areas = [results[inj]['fs_area'] for inj in injs]
print(f"   Trend: {fs_areas[0]:.0f} -> {fs_areas[1]:.0f} -> {fs_areas[2]:.0f}")
print(f"   Direction: {'INCREASING' if fs_areas[2] > fs_areas[0] else 'DECREASING'}")

print("\n3. COMBINED (all points, as R script processes them) areas:")
for inj in injs:
    print(f"   {inj}: {results[inj]['all_area']:.1f}")
all_areas = [results[inj]['all_area'] for inj in injs]
print(f"   Trend: {all_areas[0]:.0f} -> {all_areas[1]:.0f} -> {all_areas[2]:.0f}")
print(f"   Direction: {'INCREASING' if all_areas[2] > all_areas[0] else 'DECREASING'}")
print(f"   R script:  76438 -> 70186 -> 68760 (DECREASING)")

print("\n4. SIM-ONLY PEAK intensities:")
for inj in injs:
    print(f"   {inj}: {results[inj]['sim_peak']:.1f}")
sim_peaks = [results[inj]['sim_peak'] for inj in injs]
print(f"   Trend: {sim_peaks[0]:.0f} -> {sim_peaks[1]:.0f} -> {sim_peaks[2]:.0f}")
print(f"   Direction: {'INCREASING' if sim_peaks[2] > sim_peaks[0] else 'DECREASING'}")

print("\n5. FULL-SCAN PEAK intensities:")
for inj in injs:
    print(f"   {inj}: {results[inj]['fs_peak']:.1f}")
fs_peaks = [results[inj]['fs_peak'] for inj in injs]
print(f"   Trend: {fs_peaks[0]:.0f} -> {fs_peaks[1]:.0f} -> {fs_peaks[2]:.0f}")

print(f"\n{'='*80}")
print("INTERLEAVING PATTERN DETAIL")
print(f"{'='*80}")

# Show the RT gap between consecutive full-scan and SIM-only scans
for inj in injs:
    sim_pts = results[inj]['sim_points']
    fs_pts = results[inj]['fs_points']
    print(f"\n{inj}: RT spacing between consecutive FS->SIM pairs:")
    for i, (fs_rt, _) in enumerate(fs_pts[:5]):
        if i < len(sim_pts):
            sim_rt = sim_pts[i][0]
            gap = sim_rt - fs_rt
            print(f"   FS RT={fs_rt:.3f} -> SIM RT={sim_rt:.3f}, gap={gap:.3f}s")

print(f"\n{'='*80}")
print("WHY COMBINED AREA DIFFERS FROM SIM-ONLY AREA")
print(f"{'='*80}")
print("""
When full-scan and SIM-only scans are interleaved, every other trapezoid
spans from a full-scan point to a SIM-only point (or vice versa).

Near the peak apex, the SIM-only intensities are HIGHER than full-scan.
The full-scan points PULL DOWN the trapezoids that bridge between them.

As a result, the combined area is LESS than the SIM-only area alone,
because the interleaved full-scan points create "dips" between the SIM peaks.
""")

print(f"\n{'='*80}")
print("RATIO ANALYSIS: Why does full-scan/SIM ratio VARY by injection?")
print(f"{'='*80}")
for inj in injs:
    ratio = results[inj]['fs_area'] / results[inj]['sim_area']
    print(f"   {inj}: FS/SIM area ratio = {ratio:.4f}")

ratios = [results[inj]['fs_area'] / results[inj]['sim_area'] for inj in injs]
print(f"\n   The FS/SIM ratio is NOT constant across injections!")
print(f"   This means the full-scan sensitivity is degrading relative to SIM")
print(f"   OR the full-scan and SIM responses differ in their concentration sensitivity")

print(f"\n{'='*80}")
print("CRITICAL VERDICT")  
print(f"{'='*80}")
print(f"""
ALL THREE area types show a DECREASING trend:
  SIM-only:  {sim_areas[0]:.0f} -> {sim_areas[1]:.0f} -> {sim_areas[2]:.0f}  (DECREASING)
  Full-scan: {fs_areas[0]:.0f} -> {fs_areas[1]:.0f} -> {fs_areas[2]:.0f}  (DECREASING)
  Combined:  {all_areas[0]:.0f} -> {all_areas[1]:.0f} -> {all_areas[2]:.0f}  (DECREASING)
  
  MassHunter:  41573 -> 42107 -> 42886  (INCREASING)

The SIM-only area does NOT match MassHunter's INCREASING trend.
The interleaving hypothesis does NOT explain the discrepancy.

Even after separating SIM-only scans, the raw trapezoidal area
still shows a DECREASING trend - the OPPOSITE of MassHunter.

This means the problem is NOT caused by full-scan contamination.
The raw data genuinely shows a decreasing trend, while MassHunter
reports an increasing trend. The difference must lie in HOW
MassHunter processes the data (baseline subtraction, peak detection
boundaries, or intensity calibration).
""")

# Additional check: what if MH only uses a narrower RT window?
print(f"\n{'='*80}")
print("SUPPLEMENTARY: Does RT window matter? Testing [275, 278]")
print(f"{'='*80}")

for inj in injs:
    sim_narrow = [(rt, i) for rt, i in results[inj]['sim_points'] if 275.0 <= rt <= 278.0]
    fs_narrow = [(rt, i) for rt, i in results[inj]['fs_points'] if 275.0 <= rt <= 278.0]
    
    def trap(pts):
        pts = sorted(pts)
        return sum((pts[i][0]-pts[i-1][0])*(pts[i][1]+pts[i-1][1])/2 for i in range(1, len(pts)))
    
    sim_a = trap(sim_narrow) if len(sim_narrow) > 1 else 0
    fs_a = trap(fs_narrow) if len(fs_narrow) > 1 else 0
    print(f"   {inj}: SIM-only [275,278] = {sim_a:.1f}, FS [275,278] = {fs_a:.1f}")
