"""
Investigate whether SIM CSV files contain interleaved full-scan and SIM-only data.
Classification: if Mass==42 co-occurs at the same RetentionTime, it's a full-scan row.
If Mass==42 does NOT co-occur, it's SIM-only.
"""
import csv
import sys
from collections import defaultdict

files = {
    "017": r"C:\Users\A Bruce - User\Documents\TransferPersistenceRecoveryExplosives\GCMSQuantitation\20260309-QC Test\GcDataConvertedRcode\SIM\017.csv",
    "023": r"C:\Users\A Bruce - User\Documents\TransferPersistenceRecoveryExplosives\GCMSQuantitation\20260309-QC Test\GcDataConvertedRcode\SIM\023.csv",
    "029": r"C:\Users\A Bruce - User\Documents\TransferPersistenceRecoveryExplosives\GCMSQuantitation\20260309-QC Test\GcDataConvertedRcode\SIM\029.csv",
}

RT_MIN, RT_MAX = 274.0, 279.0

for inj_name, fpath in files.items():
    print(f"\n{'='*80}")
    print(f"INJECTION {inj_name}")
    print(f"{'='*80}")
    
    # Read all data
    rows = []
    with open(fpath, 'r') as f:
        reader = csv.DictReader(f)
        for r in reader:
            rt = float(r['RetentionTime'])
            mass = float(r['Mass'])
            intensity = float(r['Intensity'])
            rows.append((rt, mass, intensity))
    
    # Group by RT
    rt_groups = defaultdict(dict)
    for rt, mass, intensity in rows:
        rt_groups[rt][mass] = intensity
    
    # Filter to RT [274, 279] and Mass==46
    mass46_in_range = []
    for rt, mass, intensity in rows:
        if RT_MIN <= rt <= RT_MAX and mass == 46.0:
            mass46_in_range.append((rt, intensity))
    
    # Classify each Mass==46 row
    sim_only_points = []
    fullscan_points = []
    
    print(f"\nAll Mass==46 rows in RT [{RT_MIN}, {RT_MAX}]:")
    print(f"{'RT':>10s}  {'Intensity':>12s}  {'Type':>10s}  {'Masses at this RT'}")
    print(f"{'-'*10}  {'-'*12}  {'-'*10}  {'-'*30}")
    
    for rt, intensity in sorted(mass46_in_range):
        masses_at_rt = sorted(rt_groups[rt].keys())
        has_mass42 = 42.0 in rt_groups[rt]
        scan_type = "full-scan" if has_mass42 else "SIM-only"
        
        if has_mass42:
            fullscan_points.append((rt, intensity))
        else:
            sim_only_points.append((rt, intensity))
        
        # Show which masses are present
        mass_str = ", ".join([f"{int(m)}" for m in masses_at_rt])
        print(f"{rt:10.3f}  {intensity:12.1f}  {scan_type:>10s}  [{mass_str}]")
    
    print(f"\nTotal Mass==46 rows: {len(mass46_in_range)}")
    print(f"  SIM-only:   {len(sim_only_points)}")
    print(f"  Full-scan:  {len(fullscan_points)}")
    
    # Find peak apex for SIM-only
    if sim_only_points:
        sim_apex_rt, sim_apex_int = max(sim_only_points, key=lambda x: x[1])
        print(f"\nSIM-only peak apex: RT={sim_apex_rt:.3f}, Intensity={sim_apex_int:.1f}")
        
        # Find nearest full-scan point
        if fullscan_points:
            nearest_fs = min(fullscan_points, key=lambda x: abs(x[0] - sim_apex_rt))
            print(f"Nearest full-scan:  RT={nearest_fs[0]:.3f}, Intensity={nearest_fs[1]:.1f}")
            print(f"Full/SIM ratio at apex: {nearest_fs[1] / sim_apex_int:.4f}")
    
    # Trapezoidal area - SIM-only
    sim_sorted = sorted(sim_only_points, key=lambda x: x[0])
    sim_area = 0.0
    for i in range(1, len(sim_sorted)):
        dt = sim_sorted[i][0] - sim_sorted[i-1][0]
        avg_int = (sim_sorted[i][1] + sim_sorted[i-1][1]) / 2.0
        sim_area += dt * avg_int
    
    # Trapezoidal area - full-scan only
    fs_sorted = sorted(fullscan_points, key=lambda x: x[0])
    fs_area = 0.0
    for i in range(1, len(fs_sorted)):
        dt = fs_sorted[i][0] - fs_sorted[i-1][0]
        avg_int = (fs_sorted[i][1] + fs_sorted[i-1][1]) / 2.0
        fs_area += dt * avg_int
    
    # Trapezoidal area - ALL points combined (simulating what R script does)
    all_sorted = sorted(mass46_in_range, key=lambda x: x[0])
    all_area = 0.0
    for i in range(1, len(all_sorted)):
        dt = all_sorted[i][0] - all_sorted[i-1][0]
        avg_int = (all_sorted[i][1] + all_sorted[i-1][1]) / 2.0
        all_area += dt * avg_int
    
    print(f"\nTrapezoidal areas in RT [{RT_MIN}, {RT_MAX}]:")
    print(f"  SIM-only area:   {sim_area:.1f}")
    print(f"  Full-scan area:  {fs_area:.1f}")
    print(f"  Combined area:   {all_area:.1f}")
    if sim_area > 0:
        print(f"  Full/SIM ratio:  {fs_area / sim_area:.4f}")

# Summary table
print(f"\n\n{'='*80}")
print("SUMMARY TABLE")
print(f"{'='*80}")
print(f"\n{'Injection':>10s} | {'SIM peak':>12s} | {'SIM area':>12s} | {'FS peak':>12s} | {'FS area':>12s} | {'Combined':>12s} | {'FS/SIM ratio':>12s}")
print(f"{'-'*10}-+-{'-'*12}-+-{'-'*12}-+-{'-'*12}-+-{'-'*12}-+-{'-'*12}-+-{'-'*12}")

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
    
    sim_only_points = []
    fullscan_points = []
    for rt, intensity in mass46_in_range:
        if 42.0 in rt_groups[rt]:
            fullscan_points.append((rt, intensity))
        else:
            sim_only_points.append((rt, intensity))
    
    sim_apex = max(sim_only_points, key=lambda x: x[1])[1] if sim_only_points else 0
    fs_apex = max(fullscan_points, key=lambda x: x[1])[1] if fullscan_points else 0
    
    sim_sorted = sorted(sim_only_points)
    sim_area = sum((sim_sorted[i][0]-sim_sorted[i-1][0])*(sim_sorted[i][1]+sim_sorted[i-1][1])/2 for i in range(1, len(sim_sorted)))
    
    fs_sorted = sorted(fullscan_points)
    fs_area = sum((fs_sorted[i][0]-fs_sorted[i-1][0])*(fs_sorted[i][1]+fs_sorted[i-1][1])/2 for i in range(1, len(fs_sorted)))
    
    all_sorted = sorted(mass46_in_range)
    all_area = sum((all_sorted[i][0]-all_sorted[i-1][0])*(all_sorted[i][1]+all_sorted[i-1][1])/2 for i in range(1, len(all_sorted)))
    
    ratio = fs_area / sim_area if sim_area > 0 else 0
    
    print(f"{inj_name:>10s} | {sim_apex:12.1f} | {sim_area:12.1f} | {fs_apex:12.1f} | {fs_area:12.1f} | {all_area:12.1f} | {ratio:12.4f}")

print(f"\nMassHunter reference areas: 017=41573, 023=42107, 029=42886")
print(f"R script combined areas (from prior analysis): 017=76438, 023=70186, 029=68760")

print(f"\n\n{'='*80}")
print("TREND ANALYSIS")
print(f"{'='*80}")
