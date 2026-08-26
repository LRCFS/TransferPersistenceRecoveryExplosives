# Sartorius Balance — Swab Pressure Logging (ASTRA Swabbing Project)

## Goal
Use a lab balance to measure/log how much force ("pressure") is applied when
swabbing a surface, as part of the ASTRA Swabbing pilot study. Balance
available: **Sartorius Cubis MSA4202S-000-D0** (S/N 27501168).

## The balance
- Sartorius Cubis series, top-loading **precision balance** (not a
  microbalance/mass comparator).
- Capacity: **4200 g** | Readability: **0.01 g (10 mg)**
- Pan: ~8.1" x 8.1", stainless steel
- Response/settling time: ~1-2 s (digitally filtered)

## Measurement principle
A balance measures mass, but pressing straight down on it is equivalent to
applying a downward force. Convert the displayed mass to force with:

```
Force (N) = mass (kg) x 9.81
```

For an approximate pressure, divide force by the swab tip's contact area
(measure tip diameter with calipers) — this is only approximate since
foam/cotton tips deform under load.

### Suggested physical setup
1. Tape/place a rigid flat coupon (glass, stainless, or actual surface
   material) on the balance pan — don't swab the bare pan directly.
2. Tare the balance with the coupon in place.
3. Press the swab straight down onto the coupon, hold briefly, read the
   stabilized value, convert to Newtons, compare to target force.
4. Repeat until the operator can reliably reproduce the target force "by
   feel," then perform the real swab.

### Limitations
- Measures **static press force**, not a fast dynamic swipe — the balance's
  filtering/settling time (~1-2 s) smooths out rapid motion.
- Only captures the **vertical** component of force, not lateral/shear force
  from the swiping motion.
- Don't drop/jab the swab onto the pan — shock loading can affect a precision
  load cell over time. Load and unload gently, mostly vertical motion.
- For true dynamic force logging during an actual swipe, a dedicated
  high-sample-rate force gauge/load cell would be better than this balance.

## Interfaces on the balance
Cubis MSA balances expose (at minimum):
- **Com A** — 25-pin RS-232 "peripheral port" (what was found on the back of
  this unit)
- **Com B** — built-in USB port for PC connection (pre-configured at the
  factory to speak SBI protocol) — **use this one, it's simpler and safer**
- Com C (optional slot), Com D (Ethernet) — not relevant here

### Com A: 25-pin RS-232 pinout (reference only — Com B/USB is recommended instead)
| Pin | Signal | Pin | Signal |
|---|---|---|---|
| 1 | Signal ground | 14 | Internal ground (GND) |
| 2 | TxD (data out) | 15-19 | Control I/O 1-5 |
| 3 | RxD (data in) | 20 | DTR |
| 4 | Signal GND | 21-24 | Not used |
| 5 | CTS | 25 | +5V output |
| 6 | **Not used** | | |
| 7-8 | Internal GND | | |
| 11 | +12V output | | |
| 13 | +5V output | | |

**Warning (from the Sartorius manual):** generic pre-wired RS-232 cables
often have incorrect pin assignments for this connector and can damage the
balance/PC. Disconnect any lines assigned differently, especially pin 6.
If wiring a cable to a PC/9-pin adapter, only these pins should be connected:
Balance pin 2→PC pin 2 (TxD), 3→3 (RxD), 5→4 (CTS), 20→8 (DTR), 4/7→6 (GND),
14→5 (GND). Everything else left disconnected.

### Com B: USB connection (recommended path)
- Connector on the balance is **USB Type-B** (same as a standard USB
  printer cable — USB-A to USB-B).
- The USB port creates a **virtual COM port** via an **FTDI chip**.
- Driver needed: **FTDI VCP driver**, free from `ftdichip.com/FTDrivers.htm`.
- After installing the driver and connecting, check **Device Manager → Ports
  (COM & LPT)** for the new COM port (e.g. `COM5`).
- Recommended tweak: Device Manager → the new COM port → Properties → Port
  Settings → Advanced → set **Latency Timer to 1 ms** for faster response.

### Balance-side settings needed for (semi-)continuous logging
In the balance's System Settings / Task menu for the interface (Com A or
Com B):
- Protocol: **SBI**
- Output: **Automatic printout**
- Criterion: **Without stability** (sends every cycle, not just once stable)
- Interval: as fast as **0.1 s** (10 readings/sec), up to 10 s, or by number
  of measurement cycles (1, 2, 5, 10 ... 100)

Serial settings (must match between balance and PC/script):
- Baud: factory default **9600** (600–19200 selectable)
- Data bits: **8**
- Parity: factory default **Even** (Even/Odd/None selectable)
- Stop bits: factory default **1** (1 or 2 selectable)

## Logging script: `read-serial-Sartorius.py`
Adapted from the existing ASTRA Arduino logging script
(`read-serial-ASTRAExps.py`) to read from the balance's virtual COM port
instead of an Arduino.

Before running:
1. Set `balance_port` to the COM port assigned to the balance (Device
   Manager).
2. Set `baud`, `parity`, `stopbits` to match what's configured on the
   balance's Com B interface.
3. Set `timebetweenreadings` (ms) to match the balance's autoprint interval.
4. Update the experiment metadata fields (`exp_name`, `swabmount`,
   `carriage`, `PatternFile`, `position`, `passes`, `swabbingtime`) as
   needed per run.
5. `pip install pyserial` if not already installed.

What it does:
- Connects to the balance's virtual COM port.
- Reads lines continuously (SBI ASCII output, e.g. `"   241.32 g"`).
- Parses each line for a numeric value + unit (`g`/`kg`/`mg`), normalizes to
  grams, and converts to Newtons (`F = m x 9.81`).
- Writes a CSV with columns: `timestamp, elapsed_sec, raw_line, mass_g,
  force_N, unit`.
- Saves a companion `_metadata.json` file with all the experiment settings
  and start time.
- Unparseable lines (errors, overload, garbled data) are still logged (with
  blank mass/force columns) and flagged in the terminal as `UNPARSED:` so
  nothing silently disappears.

Output CSV location (matches existing ASTRA pipeline):
```
C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/
Experimental Results/ASTRA Swabbing/Pilot Study/Pressure Traces/<exp_name>.csv
```

**Not yet verified:** the exact text format of the balance's SBI output
line depends on the "Standard 1/2/3" printout format chosen in the task
settings. Run a short test first and check the `raw_line` column / any
`UNPARSED:` terminal output to confirm the parser is correctly picking up
the value before trusting a full run.

## Reference material
- Sartorius Cubis MSA User Manual (source of the pinout/interface details
  above): downloaded from
  `https://www.dataweigh.com/media/17992/man-cubis_msa_user_manual-e.pdf`
  — see pages ~134-139 (Interfaces chapter) for RS-232/USB details, and
  pages ~61-63 for the automatic-printout/interval settings.
