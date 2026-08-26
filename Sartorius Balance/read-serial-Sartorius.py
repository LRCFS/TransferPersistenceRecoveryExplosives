# Adapted from read-serial-ASTRAExps.py for logging pressure via the
# Sartorius Cubis MSA4202S-000-D0 balance instead of the Arduino force sensor.
# The balance is connected via its USB (Com B) port, which shows up on Windows
# as a virtual COM port once the FTDI VCP driver is installed.
# Python 3.9+
# Run with: python3 "C:\Users\A Bruce - User\Documents\TransferPersistenceRecoveryExplosives\Sartorius Balance\read-serial-Sartorius.py"

# Import required libraries
import serial
import re
from datetime import datetime
import os
import json
import time

# Input experiment name which will be the title of the csv file generated
exp_name = "PILOT_015"

# Input experimental details so they can be added to csv file

swabmount = "45 v1"          # swab mount version used
carriage = "v2"               # carriage version used
PatternFile = "X90Y90 Ratchet"  # pattern file name for swabbing
position = "X10Y10"           # position of swabbing
passes = "1"                  # number of times swabbed
swabbingtime = 8.5            # how long swabbing will take in minutes

# Balance connection settings
balance_port = "COM5"      # virtual COM port assigned to the balance's USB (Com B) interface - check Device Manager
baud = 9600                 # must match the baud rate set on the balance (System Settings > Interfaces > Com B)
parity = serial.PARITY_EVEN  # balance factory default is Even parity - change to match your balance's setting
stopbits = serial.STOPBITS_ONE
bytesize = serial.EIGHTBITS
timebetweenreadings = 100   # how long between readings in milliseconds - MUST match the balance's autoprint interval setting

# Get current date and time
current_year = datetime.now().strftime("%Y")
current_month = datetime.now().strftime("%b")
current_day = datetime.now().strftime("%d")
current_datetime = datetime.now().strftime("%H%M")
str_current_datetime = str(current_datetime)

# Create metadata dictionary
metadata = {
    "experiment_name": exp_name,
    "date": datetime.now().strftime("%Y-%m-%d"),
    "time": str_current_datetime,
    "swab_mount": swabmount,
    "carriage": carriage,
    "PatternFile": PatternFile,
    "position": position,
    "passes": int(passes),
    "swabbing_time_minutes": swabbingtime,
    "balance_port": balance_port,
    "baud_rate": baud,
    "parity": parity,
    "stopbits": stopbits,
    "time_between_readings_ms": timebetweenreadings,
    "expected_samples": int(swabbingtime * 60000 / timebetweenreadings)
}


def save_metadata(metadata_dict, file_path):
    """Save metadata to a JSON file"""
    with open(file_path, 'w') as f:
        json.dump(metadata_dict, f, indent=4)


def parse_weight(raw_line):
    """
    Parse a Sartorius SBI output line, e.g. '   241.32 g' or '+  241.32 g S'.
    Returns (mass_g, force_N, unit) or (None, None, None) if the line can't be parsed
    (e.g. blank lines, error strings like 'Err' during startup/overload).
    """
    match = re.search(r"([+-]?\d+\.?\d*)\s*(kg|g|mg)", raw_line)
    if not match:
        return None, None, None

    value = float(match.group(1))
    unit = match.group(2)

    # Normalise everything to grams
    if unit == "kg":
        mass_g = value * 1000.0
    elif unit == "mg":
        mass_g = value / 1000.0
    else:  # already grams
        mass_g = value

    # Convert to Newtons (F = m * g), using g = 9.81 m/s^2
    force_N = (mass_g / 1000.0) * 9.81

    return mass_g, force_N, unit


# State where to save the csv file created
path = "C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Experimental Results/ASTRA Swabbing/Pilot Study/Pressure Traces"
# Create path folder if it does not already exist
if not os.path.exists(path):
    os.makedirs(path)

fileName = path + "/" + exp_name + ".csv"
metadataFileName = path + "/" + exp_name + "_metadata.json"

# Connect to the balance
ser = serial.Serial(balance_port, baud, parity=parity, stopbits=stopbits, bytesize=bytesize, timeout=1)
print("Connected to balance on port: " + balance_port)

# Give the balance a moment and flush any partial/stale line sitting in the buffer
time.sleep(0.5)
ser.reset_input_buffer()

# Capture start time for elapsed time calculation
start_time = time.time()
start_datetime = datetime.now()

# Update metadata with start time information
metadata["start_datetime"] = start_datetime.strftime("%Y-%m-%d %H:%M:%S.%f")[:-3]
metadata["start_time_unix"] = start_time

# Save metadata to JSON file
save_metadata(metadata, metadataFileName)
print("Metadata saved to: " + metadataFileName)

# Calculate number of samples to collect
samples = int(swabbingtime * 60000 / timebetweenreadings)
print(f"Collecting {samples} samples...")

# Open CSV file and write headers, then collect data
print("Starting data collection...")
with open(fileName, 'w') as file:
    # Write column headers to CSV
    full_headers = "timestamp,elapsed_sec,raw_line,mass_g,force_N,unit"
    file.write(full_headers + "\n")

    line = 0
    while line < samples:
        # Capture current time
        current_time = time.time()
        elapsed_sec = current_time - start_time
        timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S.%f")[:-3]

        # Read data from balance
        getData = str(ser.readline(), 'utf-8', errors='replace')
        raw_line = getData.strip()

        # Skip empty lines (e.g. read timeout with nothing received)
        if not raw_line:
            continue

        mass_g, force_N, unit = parse_weight(raw_line)

        if mass_g is None:
            # Couldn't parse this line (error message, overload, garbled data, etc.)
            # Still log it so nothing is silently lost, but flag it in the terminal.
            print(f"UNPARSED: {raw_line}")
            full_data = f"{timestamp},{elapsed_sec:.3f},{raw_line},,,"
        else:
            full_data = f"{timestamp},{elapsed_sec:.3f},{raw_line},{mass_g:.4f},{force_N:.4f},{unit}"
            print(full_data)

        file.write(full_data + "\n")
        line += 1

print("Data collection complete!")
print("Data saved to: " + fileName)
