# nome e cognome:
# matricola:
#
# path: $HOME/thermal-monitor/app.py

import argparse
import os
import sys
import time
import psutil
import datetime

def main():
    parser = argparse.ArgumentParser(description="thermal monitor")
    parser.add_argument("--warning-temp", type=int, required=True, help="temperatura di avvertimento in gradi Celsius")
    parser.add_argument("--critical-temp", type=int, required=True, help="temperatura critica in gradi Celsius")
    parser.add_argument("--interval", type=int, required=True, help="intervallo di controllo in secondi")
    args = parser.parse_args()

    if not args.critical_temp > args.warning_tem:
        print(f"error: warning-temp ({args.warning_temp}) deve essere minore di critical-temp ({args.critical_temp})")
        sys.exit(1)

    if not args.interval > 0:
        print(f"error: interval ({args.interval}) deve essere un intero positivo (> 0)")
        sys.exit(1)

    log_dir = "~/thermal-monitor"
    os.makedirs(log_dir, exist_ok=True)
    log_path = os.path.join(log_dir, "thermal.log")

    print(f"Monitoraggio termico avviato. Warning: {args.warning_temp}°C, Critical: {args.critical_temp}°C")

    while True:
        temperatures = psutil.sensors_temperatures()

        for sensor_name, sensor_list in temperatures.items():
            for sensor in sensor_list:
                temp = sensor.current()
                timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")

                if temp >= args.critical_temp:
                    message = f"[{timestamp}] CRITICAL: {sensor_name} - {temp:.1f}°C\n"
                    with open(log_path, 'a') as f:
                        f.write(message)
                    print(f"CRITICAL: {sensor_name} - {temp:.1f}°C")
                                
                elif temp >= args.warning_temp:
                    message = f"[{timestamp}] WARNING: {sensor_name} - {temp:.1f}°C\n"
                    with open(log_path, 'a') as f:
                        f.write(message)
                    print(f"WARNING: {sensor_name} - {temp:.1f}°C")

        time.sleep(args.interval)

if __name__ == "__main__":

    main()