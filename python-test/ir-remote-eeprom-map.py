# Simple Python 3 script to plot waveform profile[0] from the specified EEPROM dump.

# Usage: python ir-remote-eeprom-map.py [bin-file]
# Example: python ir-remote-eeprom-map.py test.bin

import numpy as np
import matplotlib.pyplot as plot
import sys

filename = ""
data_unit_len = 10

if len(sys.argv) < 2:
	print("Usage: ir-remote-eeprom-map.py [BIN-FILE]")
	exit()
else:
	filename = sys.argv[1]

with open(filename, "rb") as binfile:
	rec_size = ord(binfile.read(1))
	
	print("Number of records: " + str(rec_size))
	
	time_data = []
	filepos = 0
	
	# Read 16-bit data set of the first waveform.
	while(filepos < rec_size):
		temp_data = ord(binfile.read(1)) << 8
		temp_data = temp_data | ord(binfile.read(1)) 
		
		filepos = filepos + 1
		print(hex(temp_data), end =" ")
		
		time_data.append(temp_data)
		
	print("")
	print("File reading is completed")
	
if len(time_data) > 0 :
	plot_data = [0]
	is_zero = 0
	
	recpos = 0
	while(recpos < len(time_data)):
		curr_data = time_data[recpos]
		
		# Generating square waveform.
		while(curr_data > 0):
			plot_data.append(is_zero)
			curr_data = curr_data - data_unit_len
			
		recpos = recpos + 1

		# Determine the next logic level.
		if is_zero == 0:
			is_zero = 1
		else:
			is_zero = 0
			
xs = np.repeat(range(len(plot_data)), 2)
ys = np.repeat(plot_data, 2)
xs = xs[1:]
ys = ys[:-1]

plot.plot(xs, ys)
plot.ylim(-0.5, 1.5)
plot.show()