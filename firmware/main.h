//----------------------------------------------------------------------------------
// IR Remote Cloner firmware for STM8S003F3P6.
// Main source file.
//
// Copyright (c) 2022 Dilshan R Jayakody
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//  
// Last updated: Dilshan Jayakody [11th Dec 2020]
//
// Update log:
// [11/12/2020] - Initial version - Dilshan Jayakody.
//----------------------------------------------------------------------------------

#ifndef IR_CLONER_MAIN
#define IR_CLONER_MAIN

void initSystem();
void startCapture(unsigned char buttonID);
void startTransmit(unsigned char buttonID);
unsigned char decodeKeyMatrix(unsigned char *row, unsigned char *col);

// Size of the data capture buffer.
#define CAPTURE_BUFFER_SIZE 120

// CAPTURE_DELAY = TRANSMIT_DELAY - 3 (additional 3 cycles are for 
// buffer overflow check instructions in capture loop).
#define CAPTURE_DELAY       78    
#define TRANSMIT_DELAY      81    

#define TIMEOUT_LIMIT       48000

// Idle time-out in seconds to enter device into sleep mode.
#define IDLE_TIMEOUT        10

#endif /* IR_CLONER_MAIN */