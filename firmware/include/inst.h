//-------------------------------------------------------------
// STM8S103F2, STM8S103F3, STM8S103K3 instruction mapping 
// header file for SDCC.
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
// Based on STM8S103 datasheet DocID15441 Rev 14.
// Last updated: Dilshan Jayakody [5th Oct 2020]
//
// Update log:
// [05/10/2020] - Initial version - Dilshan Jayakody.
//------------------------------------------------------------- 

#ifndef STM8S103_INST_H
#define STM8S103_INST_H

// Interrupts related instruction mappings.

#define sei()   {__asm__("rim\n");}     // Enable interrupts.
#define cli()   {__asm__("sim\n");}     // Disable interrupts.
#define wfi()   {__asm__("wfi\n");}     // Wait for interrupt.

// CPU related instruction mappings.

#define nop()   {__asm__("nop\n");}     // No operation.
#define trap()  {__asm__("trap\n");}    // Trap.
#define halt()  {__asm__("halt\n");}    // Halt.

#endif /* STM8S103_INST_H */