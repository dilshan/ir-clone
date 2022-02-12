//----------------------------------------------------------------------------------
// STM8S003F3, STM8S003K3 UART1 definitions for SDCC.
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
// Based on STM8S003 datasheet DS7147 Rev 10.
// Last updated: Dilshan Jayakody [4th Dec 2020]
//
// Update log:
// [04/12/2020] - Initial version - Dilshan Jayakody.
//----------------------------------------------------------------------------------

#ifndef STM8S003_UART1_DEF_H
#define STM8S003_UART1_DEF_H

#define UART1_SR_RESET_VALUE    ((unsigned char)0xC0)   // SR reset value.
#define UART1_BRR1_RESET_VALUE  ((unsigned char)0x00)   // BRR1 reset value.
#define UART1_BRR2_RESET_VALUE  ((unsigned char)0x00)   // BRR2 reset value.
#define UART1_CR1_RESET_VALUE   ((unsigned char)0x00)   // CR1 reset value.
#define UART1_CR2_RESET_VALUE   ((unsigned char)0x00)   // CR2 reset value.
#define UART1_CR3_RESET_VALUE   ((unsigned char)0x00)   // CR3 reset value.
#define UART1_CR4_RESET_VALUE   ((unsigned char)0x00)   // CR4 reset value.
#define UART1_CR5_RESET_VALUE   ((unsigned char)0x00)   // CR5 reset value.
#define UART1_GTR_RESET_VALUE   ((unsigned char)0x00)   // GTR reset value.
#define UART1_PSCR_RESET_VALUE  ((unsigned char)0x00)   // PSCR reset value.

#define UART1_SR_TXE    ((unsigned char)0x80)   // Transmit Data Register Empty mask.
#define UART1_SR_TC     ((unsigned char)0x40)   // Transmission Complete mask.
#define UART1_SR_RXNE   ((unsigned char)0x20)   // Read Data Register Not Empty mask.
#define UART1_SR_IDLE   ((unsigned char)0x10)   // IDLE line detected mask.
#define UART1_SR_OR     ((unsigned char)0x08)   // OverRun error mask.
#define UART1_SR_NF     ((unsigned char)0x04)   // Noise Flag mask.
#define UART1_SR_FE     ((unsigned char)0x02)   // Framing Error mask.
#define UART1_SR_PE     ((unsigned char)0x01)   // Parity Error mask.

#define UART1_CR1_R8        ((unsigned char)0x80)   // Receive Data bit 8.
#define UART1_CR1_T8        ((unsigned char)0x40)   // Transmit data bit 8.
#define UART1_CR1_UARTD     ((unsigned char)0x20)   // UART1 Disable (for low power consumption).
#define UART1_CR1_M         ((unsigned char)0x10)   // Word length mask.
#define UART1_CR1_WAKE      ((unsigned char)0x08)   // Wake-up method mask.
#define UART1_CR1_PCEN      ((unsigned char)0x04)   // Parity Control Enable mask.
#define UART1_CR1_PS        ((unsigned char)0x02)   // UART1 Parity Selection.
#define UART1_CR1_PIEN      ((unsigned char)0x01)   // UART1 Parity Interrupt Enable mask.

#define UART1_CR2_TIEN      ((unsigned char)0x80)   // Transmitter Interrupt Enable mask.
#define UART1_CR2_TCIEN     ((unsigned char)0x40)   // Transmission Complete Interrupt Enable mask.
#define UART1_CR2_RIEN      ((unsigned char)0x20)   // Receiver Interrupt Enable mask.
#define UART1_CR2_ILIEN     ((unsigned char)0x10)   // IDLE Line Interrupt Enable mask.
#define UART1_CR2_TEN       ((unsigned char)0x08)   // Transmitter Enable mask.
#define UART1_CR2_REN       ((unsigned char)0x04)   // Receiver Enable mask.
#define UART1_CR2_RWU       ((unsigned char)0x02)   // Receiver Wake-Up mask.
#define UART1_CR2_SBK       ((unsigned char)0x01)   // Send Break mask.

#define UART1_CR3_LINEN     ((unsigned char)0x40)   // Alternate Function output mask.
#define UART1_CR3_STOP      ((unsigned char)0x30)   // STOP bits [1:0] mask.
#define UART1_CR3_CKEN      ((unsigned char)0x08)   // Clock Enable mask.
#define UART1_CR3_CPOL      ((unsigned char)0x04)   // Clock Polarity mask.
#define UART1_CR3_CPHA      ((unsigned char)0x02)   // Clock Phase mask.
#define UART1_CR3_LBCL      ((unsigned char)0x01)   // Last Bit Clock pulse mask.

#define UART1_CR4_LBDIEN    ((unsigned char)0x40)   // LIN Break Detection Interrupt Enable mask.
#define UART1_CR4_LBDL      ((unsigned char)0x20)   // LIN Break Detection Length mask.
#define UART1_CR4_LBDF      ((unsigned char)0x10)   // LIN Break Detection Flag mask.
#define UART1_CR4_ADD       ((unsigned char)0x0F)   // Address of the UART1 node mask.

#define UART1_CR5_SCEN      ((unsigned char)0x20)   // Smart Card Enable mask.
#define UART1_CR5_NACK      ((unsigned char)0x10)   // Smart Card Nack Enable mask.
#define UART1_CR5_HDSEL     ((unsigned char)0x08)   // Half-Duplex Selection mask.
#define UART1_CR5_IRLP      ((unsigned char)0x04)   // Irda Low Power Selection mask.
#define UART1_CR5_IREN      ((unsigned char)0x02)   // Irda Enable mask.

#endif /* STM8S003_UART1_DEF_H */