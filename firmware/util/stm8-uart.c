//----------------------------------------------------------------------------------
// STM8S UART library for serial communication.
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
// Last updated: Dilshan Jayakody [2nd Nov 2020]
//
// Update log:
// [02/11/2020] - Initial version - Dilshan Jayakody.
//----------------------------------------------------------------------------------

#include "../include/stm8-uart.h"

void uartInit()
{
    // Set Baud rate with relative to master clock frequency.
    UART1_BRR2 = BAUD_RATE2;
    UART1_BRR1 = BAUD_RATE1;

    UART1_CR2 = UART1_CR2_TEN | UART1_CR2_REN;
}

void uartWrite(unsigned char data)
{
    UART1_DR = data;

    // Wait until data is transfered to host terminal.
    while (!(UART1_SR & UART1_SR_TC));
}

unsigned char uartRead()
{
    // Wait for incoming data from host terminal.
    while (!(UART1_SR & UART1_SR_RXNE));

    return UART1_DR;
}