//----------------------------------------------------------------------------------
// STM8S I2C library for standard mode communication.
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
// Last updated: Dilshan Jayakody [9th Oct 2020]
//
// Update log:
// [09/10/2020] - Initial version - Dilshan Jayakody.
//----------------------------------------------------------------------------------

#include "../include/stm8-i2c.h"

#ifndef F_CPU
#warning "F_CPU not defined, using 2MHz as default CPU clock frequency"
#define F_CPU 2000000UL
#endif

inline void i2cInit()
{
    // CPU clocking frequency.
    I2C_FREQR = F_CPU / 1000000; 

    // I2C frequency. Default (0x000A) is set to 100kHz.
    I2C_CCRH = 0x00;
    I2C_CCRL = 0x50;

    // 7-bit addressing mode.
    I2C_OARH = I2C_OARH_ADDMODE;

    // Enable I2C.
    I2C_CR1 = I2C_CR1_PE;
}

void i2cStart()
{
    I2C_CR2 |= I2C_CR2_START;
    while (!(I2C_SR1 & I2C_SR1_SB));
}

void i2cStop()
{
    I2C_CR2 |= I2C_CR2_STOP;
    while (I2C_SR3 & I2C_SR3_MSL);
}

void i2cWriteAddr(unsigned char addr)
{
    I2C_DR = addr;
    while (!(I2C_SR1 & I2C_SR1_ADDR));

    (void) I2C_SR3;
    I2C_CR2 |= I2C_CR2_ACK;
}

void i2cWrite(unsigned char data)
{
    I2C_DR = data;
    while (!(I2C_SR1 & I2C_SR1_TXE));
}

unsigned char i2cRead(unsigned char ack)
{
    I2C_CR2 = (ack) ? (I2C_CR2 | I2C_CR2_ACK) : (I2C_CR2 & (~(I2C_CR2_ACK)));
    while (!(I2C_SR1 & (I2C_SR1_RXNE)));

    return I2C_DR;
}