//----------------------------------------------------------------------------------
// IR Remote Cloner firmware for STM8S003F3P6.
// 24C series EEPROM driver (16-bit addressing).
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

#include "24c-eeprom.h"
#include "include/stm8-i2c.h"

void initExtEEPROM()
{
    i2cInit();
}

void writeExtEEPROM(unsigned short addr, unsigned char data)
{
    // Initialize EEPROM to perform write operation.
    i2cStart();
    i2cWriteAddr(EEPROM_ADDRESS);

    // Send write address.
    i2cWrite((addr >> 8) & 0xFF);
    i2cWrite(addr & 0xFF);

    // Write data into specified memory address.
    i2cWrite(data);
    i2cStop();
}

unsigned char readExtEEPROM(unsigned short addr)
{
    unsigned char tmpData;
    
    // Initialize EEPROM to perform read operation.
    i2cStart();
    i2cWriteAddr(EEPROM_ADDRESS);

    // Send read address.
    i2cWrite((addr >> 8) & 0xFF);
    i2cWrite(addr & 0xFF);
    i2cStop();

    // Get data from the specified memory address.
    i2cStart();
    i2cWriteAddr(EEPROM_ADDRESS | 0x01);
    tmpData = i2cRead(0);
    i2cStop();

    return tmpData;
}