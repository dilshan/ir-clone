//----------------------------------------------------------------------------------
// STM8S EEPROM library for standard mode communication.
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
// Last updated: Dilshan Jayakody [24th Oct 2020]
//
// Update log:
// [24/10/2020] - Initial version - Dilshan Jayakody.
//----------------------------------------------------------------------------------

#include "../include/stm8-eeprom.h"

void eepromWrite(unsigned short addr, unsigned char value)
{
    // Unlock EEPROM.
    FLASH_DUKR = FLASH_DUKR_KEY1;
    FLASH_DUKR = FLASH_DUKR_KEY2;
    while (!(FLASH_IAPSR & FLASH_IAPSR_DUL));

    // Writing data into specified address.
    GLOBAL_MEM(addr) = value;
    while (!(FLASH_IAPSR & FLASH_IAPSR_EOP));

    // Lock EEPROM.
    FLASH_IAPSR &= ~(FLASH_IAPSR_DUL);
}