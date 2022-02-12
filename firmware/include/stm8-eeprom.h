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

#ifndef HARDWARE_EEPROM_H
#define HARDWARE_EEPROM_H

#include "../include/stm8.h"

void eepromWrite(unsigned short addr, unsigned char value);

static inline unsigned char eepromRead(unsigned short addr)
{
    return GLOBAL_MEM(addr);
}

#endif /* HARDWARE_EEPROM_H */