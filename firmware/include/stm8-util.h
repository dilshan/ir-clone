//----------------------------------------------------------------------------------
// STM8S common utility functions.
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
// Last updated: Dilshan Jayakody [12th Oct 2020]
//
// Update log:
// [07/10/2020] - Initial version - Dilshan Jayakody.
// [12/10/2020] - Add inline delay option - Dilshan Jayakody.
//----------------------------------------------------------------------------------

#ifndef STM8S103_COMMON_UTIL_H
#define STM8S103_COMMON_UTIL_H

#include "../include/stm8.h"

#ifndef F_CPU
#warning "F_CPU not defined, using 2MHz as default CPU clock frequency"
#define F_CPU 2000000UL
#endif

static inline unsigned char bcdToDec(unsigned char bcd)
{
    return (bcd >> 4) * 10 + (bcd & 0x0F);
}

static inline unsigned char decToBCD(unsigned char dec)
{
    return ((dec / 10) << 4) + (dec % 10);
}

#ifdef INLINE_DELAY

static inline void delay_ms(unsigned long ms)
{
    unsigned long countdown;

    for (countdown = 0; countdown < ((F_CPU / 18 / 1000UL) * ms); countdown++)
    {
        nop();
    }
}

static inline void delay_cycle(unsigned short cycle)
{
    while(cycle)
    {
        cycle--;
    }
}

#else

void delay_ms(unsigned int ms);
void delay_cycle(unsigned short cycle);

#endif

#endif /* STM8S103_COMMON_UTIL_H */