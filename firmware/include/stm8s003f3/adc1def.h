//----------------------------------------------------------------------------------
// STM8S003F3, STM8S003K3 ADC definitions for SDCC.
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

#ifndef STM8S003_ADC1_DEF_H
#define STM8S003_ADC1_DEF_H

#define  ADC1_CSR_RESET_VALUE       ((unsigned char)0x00)   // CSR reset value.
#define  ADC1_CR1_RESET_VALUE       ((unsigned char)0x00)   // CR1 reset value.
#define  ADC1_CR2_RESET_VALUE       ((unsigned char)0x00)   // CR2 reset value.
#define  ADC1_CR3_RESET_VALUE       ((unsigned char)0x00)   // CR3 reset value.
#define  ADC1_TDRL_RESET_VALUE      ((unsigned char)0x00)   // TDRL reset value.
#define  ADC1_TDRH_RESET_VALUE      ((unsigned char)0x00)   // TDRH reset value.
#define  ADC1_HTRL_RESET_VALUE      ((unsigned char)0x03)   // HTRL reset value.
#define  ADC1_HTRH_RESET_VALUE      ((unsigned char)0xFF)   // HTRH reset value.
#define  ADC1_LTRH_RESET_VALUE      ((unsigned char)0x00)   // LTRH reset value.
#define  ADC1_LTRL_RESET_VALUE      ((unsigned char)0x00)   // LTRL reset value.
#define  ADC1_AWCRH_RESET_VALUE     ((unsigned char)0x00)   // AWCRH reset value.
#define  ADC1_AWCRL_RESET_VALUE     ((unsigned char)0x00)   // AWCRL reset value.

#define ADC1_CSR_EOC        ((unsigned char)0x80)   // End of Conversion mask.
#define ADC1_CSR_AWD        ((unsigned char)0x40)   // Analog Watch Dog Status mask.
#define ADC1_CSR_EOCIE      ((unsigned char)0x20)   // Interrupt Enable for EOC mask.
#define ADC1_CSR_AWDIE      ((unsigned char)0x10)   // Analog Watchdog interrupt enable mask.
#define ADC1_CSR_CH         ((unsigned char)0x0F)   // Channel selection bits mask.

#define ADC1_CR1_SPSEL      ((unsigned char)0x70)   // Prescaler selection mask.
#define ADC1_CR1_CONT       ((unsigned char)0x02)   // Continuous conversion mask.
#define ADC1_CR1_ADON       ((unsigned char)0x01)   // A/D Converter on/off mask.
 
#define ADC1_CR2_EXTTRIG    ((unsigned char)0x40)   // External trigger enable mask.
#define ADC1_CR2_EXTSEL     ((unsigned char)0x30)   // External event selection mask.
#define ADC1_CR2_ALIGN      ((unsigned char)0x08)   // Data Alignment mask.
#define ADC1_CR2_SCAN       ((unsigned char)0x02)   // Scan mode mask.
 
#define ADC1_CR3_DBUF       ((unsigned char)0x80)   // Data Buffer Enable mask.
#define ADC1_CR3_OVR        ((unsigned char)0x40)   // Overrun Status Flag mask.

#endif /* STM8S003_ADC1_DEF_H */