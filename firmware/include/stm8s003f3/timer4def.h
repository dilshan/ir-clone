//----------------------------------------------------------------------------------
// STM8S003F3, STM8S003K3 TIMER4 definitions for SDCC.
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

#ifndef STM8S003_TIMER4_DEF_H
#define STM8S003_TIMER4_DEF_H

#define TIM4_CR1_RESET_VALUE    ((unsigned char)0x00)   // CR1 reset value. 
#define TIM4_IER_RESET_VALUE    ((unsigned char)0x00)   // IER reset value. 
#define TIM4_SR1_RESET_VALUE    ((unsigned char)0x00)   // SR1 reset value. 
#define TIM4_EGR_RESET_VALUE    ((unsigned char)0x00)   // EGR reset value. 
#define TIM4_CNTR_RESET_VALUE   ((unsigned char)0x00)   // CNTR reset value. 
#define TIM4_PSCR_RESET_VALUE   ((unsigned char)0x00)   // PSCR reset value. 
#define TIM4_ARR_RESET_VALUE    ((unsigned char)0xFF)   // ARR reset value. 

#define TIM4_CR1_ARPE   ((unsigned char)0x80)   // Auto-Reload Preload Enable mask.
#define TIM4_CR1_OPM    ((unsigned char)0x08)   // One Pulse Mode mask.
#define TIM4_CR1_URS    ((unsigned char)0x04)   // Update Request Source mask.
#define TIM4_CR1_UDIS   ((unsigned char)0x02)   // Update DIsable mask.
#define TIM4_CR1_CEN    ((unsigned char)0x01)   // Counter Enable mask.

#define TIM4_IER_UIE    ((unsigned char)0x01)   // Update Interrupt Enable mask.

#define TIM4_SR1_UIF    ((unsigned char)0x01)   // Update Interrupt Flag mask.

#define TIM4_EGR_UG     ((unsigned char)0x01)   // Update Generation mask.

#define TIM4_PSCR_PSC   ((unsigned char)0x07)   // Prescaler Value  mask.

#endif /* STM8S003_TIMER4_DEF_H */