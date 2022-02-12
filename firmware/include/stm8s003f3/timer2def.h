//----------------------------------------------------------------------------------
// STM8S003F3, STM8S003K3 TIMER2 definitions for SDCC.
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

#ifndef STM8S003_TIMER2_DEF_H
#define STM8S003_TIMER2_DEF_H

#define TIM2_CR1_RESET_VALUE    ((unsigned char)0x00)   // CR1 reset value.
#define TIM2_IER_RESET_VALUE    ((unsigned char)0x00)   // IER reset value.
#define TIM2_SR1_RESET_VALUE    ((unsigned char)0x00)   // SR1 reset value.
#define TIM2_SR2_RESET_VALUE    ((unsigned char)0x00)   // SR2 reset value.
#define TIM2_EGR_RESET_VALUE    ((unsigned char)0x00)   // EGR reset value.
#define TIM2_CCMR1_RESET_VALUE  ((unsigned char)0x00)   // CCMR1 reset value.
#define TIM2_CCMR2_RESET_VALUE  ((unsigned char)0x00)   // CCMR2 reset value.
#define TIM2_CCMR3_RESET_VALUE  ((unsigned char)0x00)   // CCMR3 reset value.
#define TIM2_CCER1_RESET_VALUE  ((unsigned char)0x00)   // CCER1 reset value.
#define TIM2_CCER2_RESET_VALUE  ((unsigned char)0x00)   // CCER2 reset value.
#define TIM2_CNTRH_RESET_VALUE  ((unsigned char)0x00)   // CNTRH reset value.
#define TIM2_CNTRL_RESET_VALUE  ((unsigned char)0x00)   // CNTRL reset value.
#define TIM2_PSCR_RESET_VALUE   ((unsigned char)0x00)   // PSCR reset value.
#define TIM2_ARRH_RESET_VALUE   ((unsigned char)0xFF)   // ARRH reset value.
#define TIM2_ARRL_RESET_VALUE   ((unsigned char)0xFF)   // ARRL reset value.
#define TIM2_CCR1H_RESET_VALUE  ((unsigned char)0x00)   // CCR1H reset value.
#define TIM2_CCR1L_RESET_VALUE  ((unsigned char)0x00)   // CCR1L reset value.
#define TIM2_CCR2H_RESET_VALUE  ((unsigned char)0x00)   // CCR2H reset value.
#define TIM2_CCR2L_RESET_VALUE  ((unsigned char)0x00)   // CCR2L reset value. 
#define TIM2_CCR3H_RESET_VALUE  ((unsigned char)0x00)   // CCR3H reset value.
#define TIM2_CCR3L_RESET_VALUE  ((unsigned char)0x00)   // CCR3L reset value.

#define TIM2_CR1_ARPE   ((unsigned char)0x80)   // Auto-Reload Preload Enable mask.
#define TIM2_CR1_OPM    ((unsigned char)0x08)   // One Pulse Mode mask.
#define TIM2_CR1_URS    ((unsigned char)0x04)   // Update Request Source mask.
#define TIM2_CR1_UDIS   ((unsigned char)0x02)   // Update DIsable mask.
#define TIM2_CR1_CEN    ((unsigned char)0x01)   // Counter Enable mask.

#define TIM2_IER_CC3IE   ((unsigned char)0x08)  // Capture/Compare 3 Interrupt Enable mask.
#define TIM2_IER_CC2IE   ((unsigned char)0x04)  // Capture/Compare 2 Interrupt Enable mask.
#define TIM2_IER_CC1IE   ((unsigned char)0x02)  // Capture/Compare 1 Interrupt Enable mask.
#define TIM2_IER_UIE     ((unsigned char)0x01)  // Update Interrupt Enable mask.

#define TIM2_SR1_CC3IF   ((unsigned char)0x08)  // Capture/Compare 3 Interrupt Flag mask.
#define TIM2_SR1_CC2IF   ((unsigned char)0x04)  // Capture/Compare 2 Interrupt Flag mask.
#define TIM2_SR1_CC1IF   ((unsigned char)0x02)  // Capture/Compare 1 Interrupt Flag mask.
#define TIM2_SR1_UIF     ((unsigned char)0x01)  // Update Interrupt Flag mask.

#define TIM2_SR2_CC3OF   ((unsigned char)0x08)  // Capture/Compare 3 Overcapture Flag mask.
#define TIM2_SR2_CC2OF   ((unsigned char)0x04)  // Capture/Compare 2 Overcapture Flag mask.
#define TIM2_SR2_CC1OF   ((unsigned char)0x02)  // Capture/Compare 1 Overcapture Flag mask.

#define TIM2_EGR_CC3G    ((unsigned char)0x08)  // Capture/Compare 3 Generation mask.
#define TIM2_EGR_CC2G    ((unsigned char)0x04)  // Capture/Compare 2 Generation mask.
#define TIM2_EGR_CC1G    ((unsigned char)0x02)  // Capture/Compare 1 Generation mask.
#define TIM2_EGR_UG      ((unsigned char)0x01)  // Update Generation mask.

#define TIM2_CCMR_ICxPSC ((unsigned char)0x0C)  // Input Capture x Prescaler mask.
#define TIM2_CCMR_ICxF   ((unsigned char)0xF0)  // Input Capture x Filter mask. 
#define TIM2_CCMR_OCM    ((unsigned char)0x70)  // Output Compare x Mode mask. 
#define TIM2_CCMR_OCxPE  ((unsigned char)0x08)  // Output Compare x Preload Enable mask. 
#define TIM2_CCMR_CCxS   ((unsigned char)0x03)  // Capture/Compare x Selection mask. 

#define TIM2_CCER1_CC2P  ((unsigned char)0x20)  // Capture/Compare 2 output Polarity mask.
#define TIM2_CCER1_CC2E  ((unsigned char)0x10)  // Capture/Compare 2 output enable mask.
#define TIM2_CCER1_CC1P  ((unsigned char)0x02)  // Capture/Compare 1 output Polarity mask.
#define TIM2_CCER1_CC1E  ((unsigned char)0x01)  // Capture/Compare 1 output enable mask.

#define TIM2_CCER2_CC3P  ((unsigned char)0x02)  // Capture/Compare 3 output Polarity mask.
#define TIM2_CCER2_CC3E  ((unsigned char)0x01)  // Capture/Compare 3 output enable mask.

#endif /* STM8S003_TIMER2_DEF_H */