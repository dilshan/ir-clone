//----------------------------------------------------------------------------------
// STM8S003F3, STM8S003K3 TIMER1 definitions for SDCC.
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

#ifndef STM8S003_TIMER1_DEF_H
#define STM8S003_TIMER1_DEF_H

#define TIM1_CR1_RESET_VALUE        ((unsigned char)0x00)   // CR1 reset value.
#define TIM1_CR2_RESET_VALUE        ((unsigned char)0x00)   // CR2 reset value.
#define TIM1_SMCR_RESET_VALUE       ((unsigned char)0x00)   // SMCR reset value.
#define TIM1_ETR_RESET_VALUE        ((unsigned char)0x00)   // ETR reset value.
#define TIM1_IER_RESET_VALUE        ((unsigned char)0x00)   // IER reset value.
#define TIM1_SR1_RESET_VALUE        ((unsigned char)0x00)   // SR1 reset value.
#define TIM1_SR2_RESET_VALUE        ((unsigned char)0x00)   // SR2 reset value.
#define TIM1_EGR_RESET_VALUE        ((unsigned char)0x00)   // EGR reset value.
#define TIM1_CCMR1_RESET_VALUE      ((unsigned char)0x00)   // CCMR1 reset value.
#define TIM1_CCMR2_RESET_VALUE      ((unsigned char)0x00)   // CCMR2 reset value.
#define TIM1_CCMR3_RESET_VALUE      ((unsigned char)0x00)   // CCMR3 reset value.
#define TIM1_CCMR4_RESET_VALUE      ((unsigned char)0x00)   // CCMR4 reset value.
#define TIM1_CCER1_RESET_VALUE      ((unsigned char)0x00)   // CCER1 reset value.
#define TIM1_CCER2_RESET_VALUE      ((unsigned char)0x00)   // CCER2 reset value.
#define TIM1_CNTRH_RESET_VALUE      ((unsigned char)0x00)   // CNTRH reset value.
#define TIM1_CNTRL_RESET_VALUE      ((unsigned char)0x00)   // CNTRL reset value.
#define TIM1_PSCRH_RESET_VALUE      ((unsigned char)0x00)   // PSCRH reset value.
#define TIM1_PSCRL_RESET_VALUE      ((unsigned char)0x00)   // PSCRL reset value.
#define TIM1_ARRH_RESET_VALUE       ((unsigned char)0xFF)   // ARRH reset value.
#define TIM1_ARRL_RESET_VALUE       ((unsigned char)0xFF)   // ARRL reset value.
#define TIM1_RCR_RESET_VALUE        ((unsigned char)0x00)   // RCR reset value.
#define TIM1_CCR1H_RESET_VALUE      ((unsigned char)0x00)   // CCR1H reset value.
#define TIM1_CCR1L_RESET_VALUE      ((unsigned char)0x00)   // CCR1L reset value.
#define TIM1_CCR2H_RESET_VALUE      ((unsigned char)0x00)   // CCR2H reset value.
#define TIM1_CCR2L_RESET_VALUE      ((unsigned char)0x00)   // CCR2L reset value.
#define TIM1_CCR3H_RESET_VALUE      ((unsigned char)0x00)   // CCR3H reset value.
#define TIM1_CCR3L_RESET_VALUE      ((unsigned char)0x00)   // CCR3L reset value.
#define TIM1_CCR4H_RESET_VALUE      ((unsigned char)0x00)   // CCR4H reset value.
#define TIM1_CCR4L_RESET_VALUE      ((unsigned char)0x00)   // CCR4L reset value.
#define TIM1_BKR_RESET_VALUE        ((unsigned char)0x00)   // BKR reset value.
#define TIM1_DTR_RESET_VALUE        ((unsigned char)0x00)   // DTR reset value.
#define TIM1_OISR_RESET_VALUE       ((unsigned char)0x00)   // OISR reset value.

#define TIM1_CR1_ARPE   ((unsigned char)0x80)  // Auto-Reload Preload Enable mask.
#define TIM1_CR1_CMS    ((unsigned char)0x60)  // Center-aligned Mode Selection mask.
#define TIM1_CR1_DIR    ((unsigned char)0x10)  // Direction mask.
#define TIM1_CR1_OPM    ((unsigned char)0x08)  // One Pulse Mode mask.
#define TIM1_CR1_URS    ((unsigned char)0x04)  // Update Request Source mask.
#define TIM1_CR1_UDIS   ((unsigned char)0x02)  // Update DIsable mask.
#define TIM1_CR1_CEN    ((unsigned char)0x01)  // Counter Enable mask.

#define TIM1_CR2_TI1S   ((unsigned char)0x80)  // TI1S Selection mask.
#define TIM1_CR2_MMS    ((unsigned char)0x70)  // MMS Selection mask.
#define TIM1_CR2_COMS   ((unsigned char)0x04)  // Capture/Compare Control Update Selection mask.
#define TIM1_CR2_CCPC   ((unsigned char)0x01)  // Capture/Compare Preloaded Control mask.

#define TIM1_SMCR_MSM   ((unsigned char)0x80)    // Master/Slave Mode mask.
#define TIM1_SMCR_TS    ((unsigned char)0x70)    // Trigger Selection mask.
#define TIM1_SMCR_SMS   ((unsigned char)0x07)    // Slave Mode Selection mask.

#define TIM1_ETR_ETP    ((unsigned char)0x80)    // External Trigger Polarity mask.
#define TIM1_ETR_ECE    ((unsigned char)0x40)    // External Clock mask.
#define TIM1_ETR_ETPS   ((unsigned char)0x30)    // External Trigger Prescaler mask.
#define TIM1_ETR_ETF    ((unsigned char)0x0F)    // External Trigger Filter mask.

#define TIM1_IER_BIE    ((unsigned char)0x80)   // Break Interrupt Enable mask.
#define TIM1_IER_TIE    ((unsigned char)0x40)   // Trigger Interrupt Enable mask.
#define TIM1_IER_COMIE  ((unsigned char)0x20)   // Commutation Interrupt Enable mask.
#define TIM1_IER_CC4IE  ((unsigned char)0x10)   // Capture/Compare 4 Interrupt Enable mask.
#define TIM1_IER_CC3IE  ((unsigned char)0x08)   // Capture/Compare 3 Interrupt Enable mask.
#define TIM1_IER_CC2IE  ((unsigned char)0x04)   // Capture/Compare 2 Interrupt Enable mask.
#define TIM1_IER_CC1IE  ((unsigned char)0x02)   // Capture/Compare 1 Interrupt Enable mask.
#define TIM1_IER_UIE    ((unsigned char)0x01)   // Update Interrupt Enable mask.

#define TIM1_SR1_BIF    ((unsigned char)0x80)   // Break Interrupt Flag mask.
#define TIM1_SR1_TIF    ((unsigned char)0x40)   // Trigger Interrupt Flag mask.
#define TIM1_SR1_COMIF  ((unsigned char)0x20)   // Commutation Interrupt Flag mask.
#define TIM1_SR1_CC4IF  ((unsigned char)0x10)   // Capture/Compare 4 Interrupt Flag mask.
#define TIM1_SR1_CC3IF  ((unsigned char)0x08)   // Capture/Compare 3 Interrupt Flag mask.
#define TIM1_SR1_CC2IF  ((unsigned char)0x04)   // Capture/Compare 2 Interrupt Flag mask.
#define TIM1_SR1_CC1IF  ((unsigned char)0x02)   // Capture/Compare 1 Interrupt Flag mask.
#define TIM1_SR1_UIF    ((unsigned char)0x01)   // Update Interrupt Flag mask.

#define TIM1_SR2_CC4OF  ((unsigned char)0x10)   // Capture/Compare 4 Overcapture Flag mask.
#define TIM1_SR2_CC3OF  ((unsigned char)0x08)   // Capture/Compare 3 Overcapture Flag mask.
#define TIM1_SR2_CC2OF  ((unsigned char)0x04)   // Capture/Compare 2 Overcapture Flag mask.
#define TIM1_SR2_CC1OF  ((unsigned char)0x02)   // Capture/Compare 1 Overcapture Flag mask.

#define TIM1_EGR_BG     ((unsigned char)0x80)   // Break Generation mask.
#define TIM1_EGR_TG     ((unsigned char)0x40)   // Trigger Generation mask.
#define TIM1_EGR_COMG   ((unsigned char)0x20)   // Capture/Compare Control Update Generation mask.
#define TIM1_EGR_CC4G   ((unsigned char)0x10)   // Capture/Compare 4 Generation mask.
#define TIM1_EGR_CC3G   ((unsigned char)0x08)   // Capture/Compare 3 Generation mask.
#define TIM1_EGR_CC2G   ((unsigned char)0x04)   // Capture/Compare 2 Generation mask.
#define TIM1_EGR_CC1G   ((unsigned char)0x02)   // Capture/Compare 1 Generation mask.
#define TIM1_EGR_UG     ((unsigned char)0x01)   // Update Generation mask.

#define TIM1_CCMR_ICxPSC    ((unsigned char)0x0C)   // Input Capture x Prescaler mask.
#define TIM1_CCMR_ICxF      ((unsigned char)0xF0)   // Input Capture x Filter mask.
#define TIM1_CCMR_OCM       ((unsigned char)0x70)   // Output Compare x Mode mask.
#define TIM1_CCMR_OCxPE     ((unsigned char)0x08)   // Output Compare x Preload Enable mask.
#define TIM1_CCMR_OCxFE     ((unsigned char)0x04)   // Output Compare x Fast Enable mask.
#define TIM1_CCMR_CCxS      ((unsigned char)0x03)   // Capture/Compare x Selection mask.

#define TIM1_CCER1_CC2NP    ((unsigned char)0x80)   // Capture/Compare 2 Complementary output Polarity mask.
#define TIM1_CCER1_CC2NE    ((unsigned char)0x40)   // Capture/Compare 2 Complementary output enable mask.
#define TIM1_CCER1_CC2P     ((unsigned char)0x20)   // Capture/Compare 2 output Polarity mask.
#define TIM1_CCER1_CC2E     ((unsigned char)0x10)   // Capture/Compare 2 output enable mask.
#define TIM1_CCER1_CC1NP    ((unsigned char)0x08)   // Capture/Compare 1 Complementary output Polarity mask.
#define TIM1_CCER1_CC1NE    ((unsigned char)0x04)   // Capture/Compare 1 Complementary output enable mask.
#define TIM1_CCER1_CC1P     ((unsigned char)0x02)   // Capture/Compare 1 output Polarity mask.
#define TIM1_CCER1_CC1E     ((unsigned char)0x01)   // Capture/Compare 1 output enable mask.

#define TIM1_CCER2_CC4P     ((unsigned char)0x20)   // Capture/Compare 4 output Polarity mask.
#define TIM1_CCER2_CC4E     ((unsigned char)0x10)   // Capture/Compare 4 output enable mask.
#define TIM1_CCER2_CC3NP    ((unsigned char)0x08)   // Capture/Compare 3 Complementary output Polarity mask.
#define TIM1_CCER2_CC3NE    ((unsigned char)0x04)   // Capture/Compare 3 Complementary output enable mask.
#define TIM1_CCER2_CC3P     ((unsigned char)0x02)   // Capture/Compare 3 output Polarity mask.
#define TIM1_CCER2_CC3E     ((unsigned char)0x01)   // Capture/Compare 3 output enable mask.

#define TIM1_BKR_MOE    ((unsigned char)0x80)   // Main Output Enable mask.
#define TIM1_BKR_AOE    ((unsigned char)0x40)   // Automatic Output Enable mask.
#define TIM1_BKR_BKP    ((unsigned char)0x20)   // Break Polarity mask.
#define TIM1_BKR_BKE    ((unsigned char)0x10)   // Break Enable mask.
#define TIM1_BKR_OSSR   ((unsigned char)0x08)   // Off-State Selection for Run mode mask.
#define TIM1_BKR_OSSI   ((unsigned char)0x04)   // Off-State Selection for Idle mode mask.
#define TIM1_BKR_LOCK   ((unsigned char)0x03)   // Lock Configuration mask.

#define TIM1_OISR_OIS4      ((unsigned char)0x40)   // Output Idle state 4 (OC4 output) mask.
#define TIM1_OISR_OIS3N     ((unsigned char)0x20)   // Output Idle state 3 (OC3N output) mask.
#define TIM1_OISR_OIS3      ((unsigned char)0x10)   // Output Idle state 3 (OC3 output) mask.
#define TIM1_OISR_OIS2N     ((unsigned char)0x08)   // Output Idle state 2 (OC2N output) mask.
#define TIM1_OISR_OIS2      ((unsigned char)0x04)   // Output Idle state 2 (OC2 output) mask.
#define TIM1_OISR_OIS1N     ((unsigned char)0x02)   // Output Idle state 1 (OC1N output) mask.
#define TIM1_OISR_OIS1      ((unsigned char)0x01)   // Output Idle state 1 (OC1 output) mask.

#endif /* STM8S003_TIMER1_DEF_H */