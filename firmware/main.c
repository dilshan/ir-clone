//----------------------------------------------------------------------------------
// IR Remote Cloner firmware for STM8S003F3P6.
// Main source file.
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

#include "main.h"

#include "include/stm8.h"
#include "include/stm8-util.h"
#include "24c-eeprom.h"

unsigned short captureBuffer[CAPTURE_BUFFER_SIZE];
volatile unsigned short timeCounter;
unsigned char bufferPos, bufferSize;
unsigned char inMemoryBufferID;
unsigned char sleepCounter;

void PortD_Trigger() __interrupt(PORTD_IRQ)
{
    // Device wake-up interrupt.
    sleepCounter = 0;
}

void TIM2_update() __interrupt(TIMER2_TRIGGER_IRQ)
{   
    sleepCounter++;
    
    // Clear timer 2 interrupt flag.
    TIM2_SR1 &= ~TIM2_SR1_UIF;
}

void main()
{
    const unsigned char scanData[4] = {0xE0, 0xC8, 0xA8, 0x68};
    unsigned char scanPos;
    unsigned char scanRow, scanCol;
    
    // Initialize the system registers and peripherals.
    cli(); 
    initSystem();

    initExtEEPROM();

    // Initialize global variables.
    timeCounter = 0;
    scanRow = 0;
    scanCol = 0;
    inMemoryBufferID = 0xFF;
    sleepCounter = 0;

    // Enable interrupts.
    sei();

    // Main service loop.
    while(1)
    {        
        // Check for device wake-up from sleep mode.
        if(PD_CR2 != 0x00)
        {
            // Disable port D interrupts and shutdown Port C pull-down configuration.            
            PD_CR2 = 0x00;
            PC_ODR = 0xE8;
            PD_ODR = 0x40;
        }

        // Check idle timeout to enter device into sleep mode.
        if(sleepCounter > IDLE_TIMEOUT)
        {            
            // Enter device into sleep mode.
            PD_CR2 = 0x1E;
            PD_ODR = 0x00;

            // Set Port C to pull-down configuration to detect user key press (for wake-up).
            PC_ODR &= 0x17;
            sleepCounter = 0;

            halt();
        }
        
        scanPos = 0;
        scanRow = 0;
        scanCol = 0;

        // Start checking key-matrix for any key presses.
        while(scanPos < 4)
        {
            PC_ODR = scanData[scanPos];            
            if((PD_IDR & 0x1E) != 0x1E)
            {
                // Key press found, detect row and column of the pressed-key.
                sleepCounter = 0;
                scanRow = scanPos + 1;
                scanCol = ((~PD_IDR) & 0x1E) >> 1;
                break;
            }
            
            scanPos++;
        }

        // Reset (shutdown) keyboard scanning signals.
        PC_ODR = 0xE8;

        if((scanCol > 0) && (scanRow > 0))
        {
            // Key press is released, execute the action related to the key.
            scanPos = decodeKeyMatrix(&scanRow, &scanCol);

            if(scanPos < 16)
            {                
                // Disable interrupts and enable reset control.
                cli();
                PD_ODR = 0x00;

                // Select capture or transmit based on the mode jumper.
                if((PD_IDR & 0x20))
                {
                    // Transmit mode.
                    startTransmit(scanPos);
                }
                else
                {
                    // Capture mode.                    
                    startCapture(scanPos);
                } 

                // Enable interrupts and disable reset control.
                sleepCounter = 0;

                sei();   
                PD_ODR = 0x40;
            }        
        }

        delay_ms(1);
    }
}

unsigned char decodeKeyMatrix(unsigned char *row, unsigned char *col)
{   
    unsigned char retKey;
    
    // Decode row value.
    switch(*row)
    {
        case 1:
            retKey = 0;
            break;
        case 2:
            retKey = 4;
            break;
        case 3:
            retKey = 8;
            break;
        case 4:
            retKey = 12;
            break;
        default:
            // Unknown row value.
            return 0xFF;
    }

    // Decode column value to get the final key-pressed value.
    switch (*col)
    {
        case 1:
            return retKey + 0;
        case 2:
            return retKey + 1;
        case 4:
            return retKey + 2;
        case 8:
            return retKey + 3;
        default:
            // Unknown column value.
            return 0xFF;
    }
}

void initSystem()
{
    // Switch high speed internal clock prescaler (HSIDIV) to divide by 0 mode (16MHz).
    CLK_ECKR = 0x00;    
    CLK_CKDIVR = 0x00;
    CLK_PCKENR1 = 0xFF;
    CLK_PCKENR2 = 0xFF;
    CLK_CCOR = 0x00;

    // Select high speed internal clock (HSI) as master clock. 
    CLK_SWR = 0xE1;
    
    // Switching to new clock configuration.
    CLK_SWCR = 0x00;
    CLK_SWCR |= 0x02;

    // Waiting for switching to new clock configuration.
    while(CLK_SWCR & 0x01);

    // PA1[OUT] : IR LED drive.
    // PA2[OUT] : Capture / Busy status LED.
    // PA3[IN]  : TSSOP1838 IR photo module input.
    PA_DDR = 0xF7;
    PA_CR1 = 0xFF;
    PA_CR2 = 0x00;
    PA_ODR = 0x00;

    // PD1..PD4 [IN] : Keyboard input.
    // PD5[IN]  : Capture / Transmit mode selector.
    // PD6[OUT] : Reset control pin.
    PD_DDR = 0xC1;
    PD_CR1 = 0x60;
    PD_CR2 = 0x00;
    PD_ODR = 0x40;

    // PC3, PC4..PC7 [OUT] : Keyboard scan data.
    PC_DDR = 0xE8;
    PC_CR1 = 0xE8;
    PC_CR2 = 0x00;
    PC_ODR = 0xE8;

    // Setup Timer 1 to generate 38kHz carrier waveform on TIM1_CH4.
    TIM1_ARRH = 0x00;
    TIM1_ARRL = 0x33;

    TIM1_PSCRH = 0;
    TIM1_PSCRL = 0x07;

    // Enable PWM mode 2 on Timer 1.
    TIM1_CCMR4 |= 0x70;

    // Set PWM output enable on Timer 1 channel 4 (TIM1_CH4).
    TIM1_CCER2 |= 0x10;

    // Based on TIM1_ARR set duty cycle to 50%.
    TIM1_CCR4H = 0x00;
    TIM1_CCR4L = 0x1A;

    // Enable main output.
    TIM1_BKR = 0x80; 

    // Turn off carrier waveform output.
    TIM1_CR1 = 0x00;  

    // Set timer 2 prescaler to 256.
    TIM2_PSCR = 8;

    // Set ARRH to get 1 second interrupt.
    TIM2_ARRH = 0xF4;
    TIM2_ARRL = 0x24; 

    // Enable timer 2 interrupt.
    TIM2_IER |= TIM2_IER_UIE;
    TIM2_CR1 |= TIM2_CR1_CEN;

    // Configure Port D interrupts to trigger on both rising and falling edges. 
    EXTI_CR1 |= 0xC0;
}

void startTransmit(unsigned char buttonID)
{
    unsigned short eepromAddr = buttonID * CAPTURE_BUFFER_SIZE * 2;
    unsigned char tempPos;

    // Activate busy LED.
    PA_ODR |= 0x04;

    // Load button data from the EEPROM, only if it is not available in the memory.
    if(inMemoryBufferID != buttonID)
    {
        // Clear data buffer.
        for(tempPos = 0; tempPos < CAPTURE_BUFFER_SIZE; tempPos++)
        {
            captureBuffer[tempPos] = 0;
        }
        
        // Get the length of the data buffer.
        bufferSize = readExtEEPROM(eepromAddr++);
        bufferPos = 0;
        delay_ms(5);

        // Load IR transmit data into buffer.
        if((bufferSize > 0) && (bufferSize < CAPTURE_BUFFER_SIZE))
        {
            while(bufferPos < bufferSize)
            {
                captureBuffer[bufferPos] = readExtEEPROM(eepromAddr++);
                captureBuffer[bufferPos] = captureBuffer[bufferPos] << 8;
                delay_ms(4);
                captureBuffer[bufferPos] |= readExtEEPROM(eepromAddr++);
                delay_ms(5);

                bufferPos++;
            }
        }
        else
        {
            // Invalid buffer size or corrupted EEPROM data.
            bufferPos = 0;
            PA_ODR &= 0xF9;

            return;
        }
    }

    // Activate carrier wave osciliator.
    TIM1_CR1 = 0x01;
    delay_ms(3);

    // Reset counter and set waveform output to low [0] level.
    bufferPos = 0;
    PA_ODR &= 0xFD;

    while(bufferPos < bufferSize)
    {
        // Get length of the next wavrform.
        timeCounter =  captureBuffer[bufferPos];
        if(timeCounter > 0)
        {
            // Toggle output waveform.
            PA_ODR ^= 0x02;

            // Consume length of the waveform.
            while(timeCounter > 0)
            {
                delay_cycle(TRANSMIT_DELAY);
                timeCounter--;
            }   
        }
        
        // Go to next waveform record.
        bufferPos++;
    }

    // Shutdown waveform output and carrier wave osciliator.
    PA_ODR &= 0xFD;
    TIM1_CR1 = 0x00;

    // Turn off busy indicator.
    PA_ODR &= 0xFB;

    // Set ID belongs to the current memory buffer.
    inMemoryBufferID = buttonID;
}

void startCapture(unsigned char buttonID)
{    
    unsigned char temp, bufPos;
    unsigned short eepromAddr = buttonID * CAPTURE_BUFFER_SIZE * 2;
    
    // Initialize variables used for capturing.
    timeCounter = 0;
    bufferPos = 0;
    inMemoryBufferID = 0xFF;

    // Clear data buffer.
    for(temp = 0; temp < CAPTURE_BUFFER_SIZE; temp++)
    {
        captureBuffer[temp] = 0;
    }

    // Activate capture LED.
    PA_ODR |= 0x04;
    temp = 0x08;

    // Waiting for IR signal to start the capture.
    while(PA_IDR & 0x08)
    {
        nop();
    }

    // Start capture loop.
    do
    {        
        if((PA_IDR & 0x08) ^ temp)
        {
            // Signal state change detected. Capture counter value and reset the counter.
            captureBuffer[bufferPos] = timeCounter;
            bufferPos++;
            timeCounter = 0;
            temp = (PA_IDR & 0x08);
        }
        
        // Wait for 10us delay.
        delay_cycle(CAPTURE_DELAY);
        timeCounter++;
    }
    while((timeCounter <= TIMEOUT_LIMIT) && (bufferPos < CAPTURE_BUFFER_SIZE));

    if(bufferPos > 0)
    {        
        // Write captured data into EEPROM.
        writeExtEEPROM(eepromAddr++, bufferPos);
        delay_ms(5);
        for(bufPos = 0; bufPos < bufferPos; bufPos++)
        {
            // Write captured time limits into the EEPROM.
            writeExtEEPROM(eepromAddr++, ((captureBuffer[bufPos] >> 8) & 0xFF));
            delay_ms(5);
            writeExtEEPROM(eepromAddr++, (captureBuffer[bufPos] & 0xFF));
            delay_ms(5);
        }
    }

    // Turn off capture indicator.
    PA_ODR &= 0xFB;
}