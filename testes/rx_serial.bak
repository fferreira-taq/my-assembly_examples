#include "F:\teste PIC's 877A\rx_serial.h"

unsigned char str[4];

void main()
{

   setup_adc_ports(NO_ANALOGS);
   setup_adc(ADC_OFF);
   setup_psp(PSP_DISABLED);
   setup_spi(SPI_SS_DISABLED);
   setup_timer_0(RTCC_INTERNAL|RTCC_DIV_1);
   setup_timer_1(T1_DISABLED);
   setup_timer_2(T2_DISABLED,0,1);
   setup_comparator(NC_NC_NC_NC);
   setup_vref(FALSE);
   output_e(0);
   
   while (true)
   {
      str[0]=getc();
      str[1]=getc();
      str[2]=getc();
      str[3]=getc();

      output_e(255);
	delay_ms(200);
	output_e(0);

      printf("   %c%c%c%c...ok!!!",str[0],str[1],str[2],str[3]);	
   }
}