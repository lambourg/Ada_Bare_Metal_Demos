with System;
with HAL; use HAL;

package Slcr_Pkg is
   type SLCR_Registers_Type is record
      --  0x00
      SCL          : UInt32;
      SLCR_Lock    : UInt32;
      SLCR_Unlock  : UInt32;
      SLCR_Locksta : UInt32;

      --  0x10 - 0xff
      Pad_10_Ff : Word_Array (4 .. 16#40# - 1);

      --  0x100
      Arm_Pll_Ctrl : UInt32;
      Ddr_Pll_Ctrl : UInt32;
      Io_Pll_Ctrl  : UInt32;
      Pll_Status   : UInt32;

      --  0x110
      Arm_Pll_Cfg : UInt32;
      Ddr_Pll_Cfg : UInt32;
      Io_Pll_Cfg  : UInt32;
      Pad_11c     : UInt32;

      --  0x120
      Arm_Clk_Ctrl  : UInt32;
      Ddr_Clk_Ctrl  : UInt32;
      Dci_Clk_Ctrl  : UInt32;
      Aper_Clk_Ctrl : UInt32;

      --  0x130
      Usb0_Clk_Ctrl  : UInt32;
      Usb1_Clk_ctrl  : UInt32;
      Gem0_Rclk_Ctrl : UInt32;
      Gem1_Rclk_Ctrl : UInt32;

      --  0x140
      Gem0_Clk_Ctrl  : UInt32;
      Gem1_Clk_Ctrl  : UInt32;
      Smc_Clk_Ctrl   : UInt32;
      Lqspi_Clk_Ctrl : UInt32;

      --  0x150
      Sdio_Clk_Ctrl : UInt32;
      Uart_Clk_Ctrl : UInt32;
      Spi_Clk_Ctrl  : UInt32;
      Can_Clk_Ctrl  : UInt32;

      --  0x160
      Can_Mioclk_Ctrl : UInt32;
      Dbg_Clk_Ctrl    : UInt32;
      Pcap_Clk_Ctrl   : UInt32;
      Tposw_Clk_Ctrl  : UInt32;

      --  0x170
      Fpga0_Clk_Ctrl : UInt32;
      Fpga0_Thr_Ctrl : UInt32;
      Fpga0_Thr_Cnt  : UInt32;
      Fpga0_Thr_Sta  : UInt32;

      --  0x180
      Fpga1_Clk_Ctrl : UInt32;
      Fpga1_Thr_Ctrl : UInt32;
      Fpga1_Thr_Cnt  : UInt32;
      Fpga1_Thr_Sta  : UInt32;

      --  0x190
      Fpga2_Clk_Ctrl : UInt32;
      Fpga2_Thr_Ctrl : UInt32;
      Fpga2_Thr_Cnt  : UInt32;
      Fpga2_Thr_Sta  : UInt32;

      --  0x1a0
      Fpga3_Clk_Ctrl : UInt32;
      Fpga3_Thr_Ctrl : UInt32;
      Fpga3_Thr_Cnt  : UInt32;
      Fpga3_Thr_Sta  : UInt32;

      --  0x1b0
      Reserved_1b0 : Word_Array (0 .. 3);

      --  0x1c0
      Reserved_1c0 : Word_Array (0 .. 2);
      Clk_631_True : UInt32;

      --  0x1d0 - 0x1ff
      Reserved_1d0 : Word_Array (0 .. 11);

      --  0x200
      Pss_Rst_Ctrl   : UInt32;
      Ddr_Rst_Ctrl   : UInt32;
      Topws_Rst_Ctrl : UInt32;
      Dmac_Rst_Ctrl  : UInt32;

      --  0x210
      Usb_Rst_Ctrl  : UInt32;
      Gem_Rst_Ctrl  : UInt32;
      Sdio_Rst_Ctrl : UInt32;
      Spi_Rst_Ctrl  : UInt32;

      --  0x220
      Can_Rst_Ctrl  : UInt32;
      I2c_Rst_Ctrl  : UInt32;
      Uart_Rst_Ctrl : UInt32;
      Gpio_Rst_Ctrl : UInt32;

      --  0x230
      Lqspi_Rst_Ctrl : UInt32;
      Smc_Rst_Ctrl   : UInt32;
      Ocm_Rst_Ctrl   : UInt32;
      Reserved_23c   : UInt32;

      --  0x240
      Fpga_Rst_Ctrl   : UInt32;
      A9_Cpu_Rst_Ctrl : UInt32;
      Reserved_248    : UInt32;
      Rs_Awdt_Ctrl    : UInt32;

      --  0x250
      Reserved_250  : UInt32;
      Reserved_254  : UInt32;
      Reboot_Status : UInt32;
      Boot_Mode     : UInt32;

      --  0x260 - 0x300
      Reserved_260 : Word_Array (0 .. 10 * 4 - 1);

      --  0x300
      Apu_Ctrl     : UInt32;
      Wdt_Clk_Sel  : UInt32;
      Reserved_308 : UInt32;
      Reserved_30c : UInt32;

      --  0x310 - 0x440
      Reserved_310 : Word_Array (0 .. 19 * 4 - 1);

      --  0x440
      Tz_Dma_Ns        : UInt32;
      Tz_Dma_Irq_Ns    : UInt32;
      Tz_Dma_Periph_Ns : UInt32;
      Reserved_44c     : UInt32;

      --  0x450 - 0x530
      Reserved_450 : Word_Array (0 .. 14 * 4 - 1);

      --  0x530
      Pss_Idcode   : UInt32;
      Reserved_534 : UInt32;
      Reserved_538 : UInt32;
      Reserved_53c : UInt32;

      --  0x540 - 0x600
      Reserved_540 : Word_Array (0 .. 12 * 4 - 1);

      --  0x600
      Ddr_Urgent    : UInt32;
      Reserved_604  : UInt32;
      Reserved_608  : UInt32;
      Ddr_Cal_Start : UInt32;

      --  0x610
      Reserved_610   : UInt32;
      Ddr_Ref_Start  : UInt32;
      Ddr_Cmd_Sta    : UInt32;
      Ddr_Urgent_Sel : UInt32;

      --  0x620
      Ddr_Dfi_Status : UInt32;
      Reserved_624   : UInt32;
      Reserved_628   : UInt32;
      Reserved_62c   : UInt32;

      --  0x630 -- 0x700
      Reserved_630 : Word_Array (0 .. 13 * 4 - 1);

      --  0x700
      Mio_Pin_00 : UInt32;
      Mio_Pin_01 : UInt32;
      Mio_Pin_02 : UInt32;
      Mio_Pin_03 : UInt32;

      --  0x710
      Mio_Pin_04 : UInt32;
      Mio_Pin_05 : UInt32;
      Mio_Pin_06 : UInt32;
      Mio_Pin_07 : UInt32;

      --  0x720
      Mio_Pin_08 : UInt32;
      Mio_Pin_09 : UInt32;
      Mio_Pin_10 : UInt32;
      Mio_Pin_11 : UInt32;

      --  0x730
      Mio_Pin_12 : UInt32;
      Mio_Pin_13 : UInt32;
      Mio_Pin_14 : UInt32;
      Mio_Pin_15 : UInt32;

      --  0x740
      Mio_Pin_16 : UInt32;
      Mio_Pin_17 : UInt32;
      Mio_Pin_18 : UInt32;
      Mio_Pin_19 : UInt32;

      --  0x750
      Mio_Pin_20 : UInt32;
      Mio_Pin_21 : UInt32;
      Mio_Pin_22 : UInt32;
      Mio_Pin_23 : UInt32;

      --  0x760
      Mio_Pin_24 : UInt32;
      Mio_Pin_25 : UInt32;
      Mio_Pin_26 : UInt32;
      Mio_Pin_27 : UInt32;

      --  0x770
      Mio_Pin_28 : UInt32;
      Mio_Pin_29 : UInt32;
      Mio_Pin_30 : UInt32;
      Mio_Pin_31 : UInt32;

      --  0x780
      Mio_Pin_32 : UInt32;
      Mio_Pin_33 : UInt32;
      Mio_Pin_34 : UInt32;
      Mio_Pin_35 : UInt32;

      --  0x790
      Mio_Pin_36 : UInt32;
      Mio_Pin_37 : UInt32;
      Mio_Pin_38 : UInt32;
      Mio_Pin_39 : UInt32;

      --  0x7a0
      Mio_Pin_40 : UInt32;
      Mio_Pin_41 : UInt32;
      Mio_Pin_42 : UInt32;
      Mio_Pin_43 : UInt32;

      --  0x7b0
      Mio_Pin_44 : UInt32;
      Mio_Pin_45 : UInt32;
      Mio_Pin_46 : UInt32;
      Mio_Pin_47 : UInt32;

      --  0x7c0
      Mio_Pin_48 : UInt32;
      Mio_Pin_49 : UInt32;
      Mio_Pin_50 : UInt32;
      Mio_Pin_51 : UInt32;

      --  0x7d0
      Mio_Pin_52   : UInt32;
      Mio_Pin_53   : UInt32;
      Reserved_7d8 : UInt32;
      Reserved_7dc : UInt32;

      --  0x7e0
      Reserved_7e0 : Word_Array (0 .. 7);

      --  0x800
      Reserved_800 : UInt32;
      Mio_Loopback : UInt32;
      Reserved_808 : UInt32;
      Mio_Mst_Tri0 : UInt32;

      --  0x810
      Mio_Mst_Tri1 : UInt32;
      Reserved_814 : UInt32;
      Reserved_818 : UInt32;
      Reserved_81c : UInt32;

      --  0x820 - 0x830
      Reserved_820 : Word_Array (0 .. 3);

      --  0x830
      Sd0_Wp_Cd_Sel : UInt32;
      Sd1_Wp_Cd_Sel : UInt32;
      Reserved_838  : UInt32;
      Reserved_83c  : UInt32;

      --  0x840 - 0x900
      Reserved_840 : Word_Array (0 .. 12 * 4 - 1);

      --  0x900
      Lvl_Shiftr_En : UInt32;
      Reserved_904  : UInt32;
      Reserved_908  : UInt32;
      Reserved_90c  : UInt32;

      --  0x910
      Ocm_Cfg      : UInt32;
      Reserved_914 : UInt32;
      Reserved_918 : UInt32;
      Reserved_91c : UInt32;

      --  0x920 - 0xb00
      Reserved_920 : Word_Array (0 .. 30 * 4 - 1);

      --  0xb00
      Gpiob_Ctrl       : UInt32;
      Gpiob_Cfg_Cmos18 : UInt32;
      Gpiob_Cfg_Cmos25 : UInt32;
      Gpiob_Cfg_Cmos33 : UInt32;

      --  0xb10
      Reserved_B10         : UInt32;
      Gpiob_Cfg_Hstl       : UInt32;
      Gpiob_Drvr_Bias_Ctrl : UInt32;
      Reserved_B1c         : UInt32;

      --  0xb20 - 0xb40
      Reserved_B20 : Word_Array (0 .. 7);

      --  0xb40
      Ddriob_Addr0 : UInt32;
      Ddriob_Addr1 : UInt32;
      Ddriob_Data0 : UInt32;
      Ddriob_Data1 : UInt32;

      --  0xb50
      Ddriob_Diff0           : UInt32;
      Ddriob_Diff1           : UInt32;
      Ddriob_Clock           : UInt32;
      Ddriob_Drive_Slew_Addr : UInt32;

      --  0xb60
      Ddriob_Drive_Slow_Data  : UInt32;
      Ddriob_Drive_Slow_Diff  : UInt32;
      Ddriob_Drive_Slow_Clock : UInt32;
      Ddrib_Ddr_Ctrl          : UInt32;

      --  0xb70
      Ddriob_Dci_Ctrl   : UInt32;
      Ddriob_Dci_Status : UInt32;
   end record;

   SLCR : SLCR_Registers_Type
     with Volatile, Import, Address => System'To_Address (16#f800_0000#);
end Slcr_Pkg;
