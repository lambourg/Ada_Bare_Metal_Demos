with System;
with HAL; use HAL;

package Eth is
   type GEM_Registers_Type is record
      --  0x00
      Net_Ctrl   : UInt32;
      Net_Cfg    : UInt32;
      Net_Status : UInt32;
      Pad_0c     : UInt32;

      --  0x10
      Dma_Cfg   : UInt32;
      Tx_Status : UInt32;
      Rx_Qbar   : UInt32;
      Tx_Qbar   : UInt32;

      --  0x20
      Rx_Status   : UInt32;
      Intr_Status : UInt32;
      Intr_En     : UInt32;
      Intr_Dis    : UInt32;

      --  0x30
      Intr_Mask : UInt32;
      Phy_Maint : UInt32;
      Rx_Pauseq : UInt32;
      Tx_Pauseq : UInt32;

      --  0x40
      Pad_40 : UInt32;
      Pad_44 : UInt32;
      Pad_48 : UInt32;
      Pad_4c : UInt32;

      --  0x50
      Pad_50 : UInt32;
      Pad_54 : UInt32;
      Pad_58 : UInt32;
      Pad_5c : UInt32;

      --  0x60
      Pad_60 : UInt32;
      Pad_64 : UInt32;
      Pad_68 : UInt32;
      Pad_6c : UInt32;

      --  0x70
      Pad_70 : UInt32;
      Pad_74 : UInt32;
      Pad_78 : UInt32;
      Pad_7c : UInt32;

      --  0x80
      Hash_Bot       : UInt32;
      Hash_Top       : UInt32;
      Spec_Addr1_Bot : UInt32;
      Spec_Addr1_Top : UInt32;

      --  0x90
      Spec_Addr2_Bot : UInt32;
      Spec_Addr2_Top : UInt32;
      Spec_Addr3_Bot : UInt32;
      Spec_Addr3_Top : UInt32;

      --  0xa0
      Spec_Addr4_Bot : UInt32;
      Spec_Addr4_Top : UInt32;
      Type_Id_Match1 : UInt32;
      Type_Id_Match2 : UInt32;

      --  0xb0
      Type_Id_Match3 : UInt32;
      Type_Id_Match4 : UInt32;
      Wake_On_Lan    : UInt32;
      Ipg_Stretch    : UInt32;

      --  0xc0
      Stacked_Vlan        : UInt32;
      Tx_Pfc_Pause        : UInt32;
      Spec_Addr1_Mask_Bot : UInt32;
      Spec_Addr1_Mask_Top : UInt32;

      --  0xd0
      Pad_d0 : UInt32;
      Pad_d4 : UInt32;
      Pad_d8 : UInt32;
      Pad_dc : UInt32;

      --  0xe0
      Pad_e0 : UInt32;
      Pad_e4 : UInt32;
      Pad_e8 : UInt32;
      Pad_ec : UInt32;

      --  0xf0
      Pad_f0    : UInt32;
      Pad_f4    : UInt32;
      Pad_f8    : UInt32;
      Module_Id : UInt32;

      --  0x100
      Octets_Tx_Bot       : UInt32;
      Octets_Tx_Top       : UInt32;
      Frames_Tx           : UInt32;
      Broadcast_Frames_Tx : UInt32;

      --  0x110
      Multi_Frames_Tx    : UInt32;
      Pause_Frames_Tx    : UInt32;
      Frames_64b_Tx      : UInt32;
      Frames_65to127b_Tx : UInt32;

      --  0x120
      Frames_128to255b_Tx   : UInt32;
      Frames_256to511b_Tx   : UInt32;
      Frames_512to1023b_Tx  : UInt32;
      Frames_1024to1518b_Tx : UInt32;

      --  0x130
      Pad_130               : UInt32;
      Tx_Under_Runs         : UInt32;
      Single_Collisn_Frames : UInt32;
      Multi_Collisn_Frames  : UInt32;

      --  0x140
      Excessive_Collisns : UInt32;
      Late_Collisns      : UInt32;
      Deferred_Tx_Frames : UInt32;
      Carrier_Sense_Errs : UInt32;

      --  0x150
      Octets_Rx_Bot    : UInt32;
      Octets_Rx_Top    : UInt32;
      Frames_Rx        : UInt32;
      Bdcast_Frames_Rx : UInt32;

      --  0x160
      Multi_Frames_Rx    : UInt32;
      Pause_Rx           : UInt32;
      Frames_64b_Rx      : UInt32;
      Frames_65to127b_Rx : UInt32;

      --  0x170
      Frames_128to255b_Rx   : UInt32;
      Frames_256to511b_Rx   : UInt32;
      Frames_512to1023b_Rx  : UInt32;
      Frames_1024to1518b_Rx : UInt32;

      --  0x180
      Pad_180    : UInt32;
      Undersz_Rx : UInt32;
      Oversz_Rx  : UInt32;
      Jab_Rx     : UInt32;

      --  0x190
      Fcs_Errors          : UInt32;
      Length_Field_Errors : UInt32;
      Rx_Symbol_Errors    : UInt32;
      Align_Errors        : UInt32;

      --  0x1a0
      Rx_Resource_Errors : UInt32;
      Rx_Overrun_Errors  : UInt32;
      Ip_Hdr_Csum_Errors : UInt32;
      Tcp_Csum_Errors    : UInt32;

      --  0x1b0
      Udp_Csum_Errors : UInt32;
      Pad_1b4         : UInt32;
      Pad_1b8         : UInt32;
      Pad_1bc         : UInt32;

      --  0x1c0
      Pad_1c0         : UInt32;
      Pad_1c4         : UInt32;
      Timer_Strobe_S  : UInt32;
      Timer_Strobe_Ns : UInt32;

      --  0x1d0
      Timer_S      : UInt32;
      Timer_Ns     : UInt32;
      Timer_Adjust : UInt32;
      Timer_Incr   : UInt32;

      --  0x1e0
      Ptp_Tx_S  : UInt32;
      Ptp_Tx_Ns : UInt32;
      Ptp_Rx_S  : UInt32;
      Ptp_Rx_Ns : UInt32;

      --  0x1f0
      Ptp_Peer_Tx_S  : UInt32;
      Ptp_Peer_Tx_Ns : UInt32;
      Ptp_Peer_Rx_S  : UInt32;
      Ptp_Peer_Rx_Ns : UInt32;

      --  0x200

      --  0x210

      --  0x220

      --  0x230

      --  0x240

      --  0x250

      --  0x260

      --  0x270

      --  0x280

      --  0x290
   end record;

   package Gem_Bits is
      --  net_ctrl
      Clear_Stat_Regs : constant := 2**5;
      Mgmt_Port_En : constant := 2**4;
      Tx_En : constant := 2**3;
      Rx_En : constant := 2**2;
      Loopback_Local : constant := 2**1;

      --  net_cfg
      Rx_Chksum_Offld_En : constant := 2**24;
      Dbus_width : constant := 2**21;
      Mdc_Clk_Div : constant := 2**18;
      Pause_En : constant := 2**13;
      Gige_En : constant := 2**10;
      Uni_Hash_En : constant := 2**7;
      Multi_Hash_En : constant := 2**6;
      Copy_All : constant := 2**4;
      Full_Duplex : constant := 2**1;
      Speed : constant := 2**0;

      --  net_status
      Phy_Mgmt_Idle : constant := 2**2;

      --  dma_cfg
      Disc_When_No_Ahb : constant := 2**24;
      Ahb_Mem_Rx_Buf_Size : constant := 2**16;
      Csum_Gen_Offload_En : constant := 2**11;
      Tx_Pktbuf_Memsz_Sel : constant := 2**10;
      Rx_Pktbuf_Memsz_Sel : constant := 2**9;
      Ahb_Fixed_Burst_Len : constant := 2**0;

      --  Intr status
      Rx_Overrun : constant := 2**10;
      Tx_Complete : constant := 2**7;
      Tx_Used_Read : constant := 2**2;
      Rx_Complete : constant := 2**1;
      Mgmt_Done : constant := 2**0;

      --  RX status
      Frame_Recd : constant := 2**1;
   end Gem_Bits;

   Gem0 : GEM_Registers_Type
     with Volatile, Import, Address => System'To_Address (16#e000b000#);

   type DMA_Desc_Type is record
      Addr : UInt32;
      Flags : UInt32;
   end record;

   type DMA_Desc_Array is array (Natural range <>) of DMA_Desc_Type;

   package Gem_Desc_Bits is
      Addr_Wrap : constant := 2**1;  --  Last descriptor
      Addr_Owner : constant := 2**0; --  0: by controller, 1: by user

      --  RX
      Flag_Global : constant := 2**31;  --  Broadcast address
      Flag_Multi  : constant := 2**30;  --  Multicast
      Flag_Uni    : constant := 2**29;  --  Unicast
      Flag_Specific : constant := 2**27;
      Flag_Specific_Addr_Mask : constant := 3 * 2**25;
      Flag_Typeid : constant := 2**24;
      Flag_Snap   : constant := 2**24;
      Flag_Typeid_Mask : constant := 3 * 2**22;
      Flag_Rx_Csum_Mask : constant := 3 * 2**22;
      Flag_Vlan : constant := 2**21;
      Flag_Priority : constant := 2**20;
      Flag_Priority_Mask : constant := 7 * 2**17;
      Flag_Cfi : constant := 2**16;
      Flag_End : constant := 2**15;  --  End of frame
      Flag_Start : constant := 2**14;  --  Start of frame
      Flag_Fcs : constant := 2**13;
      Flag_Rx_Len_Mask : constant := 16#1fff#;

      --  TX
      Flag_Wrap : constant := 2**30;
      Flag_Tx_Error : constant := 2**29;
      Flag_Ahb_Error : constant := 2**27;
      Flag_Late_Coll : constant := 2**26;
      Flag_Tx_Csum_Mask : constant := 7 * 2**20;
      Flag_No_Crc : constant := 2**16;
      Flag_Last : constant := 2**15;
      Flag_Tx_Len_Mask : constant := 16#3fff#;
   end Gem_Desc_Bits;

   procedure Init;

   procedure Wait_Packet;

end Eth;
