with Interfaces; use Interfaces;
with Interfaces.Cache; use Interfaces.Cache;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Hex_Images; use Hex_Images;
with Slcr_Pkg; use Slcr_Pkg;

package body Eth is
   function To_Unsigned_32 is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Unsigned_32);

   --  Mii registers.
   Mii_Control : constant := 0;
   Mii_Status : constant := 1;

   Mii_Link_Status : constant := 2**2;

   Mii_Duplex_Mode : constant := 2**8;
   Mii_Speed_100Mb : constant := 2**13;
   Mii_Speed_1000Mb : constant := 2**6;

   --  MII phy address
   Phy_Addr : constant Unsigned_32 := 2#00111#;

   Nbr_Tx_Bufs : constant := 8;
   subtype Rx_Buf_Range is Natural range 0 .. 7;

   subtype Buf_Type is Byte_Array (0 .. 1023);
   type Tx_Buf_Arr is array (1 .. Nbr_Tx_Bufs) of Buf_Type
     with Alignment => 4;
   type Rx_Buf_Arr is array (Rx_Buf_Range) of Buf_Type
     with Alignment => 4;

   Rx_Bufs : Rx_Buf_Arr;

   --  RX descriptors (in non-cached memory).
   Rx_Descs : DMA_Desc_Array (Rx_Buf_Range);
   pragma Linker_Section (Rx_Descs, ".ucache");

   Cur_Rx_Desc : Rx_Buf_Range;

   procedure Phy_Read
     (Addr : Unsigned_32; Reg : Unsigned_32; Res : out Unsigned_16)
   is
      use Gem_Bits;
      Val : Unsigned_32;
   begin
      --  Wait until no MDIO operation in progress
      while (Gem0.Net_Status and Phy_Mgmt_Idle) = 0 loop
         null;
      end loop;

      --  Write data to the PHY maintainance register
      Gem0.Phy_Maint := 2#0110# * 2**28 + Addr * 2**23 + Reg * 2**18
        + 2#10# * 2**16;

      --  Wait for completion of operation
      while (Gem0.Net_Status and Phy_Mgmt_Idle) = 0 loop
         null;
      end loop;

      --  Read data bits for a read operation
      Val := Gem0.Phy_Maint;
      Res := Unsigned_16 (Val and 16#ffff#);
   end Phy_Read;

   procedure Init
   is
      use Gem_Bits;
      use Gem_Desc_Bits;
   begin
      --  16.3.1 Initialize the controller
      Gem0.Net_Ctrl := 0;
      Gem0.Net_Ctrl := Clear_Stat_Regs;
      Gem0.Rx_Status := 16#0f#;
      Gem0.Tx_Status := 16#ff#;
      Gem0.Intr_Dis := 16#7ff_feff#;
      Gem0.Rx_Qbar := 0;
      Gem0.Tx_Qbar := 0;

      --  16.3.2 Configure the Controller
      Gem0.Net_Cfg := Full_Duplex or Speed or Multi_Hash_En or Copy_All
        or Rx_Chksum_Offld_En or Pause_En or (3 * Mdc_Clk_Div);
      Gem0.Spec_Addr1_Bot := 16#00_01_02_03#;
      Gem0.Spec_Addr1_Top := 16#aa_00#;
      Gem0.Net_Ctrl := Mgmt_Port_En;

      --  Check link
      declare
         Reg : Unsigned_16;
      begin
         Phy_Read (Phy_Addr, Mii_Status, Reg);
         if (Reg and Mii_Link_Status) = 0 then
            Put_Line ("Link is down");
            return;
         end if;

         Phy_Read (Phy_Addr, Mii_Control, Reg);
         Put ("Link is up");
         if (Reg and Mii_Duplex_Mode) /= 0 then
            Put (", full-duplex");
         end if;
         case Reg and (Mii_Speed_100Mb or Mii_Speed_1000Mb) is
            when 0 =>
               Put (", 10Mb");
            when Mii_Speed_100Mb =>
               Put (", 100Mb");
            when Mii_Speed_1000Mb =>
               Put (", 1000Mb");
            when others =>
               Put (", ???Mb");
         end case;
         New_Line;

         if (Reg and Mii_Duplex_Mode) /= 0 then
            Gem0.Net_Cfg := Gem0.Net_Cfg or Full_Duplex;
         end if;
         case Reg and (Mii_Speed_100Mb or Mii_Speed_1000Mb) is
            when 0 =>
               null;
            when Mii_Speed_100Mb =>
               Gem0.Net_Cfg := Gem0.Net_Cfg or Speed;
            when Mii_Speed_1000Mb =>
               Gem0.Net_Cfg := Gem0.Net_Cfg or Gige_En;
            when others =>
               null;
         end case;
      end;

      --  16.3.3 IO Configuration
      null;  --  SLCR already set

      --  16.3.4 Configure the PHY
      null; --  TODO

      --  16.3.5 Configure the Buffer Descriptors
      for I in Rx_Descs'Range loop
         Rx_Descs (I) := (Addr => To_Unsigned_32 (Rx_Bufs (I)'Address),
                          Flags => 0);
      end loop;
      Rx_Descs (Rx_Descs'Last).Addr :=
        Rx_Descs (Rx_Descs'Last).Addr or Addr_Wrap;
      Gem0.Rx_Qbar := To_Unsigned_32 (Rx_Descs'Address);
      Gem0.Dma_Cfg := (Buf_Type'Size / 64) * Ahb_Mem_Rx_Buf_Size
        + 3 * Rx_Pktbuf_Memsz_Sel + 1 * Tx_Pktbuf_Memsz_Sel
        + 1 * Csum_Gen_Offload_En + 16#10# * Ahb_Fixed_Burst_Len;
      Cur_Rx_Desc := Rx_Descs'First;

      Dcache_Flush_By_Range (Rx_Bufs'Address, Rx_Bufs'Size / 8);

      --  16.3.6 Configure Interrupts
      Gem0.Intr_En := Rx_Complete or Mgmt_Done;

      --  16.3.7 Enable the Controller.
      --  Gem0.Net_Ctrl := Gem0.Net_Ctrl or Tx_En or Rx_En;
      Gem0.Net_Ctrl := Gem0.Net_Ctrl or Rx_En;
   end Init;

   procedure Wait_Packet
   is
      use Gem_Bits;
      use Gem_Desc_Bits;

      Status : Unsigned_32;
      Old_Status : Unsigned_32 := 0;
      Rx_Status : Unsigned_32;
      Old_Rx_Status : Unsigned_32 := 0;
   begin
      loop
         --  Buffer ready for user.
         exit when (Rx_Descs (Cur_Rx_Desc).Addr and Addr_Owner) /= 0;

         --  Clear interrupts
         Gem0.Intr_Status := Rx_Complete;
         Gem0.Rx_Status := Frame_Recd;

         --  Check for race condition.
         exit when (Rx_Descs (Cur_Rx_Desc).Addr and Addr_Owner) /= 0;

         --  Wait for rx_complete
         loop
            Status := Gem0.Intr_Status;
            exit when (Status and Rx_Complete) /= 0;
         end loop;
         Rx_Status := Gem0.Rx_Status;
         Put ("rx_status: ");
         Put_Line (Hex8 (Rx_Status));
      end loop;

      Put ("Desc[");
      Put (Hex2 (Unsigned_8 (Cur_Rx_Desc)));
      Put ("]: ");
      Put (Hex8 (Rx_Descs (Cur_Rx_Desc).Addr));
      Put (' ');
      Put (Hex8 (Rx_Descs (Cur_Rx_Desc).Flags));
      for J in 0 .. 15 loop
         Put (' ');
         Put (Hex2 (Rx_Bufs (Cur_Rx_Desc)(J)));
      end loop;
      New_Line;

      Dcache_Flush_By_Range (Rx_Bufs (Cur_Rx_Desc)'Address,
                             Rx_Bufs (Cur_Rx_Desc)'Size / 8);

      declare
         Buf_Addr : constant Unsigned_32 :=
           To_Unsigned_32 (Rx_Bufs (Cur_Rx_Desc)'Address);
      begin
         if Cur_Rx_Desc = Rx_Descs'Last then
            Rx_Descs (Cur_Rx_Desc).Addr := Buf_Addr or Addr_Wrap;
            Cur_Rx_Desc := Rx_Descs'First;
         else
            Rx_Descs (Cur_Rx_Desc).Addr := Buf_Addr;
            Cur_Rx_Desc := Cur_Rx_Desc + 1;
         end if;
      end;
   end Wait_Packet;

   procedure Dump_Regs is
   begin
      Put_Line ("SLCR:");
      Put ("Lock: ");
      Put_Line (Hex8 (SLCR.SCL));
      Put ("gem0_clk_ctrl: ");
      Put_Line (Hex8 (SLCR.Gem0_Clk_Ctrl));
      Put ("gem0_rclk_ctrl: ");
      Put_Line (Hex8 (SLCR.Gem0_Rclk_Ctrl));
      Put ("gem_rst_ctrl: ");
      Put_Line (Hex8 (SLCR.Gem_Rst_Ctrl));
      Put ("mio_pin_16 @ ");
      Put (Hex8 (To_Unsigned_32 (SLCR.Mio_Pin_16'Address)));
      Put (": ");
      Put_Line (Hex8 (SLCR.Mio_Pin_16));

      Put ("mio_pin_22: ");
      Put_Line (Hex8 (SLCR.Mio_Pin_22));

      Put ("mio_pin_52: ");
      Put (Hex8 (SLCR.Mio_Pin_52));
      Put (", 53: ");
      Put_Line (Hex8 (SLCR.Mio_Pin_53));

      Put ("gpiob: ");
      Put_Line (Hex8 (SLCR.Gpiob_Ctrl));

      Put ("net_status @ ");
      Put (Hex8 (To_Unsigned_32 (Gem0.Net_Status'Address)));
      Put (": ");
      Put_Line (Hex8 (Gem0.Net_Status));
      Put ("net_cfg: ");
      Put_Line (Hex8 (Gem0.Net_Cfg));
   end Dump_Regs;

   procedure Dump_Mii is
      Val : Unsigned_16;
   begin
      Put ("MII: ");
      for I in Unsigned_32 range 0 .. 15 loop
         Phy_Read (2#00111#, I, Val);
         Put (' ');
         Put (Hex4 (Val));
      end loop;
      New_Line;
   end Dump_Mii;
end Eth;
