------------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2015-2017, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with System;                   use System;
with System.Storage_Elements;  use System.Storage_Elements;

with DMA; use DMA;

package body Bitmap is

   DMA_Periph : DMA_Peripheral renames DMA_0;

   type SCB_Index is range 0 .. 100;
   subtype Valid_SCB_Index is SCB_Index range 1 .. SCB_Index'Last;

   DMA_SCB    : array (Valid_SCB_Index) of aliased DMA_Control_Block
     with Import, Address => System'To_Address (16#3ae00000#);

   BPP        : constant := 2;
   --  Bytes per Pixel

   procedure DMA_Find_Free_SCB
     (Tail      : out SCB_Index;
      Available : out SCB_Index);
   --  Find the first free control block
   --  Tail: index of the last block in the current chain
   --  Available: index of the first available block

   protected DMA_Controller is
      procedure Wait_Transfer;
      --  Wait for all DMA transfers to terminate

      procedure Take_Transfer (Num_Control_Blocks : Natural;
                               Index              : out SCB_Index;
                               Status             : out Boolean);
      --  Waits for Num_Control_Blocks to be available, and returns the index
      --  of the first block. If another task is preparing a transfer, then
      --  Status is set to False

      procedure Start_Transfer;
      --  Sets the Active bit to start/resume DMA transfers

   private
      DMA_Started     : Boolean := False;
      New_Block       : SCB_Index := 0;
   end DMA_Controller;

   --------------------
   -- DMA_Controller --
   --------------------

   protected body DMA_Controller is

      -------------------
      -- Wait_Transfer --
      -------------------

      procedure Wait_Transfer is
      begin
         if not DMA_Started then
            return;
         end if;

         while not DMA_Periph.CS.Ended
           or else DMA_Periph.CS.Active
         loop
            null;
         end loop;

         --  Write 1 to clear
         DMA_Periph.CS.Ended := True;

         DMA_Started := False;
      end Wait_Transfer;

      -------------------
      -- Take_Transfer --
      -------------------

      procedure Take_Transfer (Num_Control_Blocks : Natural;
                               Index              : out SCB_Index;
                               Status             : out Boolean)
      is
         Tail      : SCB_Index;
         Prev      : SCB_Index;
         Next      : SCB_Index;
      begin
         if New_Block /= 0 then
            --  Some other task is preparing a new DMA transfer. Cannot
            --  do anything until this is finished.
            Status := False;
            Index := 0;

            return;
         end if;

         loop
            DMA_Find_Free_SCB (Tail, Index);

            if Index /= 0 then
               Prev := Index;
               --  Check that enough blocks are available
               for J in 2 .. Num_Control_Blocks loop
                  if Prev = DMA_SCB'Last then
                     Next := DMA_SCB'First;
                  else
                     Next := Prev + 1;
                  end if;

                  if DMA_Periph.CONBLK_AD =
                    To_BUS (DMA_SCB (Next)'Address)
                  then
                     --  Not enough room
                     Index := 0;
                     exit;
                  end if;
               end loop;
            end if;

            exit when Index /= 0;
         end loop;

         --  Pause the transfer
         DMA_Periph.CS.Active := False;

         --  Update the chain
         Prev := Tail;

         for J in 1 .. Num_Control_Blocks loop
            if Prev = DMA_SCB'Last then
               Next := DMA_SCB'First;
            else
               Next := Prev + 1;
            end if;

            if Prev /= 0 then
               DMA_SCB (Prev).Next_CB := To_BUS (DMA_SCB (Next)'Address);
            end if;

            Prev := Next;
         end loop;

         DMA_SCB (Next).Next_CB := System.Null_Address;

         --  Setup New_Block for use when calling Start_Transfer
         New_Block := Index;
         Status    := True;
      end Take_Transfer;

      --------------------
      -- Start_Transfer --
      --------------------

      procedure Start_Transfer is
      begin
         DMA_Started := True;

         --  Prepair the peripheral for the transfer
         if DMA_Periph.CONBLK_AD = System.Null_Address then
            DMA_Periph.CONBLK_AD := To_BUS (DMA_SCB (New_Block)'Address);
         elsif DMA_Periph.NEXTCONB = System.Null_Address then
            DMA_Periph.NEXTCONB := To_BUS (DMA_SCB (New_Block)'Address);
         end if;
         --  Start/resume the transfer
         DMA_Periph.CS.Active := True;
         --  Reset New_Block
         New_Block := 0;
      end Start_Transfer;

   end DMA_Controller;

   -----------------------
   -- DMA_Find_Free_SCB --
   -----------------------

   procedure DMA_Find_Free_SCB
     (Tail      : out SCB_Index;
      Available : out SCB_Index)
   is
      Current_Block : constant BUS_Address := DMA_Periph.CONBLK_AD;
      Index         : SCB_Index;
      Next_Index    : SCB_Index;
      Transfering   : SCB_Index := 0;

   begin
      if Current_Block = System.Null_Address then
         --  Easy case: no DMA transfer is active
         Tail := 0;
         Available := DMA_SCB'First;

         return;
      end if;

      --  Transfer is currently in progress
      Index := DMA_SCB'First;
      loop
         if Index = DMA_SCB'Last then
            Next_Index := DMA_SCB'First;
         else
            Next_Index := Index + 1;
         end if;

         if Transfering = 0
           and then To_BUS (DMA_SCB (Index)'Address) = Current_Block
         then
            --  Found the block being transfered
            Transfering := Index;

         elsif Transfering /= 0 then
            --  Search for the terminal block in the chain of transfers
            if DMA_SCB (Index).Next_CB = System.Null_Address then
               --  Next block is unused
               --  But check that it's not the transfering block
               if Next_Index /= Transfering then
                  --  return the values
                  Tail := Index;
                  Available := Next_Index;
               else
                  --  all blocks are in use
                  Tail := 0;
                  Available := 0;
               end if;

               return;
            end if;
         end if;

         --  Move on to the next block
         Index := Next_Index;
      end loop;
   end DMA_Find_Free_SCB;

   -------------------
   -- Wait_Transfer --
   -------------------

   procedure Wait_Transfer
     (Buffer  : Bitmap_Buffer)
   is
      pragma Unreferenced (Buffer);
   begin
      DMA_Controller.Wait_Transfer;
   end Wait_Transfer;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel
     (Buffer  : Bitmap_Buffer;
      X       : Natural;
      Y       : Natural;
      Value   : Bitmap_Color)
   is
      Col : constant Unsigned_32 :=
              Bitmap_Color_To_Unsigned_32 (Buffer.Color_Mode, Value);
   begin
      Set_Pixel (Bitmap_Buffer'Class (Buffer), X, Y, Col);
   end Set_Pixel;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel
     (Buffer : Bitmap_Buffer;
      X      : Natural;
      Y      : Natural;
      Value  : Unsigned_32)
   is
      Offset : constant Natural := X + Y * Buffer.Width;

   begin
      if X >= Buffer.Width
        or else Y >= Buffer.Height
      then
         return;
      end if;

      case Buffer.Color_Mode is
         when ARGB_8888 =>
            declare
               Pixel : aliased Unsigned_32
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 4);
            begin
               Pixel := Value;
            end;

         when RGB_888 =>
            declare
               Pixel_B : aliased Unsigned_8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3);
               Pixel_G : aliased Unsigned_8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3 + 1);
               Pixel_R : aliased Unsigned_8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3 + 2);
               R       : constant Unsigned_8 :=
                           Unsigned_8 (Shift_Right (Value and 16#FF0000#, 16));
               G       : constant Unsigned_8 :=
                           Unsigned_8 (Shift_Right (Value and 16#FF00#, 8));
               B       : constant Unsigned_8 := Unsigned_8 (Value and 16#FF#);
            begin
               Pixel_B := B;
               Pixel_G := G;
               Pixel_R := R;
            end;

         when ARGB_1555 | ARGB_4444 | RGB_565 | AL_88 =>
            declare
               Pixel : aliased Unsigned_16
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 2);
            begin
               Pixel := Unsigned_16 (Value and 16#FF_FF#);
            end;

         when L_8 | AL_44 | A_8 =>
            declare
               Pixel : aliased Unsigned_8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset);
            begin
               Pixel := Unsigned_8 (Value and 16#FF#);
            end;

         when L_4 | A_4 =>
            declare
               Pixel : aliased Unsigned_8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset / 2);
            begin
               if Offset mod 2 = 0 then
                  Pixel :=
                    (Pixel and 16#0F#) or
                    Shift_Left (Unsigned_8 (Value and 16#0F#), 4);
               else
                  Pixel := (Pixel and 16#F0#) or Unsigned_8 (Value and 16#0F#);
               end if;
            end;

      end case;
   end Set_Pixel;

   ---------------------
   -- Set_Pixel_Blend --
   ---------------------

   procedure Set_Pixel_Blend
     (Buffer : Bitmap_Buffer;
      X      : Natural;
      Y      : Natural;
      Value  : Bitmap_Color)
   is
      Col : Bitmap_Color;
      FgA, FgR, FgG, FgB : Float;
      BgA, BgR, BgG, BgB : Float;
      RA, RR, RG, RB     : Float;

   begin
      if Value.Alpha = 255 then
         Set_Pixel (Bitmap_Buffer'Class (Buffer), X, Y, Value);
      else
         Col := Get_Pixel (Bitmap_Buffer'Class (Buffer), X, Y);
         BgA := Float (Col.Alpha) / 255.0;
         BgR := Float (Col.Red) / 255.0;
         BgG := Float (Col.Green) / 255.0;
         BgB := Float (Col.Blue) / 255.0;

         FgA := Float (Value.Alpha) / 255.0;
         FgR := Float (Value.Red) / 255.0;
         FgG := Float (Value.Green) / 255.0;
         FgB := Float (Value.Blue) / 255.0;

         RA := 1.0 - (1.0 - FgA) * (1.0 - FgB);
         RR := FgR * FgA / RA + BgR * BgA * (1.0 - FgA) / RA;
         RG := FgG * FgA / RA + BgG * BgA * (1.0 - FgA) / RA;
         RB := FgB * FgA / RA + BgB * BgA * (1.0 - FgA) / RA;

         Col := (Alpha => Unsigned_8 (RA * 255.0),
                 Red   => Unsigned_8 (RR * 255.0),
                 Green => Unsigned_8 (RG * 255.0),
                 Blue  => Unsigned_8 (RB * 255.0));
         Set_Pixel (Bitmap_Buffer'Class (Buffer), X, Y, Col);
      end if;
   end Set_Pixel_Blend;

   ---------------
   -- Get_Pixel --
   ---------------

   function Get_Pixel
     (Buffer : Bitmap_Buffer;
      X      : Natural;
      Y      : Natural)
      return Bitmap_Color
   is
      Native_Color : Unsigned_32;
   begin
      Native_Color := Get_Pixel
        (Bitmap_Buffer'Class (Buffer),
         X, Y);

      return Unsigned_32_To_Bitmap_Color (Buffer.Color_Mode, Native_Color);
   end Get_Pixel;

   ---------------
   -- Get_Pixel --
   ---------------

   function Get_Pixel
     (Buffer : Bitmap_Buffer;
      X      : Natural;
      Y      : Natural)
      return Unsigned_32
   is
      Offset : constant Natural := X + Y * Buffer.Width;

   begin
      if X >= Buffer.Width
        or else Y >= Buffer.Height
      then
         return 0;
      end if;

      case Buffer.Color_Mode is
         when ARGB_8888 =>
            declare
               Pixel : aliased Unsigned_32
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 4);
            begin
               return Pixel;
            end;

         when RGB_888 =>
            declare
               Pixel_B : aliased Unsigned_8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3);
               Pixel_G : aliased Unsigned_8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3 + 1);
               Pixel_R : aliased Unsigned_8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3 + 2);
            begin
               return Shift_Left (Unsigned_32 (Pixel_R), 16)
                 or Shift_Left (Unsigned_32 (Pixel_G), 8)
                 or Unsigned_32 (Pixel_B);
            end;

         when ARGB_1555 | ARGB_4444 | RGB_565 | AL_88 =>
            declare
               Pixel : aliased Unsigned_16
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 2);
            begin
               return Unsigned_32 (Pixel);
            end;

         when L_8 | AL_44 | A_8 =>
            declare
               Pixel : aliased Unsigned_8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset);
            begin
               return Unsigned_32 (Pixel);
            end;

         when L_4 | A_4 =>
            declare
               Pixel : aliased Unsigned_8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset / 2);
            begin
               if Offset mod 2 = 0 then
                  return Unsigned_32 (Shift_Right (Pixel and 16#F0#, 4));
               else
                  return Unsigned_32 (Pixel and 16#0F#);
               end if;
            end;
      end case;
   end Get_Pixel;

   ----------
   -- Fill --
   ----------

   procedure Fill
     (Buffer : Bitmap_Buffer;
      Color  : Bitmap_Color)
   is
      Col : constant Unsigned_32 :=
              Bitmap_Color_To_Unsigned_32 (Buffer.Color_Mode, Color);
   begin
      Fill (Bitmap_Buffer'Class (Buffer), Col);
   end Fill;

   ----------
   -- Fill --
   ----------

   procedure Fill
     (Buffer : Bitmap_Buffer;
      Color  : Unsigned_32)
   is
   begin
      Fill_Rect (Bitmap_Buffer'Class (Buffer), Color,
                 0, 0,
                 Buffer.Width, Buffer.Height);
   end Fill;

   ---------------
   -- Fill_Rect --
   ---------------

   procedure Fill_Rect
     (Buffer      : Bitmap_Buffer;
      Color       : Bitmap_Color;
      X           : Integer;
      Y           : Integer;
      Width       : Integer;
      Height      : Integer;
      Synchronous : Boolean := False)
   is
   begin
      Fill_Rect
        (Bitmap_Buffer'Class (Buffer),
         Bitmap_Color_To_Unsigned_32 (Buffer.Color_Mode, Color),
         X, Y, Width, Height, Synchronous);
   end Fill_Rect;

   ---------------
   -- Fill_Rect --
   ---------------

   procedure Fill_Rect
     (Buffer      : Bitmap_Buffer;
      Color       : Unsigned_32;
      X           : Integer;
      Y           : Integer;
      Width       : Integer;
      Height      : Integer;
      Synchronous : Boolean := False)
   is
      Offset      : constant Storage_Offset :=
                      Storage_Offset ((X + Y * Buffer.Width) * BPP);
      Offset2     : constant Storage_Offset :=
                      Offset + Storage_Offset (Buffer.Width * BPP);
      Dest_Stride : Integer_16;
      Status      : Boolean;
      Index       : SCB_Index;
      Num_Blocks  : Natural;
      W, H        : Natural;

   begin
      if Width <= 0 or else Height <= 0 then
         return;
      end if;

      if X + Width > Buffer.Width then
         W := Buffer.Width - X;
      else
         W := Width;
      end if;

      if Y + Height > Buffer.Height then
         H := Buffer.Height - Y;
      else
         H := Height;
      end if;

      Dest_Stride := Integer_16 (Buffer.Width - W) * BPP;

      if W > 1 and then H > 1 then
         Num_Blocks := 2;
      else
         Num_Blocks := 1;
      end if;

      loop
         DMA_Controller.Take_Transfer (Num_Blocks, Index, Status);
         exit when Status;
      end loop;

      if W > 1 then
         --  DMA transfers are byte-oriented, while here we need word-oriented
         --  transfers.
         --  So we need to chain two dma transfers, one repeating the color
         --  on the first line, to achieve a proper byte sequence, then
         --  the second one repeating this first line through all height
         --  First transfer fills one line

         DMA_SCB (Index).TI :=
           (Interrupt_Enable => False,
            Two_D_Mode       => True,
            Dest_Inc         => True,
            Dest_Width       => Width_128bit,
            Src_Inc          => False,
            Src_Width        => Width_32bit,
            Wait_Response    => True,
            others           => <>);
         DMA_SCB (Index).Reserved_7 := Color;
         DMA_SCB (Index).Source_Address :=
           To_BUS (DMA_SCB (Index).Reserved_7'Address);
         DMA_SCB (Index).Destination_Address := To_BUS (Buffer.Addr + Offset);
         DMA_SCB (Index).Transfer_Length :=
           (TD_Mode  => True,
            X_Length => Unsigned_16 (BPP),
            Y_Length => UInt14 (W - 1),
            others   => <>);
         DMA_SCB (Index).Stride :=
           (S_STRIDE => 0,
            D_STRIDE => 0);

         if H > 1 then
            if Index = DMA_SCB'Last then
               Index := DMA_SCB'First;
            else
               Index := Index + 1;
            end if;

            --  Copy the first line to the following ones
            DMA_SCB (Index) :=
              (TI                  =>
                 (Interrupt_Enable => False,
                  Two_D_Mode       => True,
                  Dest_Inc         => True,
                  Dest_Width       => Width_128bit,
                  Src_Inc          => True,
                  Src_Width        => Width_128bit,
                  Wait_Response    => True,
                  others           => <>),
               Source_Address      => To_BUS (Buffer.Addr + Offset),
               Destination_Address => To_BUS (Buffer.Addr + Offset2),
               Transfer_Length     => (TD_Mode  => True,
                                       X_Length => Unsigned_16 (BPP * W),
                                       Y_Length => UInt14 (H - 2),
                                       others   => <>),
               Stride              => (S_STRIDE => Integer_16 ((-BPP) * W),
                                       D_STRIDE => Dest_Stride),
               Next_CB             => System.Null_Address,
               others              => <>);
         end if;
      else
         DMA_SCB (Index) :=
           (TI                  =>
              (Interrupt_Enable => False,
               Two_D_Mode       => True,
               Dest_Inc         => True,
               Dest_Width       => Width_32bit,
               Src_Inc          => False,
               Src_Width        => Width_32bit,
               Wait_Response    => True,
               others           => <>),
            Source_Address      => To_BUS (DMA_SCB (Index).Reserved_7'Address),
            Destination_Address => To_BUS (Buffer.Addr + Offset),
            Transfer_Length     => (TD_Mode  => True,
                                    X_Length => BPP,
                                    Y_Length => UInt14 (H - 1),
                                    others   => <>),
            Stride              => (S_STRIDE => 0,
                                    D_STRIDE => Dest_Stride),
            Next_CB             => System.Null_Address,
            Reserved_7          => Color,
            others              => <>);
      end if;

      --  Start the transfer
      DMA_Controller.Start_Transfer;

      if Synchronous then
         DMA_Controller.Wait_Transfer;
      end if;
   end Fill_Rect;

   ---------------
   -- Copy_Rect --
   ---------------

   procedure Copy_Rect
     (Src_Buffer  : Bitmap_Buffer'Class;
      X_Src       : Natural;
      Y_Src       : Natural;
      Dst_Buffer  : Bitmap_Buffer;
      X_Dst       : Natural;
      Y_Dst       : Natural;
      Bg_Buffer   : Bitmap_Buffer'Class;
      X_Bg        : Natural;
      Y_Bg        : Natural;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean)
   is
      pragma Unreferenced (X_Bg, Y_Bg);
      Src_Offset : constant Storage_Offset :=
                     Storage_Offset ((X_Src + Y_Src * Src_Buffer.Width) * BPP);
      Src_Stride : constant Integer_16 :=
                     Integer_16 (Src_Buffer.Width - Width) * BPP;
      Dst_Offset : constant Storage_Offset :=
                     Storage_Offset ((X_Dst + Y_Dst * Dst_Buffer.Width) * BPP);
      Dst_Stride : constant Integer_16 :=
                     Integer_16 (Dst_Buffer.Width - Width) * BPP;
      Status     : Boolean;
      Index      : SCB_Index;

   begin
      if Width = 0 or else Height = 0 then
         return;
      end if;

      if Bg_Buffer.Addr /= System.Null_Address then
         raise Constraint_Error with "Not implemented yet.";
      end if;

      loop
         DMA_Controller.Take_Transfer (1, Index, Status);
         exit when Status;
      end loop;

      DMA_SCB (Index) :=
        (TI                  =>
           (Interrupt_Enable => False,
            Two_D_Mode       => True,
            Wait_Response    => True,
            Dest_Inc         => True,
            Dest_Width       => Width_128bit,
            Src_Inc          => True,
            Src_Width        => Width_128bit,
            others           => <>),
         Source_Address      => To_BUS (Src_Buffer.Addr + Src_Offset),
         Destination_Address => To_BUS (Dst_Buffer.Addr + Dst_Offset),
         Transfer_Length     => (TD_Mode  => True,
                                 X_Length => Unsigned_16 (Width * BPP),
                                 Y_Length => UInt14 (Height - 1),
                                 others   => <>),
         Stride              => (S_STRIDE => Src_Stride,
                                 D_STRIDE => Dst_Stride),
         Next_CB             => System.Null_Address,
         others              => <>);

      --  Start the transfer
      DMA_Controller.Start_Transfer;

      if Synchronous then
         DMA_Controller.Wait_Transfer;
      end if;
   end Copy_Rect;

   ---------------
   -- Copy_Rect --
   ---------------

   procedure Copy_Rect
     (Src_Buffer  : Bitmap_Buffer'Class;
      X_Src       : Natural;
      Y_Src       : Natural;
      Dst_Buffer  : Bitmap_Buffer;
      X_Dst       : Natural;
      Y_Dst       : Natural;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean)
   is
      Null_Buffer : Bitmap_Buffer'Class := Src_Buffer;
   begin
      Null_Buffer.Addr := System.Null_Address;
      Null_Buffer.Width := 0;
      Null_Buffer.Height := 0;

      Copy_Rect
        (Src_Buffer  => Src_Buffer,
         X_Src       => X_Src,
         Y_Src       => Y_Src,
         Dst_Buffer  => Bitmap_Buffer'Class (Dst_Buffer),
         X_Dst       => X_Dst,
         Y_Dst       => Y_Dst,
         Bg_Buffer   => Null_Buffer,
         X_Bg        => 0,
         Y_Bg        => 0,
         Width       => Width,
         Height      => Height,
         Synchronous => Synchronous);
   end Copy_Rect;

   ---------------------
   -- Copy_Rect_Blend --
   ---------------------

   procedure Copy_Rect_Blend
     (Src_Buffer  : Bitmap_Buffer;
      X_Src       : Natural;
      Y_Src       : Natural;
      Dst_Buffer  : Bitmap_Buffer'Class;
      X_Dst       : Natural;
      Y_Dst       : Natural;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean)
   is
   begin
      Copy_Rect
        (Src_Buffer  => Bitmap_Buffer'Class (Src_Buffer),
         X_Src       => X_Src,
         Y_Src       => Y_Src,
         Dst_Buffer  => Dst_Buffer,
         X_Dst       => X_Dst,
         Y_Dst       => Y_Dst,
         Bg_Buffer   => Dst_Buffer,
         X_Bg        => X_Dst,
         Y_Bg        => Y_Dst,
         Width       => Width,
         Height      => Height,
         Synchronous => Synchronous);
   end Copy_Rect_Blend;

   ------------------------
   -- Draw_Vertical_Line --
   ------------------------

   procedure Draw_Vertical_Line
     (Buffer : Bitmap_Buffer;
      Color  : Unsigned_32;
      X      : Integer;
      Y      : Integer;
      Height : Integer)
   is
   begin
      Fill_Rect (Bitmap_Buffer'Class (Buffer), Color, X, Y, 1, Height);
   end Draw_Vertical_Line;

   ------------------------
   -- Draw_Vertical_Line --
   ------------------------

   procedure Draw_Vertical_Line
     (Buffer : Bitmap_Buffer;
      Color  : Bitmap_Color;
      X      : Integer;
      Y      : Integer;
      Height : Integer)
   is
   begin
      Fill_Rect (Bitmap_Buffer'Class (Buffer), Color, X, Y, 1, Height);
   end Draw_Vertical_Line;

   --------------------------
   -- Draw_Horizontal_Line --
   --------------------------

   procedure Draw_Horizontal_Line
     (Buffer : Bitmap_Buffer;
      Color  : Unsigned_32;
      X      : Integer;
      Y      : Integer;
      Width  : Integer)
   is
   begin
      Fill_Rect (Bitmap_Buffer'Class (Buffer), Color, X, Y, Width, 1);
   end Draw_Horizontal_Line;

   --------------------------
   -- Draw_Horizontal_Line --
   --------------------------

   procedure Draw_Horizontal_Line
     (Buffer : Bitmap_Buffer;
      Color  : Bitmap_Color;
      X      : Integer;
      Y      : Integer;
      Width  : Integer)
   is
   begin
      Fill_Rect (Bitmap_Buffer'Class (Buffer), Color, X, Y, Width, 1);
   end Draw_Horizontal_Line;

   ---------------
   -- Draw_Rect --
   ---------------

   procedure Draw_Rect
     (Buffer : Bitmap_Buffer;
      Color  : Bitmap_Color;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer)
   is
      W    : Integer renames Width;
      H    : Integer renames Height;

   begin
      Draw_Horizontal_Line (Buffer, Color, X, Y, W);
      Draw_Horizontal_Line (Buffer, Color, X, Y + H - 1, W);
      Draw_Vertical_Line (Buffer, Color, X, Y, H);
      Draw_Vertical_Line (Buffer, Color, X + W - 1, Y, H);
   end Draw_Rect;

   -----------------
   -- Buffer_Size --
   -----------------

   function Buffer_Size (Buffer : Bitmap_Buffer) return Natural
   is
   begin
      return Bits_Per_Pixel (Buffer.Color_Mode) *
        Buffer.Width * Buffer.Height / 8;
   end Buffer_Size;

   --------------------------
   -- Bitmap_Color_To_Unsigned_32 --
   --------------------------

   function Bitmap_Color_To_Unsigned_32
     (Mode : Bitmap_Color_Mode; Col : Bitmap_Color)
      return Unsigned_32
   is
      Ret : Unsigned_32 := 0;

      procedure Add_Unsigned_8
        (Value : Unsigned_8; Pos : Natural; Size : Positive) with Inline;

      function Luminance return Unsigned_8;

      --------------
      -- Add_Unsigned_8 --
      --------------

      procedure Add_Unsigned_8
        (Value : Unsigned_8; Pos : Natural; Size : Positive)
      is
         Val : constant Unsigned_32 :=
                 Shift_Left
                   (Unsigned_32
                      (Shift_Right (Value,
                                    abs (Integer (Size) - 8))),
                    Pos);
      begin
         Ret := Ret or Val;
      end Add_Unsigned_8;

      ---------------
      -- Luminance --
      ---------------

      function Luminance return Unsigned_8
      is
      begin
         return Unsigned_8
           (Shift_Right
              (Unsigned_32 (Col.Red) * 3 +
                   Unsigned_32 (Col.Blue) + Unsigned_32 (Col.Green) * 4,
               3));
      end Luminance;

   begin
      case Mode is
         when ARGB_8888 =>
            Add_Unsigned_8 (Col.Alpha, 24, 8);
            Add_Unsigned_8 (Col.Red,   16, 8);
            Add_Unsigned_8 (Col.Green,  8, 8);
            Add_Unsigned_8 (Col.Blue,   0, 8);

         when RGB_888 =>
            Add_Unsigned_8 (Col.Red,   16, 8);
            Add_Unsigned_8 (Col.Green,  8, 8);
            Add_Unsigned_8 (Col.Blue,   0, 8);

         when RGB_565 =>
            Add_Unsigned_8 (Col.Red,   11, 5);
            Add_Unsigned_8 (Col.Green,  5, 6);
            Add_Unsigned_8 (Col.Blue,   0, 5);

         when ARGB_1555 =>
            Add_Unsigned_8 (Col.Alpha, 15, 1);
            Add_Unsigned_8 (Col.Red,   10, 5);
            Add_Unsigned_8 (Col.Green,  5, 5);
            Add_Unsigned_8 (Col.Blue,   0, 5);

         when ARGB_4444 =>
            Add_Unsigned_8 (Col.Alpha, 12, 4);
            Add_Unsigned_8 (Col.Red,    8, 4);
            Add_Unsigned_8 (Col.Green,  4, 4);
            Add_Unsigned_8 (Col.Blue,   0, 4);

         when L_8 =>
            Add_Unsigned_8 (Luminance, 0, 8);

         when AL_44 =>
            Add_Unsigned_8 (Col.Alpha, 4, 4);
            Add_Unsigned_8 (Luminance, 0, 4);

         when AL_88 =>
            Add_Unsigned_8 (Col.Alpha, 8, 8);
            Add_Unsigned_8 (Luminance, 0, 8);

         when L_4 =>
            Add_Unsigned_8 (Luminance, 0, 4);

         when A_8 =>
            Add_Unsigned_8 (Col.Alpha, 0, 8);

         when A_4 =>
            Add_Unsigned_8 (Col.Alpha, 0, 4);
      end case;

      return Ret;
   end Bitmap_Color_To_Unsigned_32;

   --------------------------
   -- Unsigned_32_To_Bitmap_Color --
   --------------------------

   function Unsigned_32_To_Bitmap_Color
     (Mode : Bitmap_Color_Mode; Col : Unsigned_32)
      return Bitmap_Color
   is

      function Get_Unsigned_8
        (Pos : Natural; Size : Positive) return Unsigned_8 with Inline;

      --------------
      -- Get_Unsigned_8 --
      --------------

      function Get_Unsigned_8
        (Pos : Natural; Size : Positive) return Unsigned_8
      is
         Ret : Unsigned_8;
         Mask : constant Unsigned_32 := Shift_Left (2 ** Size - 1, Pos);
      begin
         Ret := Unsigned_8 (Shift_Right (Col and Mask, Pos));

         if Size = 8 then
            return Ret;
         elsif Size = 1 then
            return (if Ret > 0 then 255 else 0);
         elsif Size >= 4 then
            --  return [7..3] => Ret[4 .. 0], [2 .. 0] => Ret[4 .. 2]
            return Shift_Left (Ret, 8 - Size) or
              Shift_Right (Ret, 2 * Size - 8);
         else
            raise Constraint_Error with "Unsupported color component size";
         end if;
      end Get_Unsigned_8;

      A, R, G, B : Unsigned_8;
   begin
      case Mode is
         when ARGB_8888 =>
            A := Get_Unsigned_8 (24, 8);
            R := Get_Unsigned_8 (16, 8);
            G := Get_Unsigned_8 (8, 8);
            B := Get_Unsigned_8 (0, 8);

         when RGB_888 =>
            A := 255;
            R := Get_Unsigned_8 (16, 8);
            G := Get_Unsigned_8 (8, 8);
            B := Get_Unsigned_8 (0, 8);

         when RGB_565 =>
            A := 255;
            R := Get_Unsigned_8 (11, 5);
            G := Get_Unsigned_8 (5, 6);
            B := Get_Unsigned_8 (0, 5);

         when ARGB_1555 =>
            A := Get_Unsigned_8 (15, 1);
            R := Get_Unsigned_8 (10, 5);
            G := Get_Unsigned_8 (5, 5);
            B := Get_Unsigned_8 (0, 5);

         when ARGB_4444 =>
            A := Get_Unsigned_8 (12, 4);
            R := Get_Unsigned_8 (8, 4);
            G := Get_Unsigned_8 (4, 4);
            B := Get_Unsigned_8 (0, 4);

         when L_8 =>
            A := 255;
            R := Get_Unsigned_8 (0, 8);
            G := R;
            B := R;

         when AL_44 =>
            A := Get_Unsigned_8 (4, 4);
            R := Get_Unsigned_8 (0, 4);
            G := R;
            B := R;

         when AL_88 =>
            A := Get_Unsigned_8 (8, 8);
            R := Get_Unsigned_8 (0, 8);
            G := R;
            B := R;

         when L_4 =>
            A := 255;
            R := Get_Unsigned_8 (0, 4);
            G := R;
            B := R;

         when A_8 =>
            A := Get_Unsigned_8 (0, 8);
            R := 255;
            G := 255;
            B := 255;

         when A_4 =>
            A := Get_Unsigned_8 (0, 4);
            R := 255;
            G := 255;
            B := 255;
      end case;

      return (Alpha => A,
              Red   => R,
              Green => G,
              Blue  => B);
   end Unsigned_32_To_Bitmap_Color;

end Bitmap;
