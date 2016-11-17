with HAL.Block_Drivers;

package SDCard_Buf is
   --  Buffer for speed test.  Too large to be on the stack.
   Data : HAL.Block_Drivers.Block (0 .. 256 * 512 - 1);
end SDCard_Buf;
