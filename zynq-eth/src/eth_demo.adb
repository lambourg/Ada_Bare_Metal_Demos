with Ada.Text_IO; use Ada.Text_IO;
with System.Text_IO; use System.Text_IO;
with Eth;

procedure Eth_Demo is
begin
   Put_Line ("Hello ethernet");
   Eth.Init;
   Put ("Press q to reset");
   loop
      Eth.Wait_Packet;

      if Is_Rx_Ready then
         exit when Get = 'q';
      end if;
   end loop;
end Eth_Demo;
