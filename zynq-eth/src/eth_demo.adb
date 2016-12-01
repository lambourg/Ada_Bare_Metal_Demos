with Ada.Text_IO; use Ada.Text_IO;
with Eth;

procedure Eth_Demo is
begin
   Put_Line ("Hello ethernet");
   Eth.Init;
   Put ("Press q to reset");
   loop
      declare
         C : Character;
      begin
         Get (C);
         exit when C = 'q';
      end;
   end loop;
end Eth_Demo;
