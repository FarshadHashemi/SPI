Library IEEE ;
Use IEEE.STD_Logic_1164.All ;
Use IEEE.Numeric_STD.All ;

Entity SPI_Slave Is

   Generic(
      SPI_Mode  : UNSIGNED(1 Downto 0) := To_Unsigned(3,2) ;
      Word_Size : Integer              := 8
   ) ;

   Port(
      Clock               : In  STD_LOGIC ;
      Synchronous_Reset   : In  STD_LOGIC ;
   -- User Interface
      Input_Data          : In  STD_Logic_Vector(Word_Size-1 Downto 0) ;
      Available_Input     : In  STD_LOGIC ;
      Output_Data         : Out STD_Logic_Vector(Word_Size-1 Downto 0) ;
      Valid_Output        : Out STD_Logic ;
      Busy                : Out STD_Logic ;
   -- %%%%%%%%%%%%%%

   -- SPI Interface
      Serial_Clock        : In  STD_Logic ;
      Master_Out_Slave_In : In  STD_Logic ;
      Master_In_Slave_Out : Out STD_Logic ;
      Slave_Select        : In  STD_Logic
   -- %%%%%%%%%%%%%
   ) ;

End SPI_Slave ;

Architecture Behavioral Of SPI_Slave Is

   Signal Synchronous_Reset_Register   : STD_Logic                              := '0' ;
   Signal Input_Data_Register          : STD_Logic_Vector(Word_Size-1 Downto 0) := (Others=>'0') ;
   Signal Available_Input_Register     : STD_Logic                              := '0' ;
   Signal Output_Data_Register         : STD_Logic_Vector(Word_Size-1 Downto 0) := (Others=>'0') ;
   Signal Valid_Output_Register        : STD_Logic                              := '0' ;
   Signal Busy_Register                : STD_Logic                              := '0' ;
   Signal Serial_Clock_Register        : STD_Logic                              := STD_LOGIC(SPI_Mode(1)) ;
   Signal Master_Out_Slave_In_Register : STD_Logic                              := '0' ;
   Signal Master_In_Slave_Out_Register : STD_Logic                              := 'Z' ;
   Signal Slave_Select_Register        : STD_Logic                              := '1' ;

   Signal Serial_Clock_1_Delay         : STD_Logic                              := STD_LOGIC(SPI_Mode(1)) ;
   Signal Slave_Select_1_Delay         : STD_Logic                              := '1' ;

   Signal Data                         : STD_Logic_Vector(Word_Size-1 Downto 0) := (Others=>'Z') ;

Begin

   Process(Clock)
   Begin

      If Rising_Edge(Clock) Then

      -- Registering Input Ports
         Synchronous_Reset_Register   <= Synchronous_Reset ;
         Input_Data_Register          <= Input_Data ;
         Available_Input_Register     <= Available_Input ;

         Master_Out_Slave_In_Register <= Master_Out_Slave_In ;
         Slave_Select_Register        <= Slave_Select ; 
         Serial_Clock_Register        <= Serial_Clock ;
      -- %%%%%%%%%%%%%%%%%%%%%%%

      -- Reset
         If Synchronous_Reset_Register='1' Then

            Serial_Clock_1_Delay          <= STD_LOGIC(SPI_Mode(1)) ;
            Slave_Select_1_Delay          <= '1' ;
            Data                          <= (Others=>'Z') ;
            Output_Data_Register          <= (Others=>'0') ;
            Valid_Output_Register         <= '0' ;
            Busy_Register                 <= '0' ;
            Master_In_Slave_Out_Register  <= 'Z' ;
      -- %%%%%

         Else

            Slave_Select_1_Delay <= Slave_Select_Register ;
            Serial_Clock_1_Delay <= Serial_Clock_Register ;

            Valid_Output_Register <= '0' ;
            Busy_Register         <= '0' ;

            If Slave_Select_Register='0' And Slave_Select_1_Delay='1' And Available_Input_Register='1' Then
               Data <= Input_Data_Register ;
            End If;

         -- Send And Receive
            If Slave_Select_Register='0' Then

               Busy_Register <= '1' ;

            -- SPI_Mode=0
               If SPI_Mode=To_Unsigned(0,2) Then

                  Master_In_Slave_Out_Register <= Data(Word_Size-1) ;
                  If Serial_Clock_1_Delay='0' And Serial_Clock_Register='1' Then    -- Sampling In Rising_Edge(Serial_Clock)
                     Output_Data_Register <= Output_Data_Register(Word_Size-2 Downto 0) & Master_Out_Slave_In_Register ;
                  Elsif Serial_Clock_1_Delay='1' And Serial_Clock_Register='0' Then -- Send Next Bit In Falling_Edge(Serial_Clock)
                     Data <= Data(Word_Size-2 Downto 0) & 'Z' ; 
                  End If ;
            -- %%%%%%%%%%

            -- SPI_Mode=1
               Elsif SPI_Mode=To_Unsigned(1,2) Then

                  If Serial_Clock_1_Delay='1' And Serial_Clock_Register='0' Then    -- Sampling In Falling_Edge(Serial_Clock)
                     Output_Data_Register <= Output_Data_Register(Word_Size-2 Downto 0) & Master_Out_Slave_In_Register ;
                  Elsif Serial_Clock_1_Delay='0' And Serial_Clock_Register='1' Then -- Send Next Bit In Rising_Edge(Serial_Clock)
                     Data <= Data(Word_Size-2 Downto 0) & 'Z' ;
                     Master_In_Slave_Out_Register <= Data(Word_Size-1) ;
                  End If ;
            -- %%%%%%%%%%

            -- SPI_Mode=2
               Elsif SPI_Mode=To_Unsigned(2,2) Then

                  Master_In_Slave_Out_Register <= Data(Word_Size-1) ;
                  If Serial_Clock_1_Delay='1' And Serial_Clock_Register='0' Then    -- Sampling In Falling_Edge(Serial_Clock)
                     Output_Data_Register <= Output_Data_Register(Word_Size-2 Downto 0) & Master_Out_Slave_In_Register ;
                  Elsif Serial_Clock_1_Delay='0' And Serial_Clock_Register='1' Then -- Send Next Bit In Rising_Edge(Serial_Clock)
                     Data <= Data(Word_Size-2 Downto 0) & 'Z' ; 
                  End If ;
            -- %%%%%%%%%%

            -- SPI_Mode=3
               Elsif SPI_Mode=To_Unsigned(3,2) Then

                  If Serial_Clock_1_Delay='0' And Serial_Clock_Register='1' Then    -- Sampling In Rising_Edge(Serial_Clock)
                     Output_Data_Register <= Output_Data_Register(Word_Size-2 Downto 0) & Master_Out_Slave_In_Register ;
                  Elsif Serial_Clock_1_Delay='1' And Serial_Clock_Register='0' Then -- Send Next Bit In Falling_Edge(Serial_Clock)
                     Data <= Data(Word_Size-2 Downto 0) & 'Z' ;
                     Master_In_Slave_Out_Register <= Data(Word_Size-1) ;
                  End If ;
            -- %%%%%%%%%%

            End If;

            Else
               Master_In_Slave_Out_Register <= 'Z' ;
            End If ;
         -- %%%%%%%%%%%%%%%%

         -- End Receive
            If Slave_Select_Register='1' And Slave_Select_1_Delay='0' Then
               Valid_Output_Register <= '1' ;
            End If;
         -- %%%%%%%%%%%

         End If ;

      End If ;

   End Process ;

-- Registering Output Ports
   Output_Data         <= Output_Data_Register ;
   Valid_Output        <= Valid_Output_Register ;
   Busy                <= Busy_Register ;

   Master_In_Slave_Out <= Master_In_Slave_Out_Register ;
-- %%%%%%%%%%%%%%%%%%%%%%%%

End Behavioral ;