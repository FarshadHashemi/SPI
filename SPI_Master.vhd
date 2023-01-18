Library IEEE ;
Use IEEE.STD_Logic_1164.All ;
Use IEEE.Numeric_STD.All ;
Use IEEE.Math_Real.All ;

Entity SPI_Master Is

   Generic(
      SPI_Mode               : UNSIGNED(1 Downto 0) := To_Unsigned(3,2) ;
      Number_Of_Slaves       : Integer              := 8 ;
      Word_Size              : Integer              := 8 ;
      Clock_Frequency        : Integer              := 100 ; -- (MHz)
      Serial_Clock_Frequency : Integer              := 1000 -- (KHz)
   ) ;

   Port(
      Clock               : In  STD_LOGIC ;
      Synchronous_Reset   : In  STD_LOGIC ;
   -- User Interface
      Input_Data          : In  STD_Logic_Vector(Word_Size-1 Downto 0) ;
      Slave_Address       : In  Unsigned(((Integer(Ceil(Log2(Real(Word_Size)))))-1) Downto 0) ;
      Available_Input     : In  STD_LOGIC ;
      Output_Data         : Out STD_Logic_Vector(Word_Size-1 Downto 0) ;
      Valid_Output        : Out STD_Logic ;
      Busy                : Out STD_Logic ;
   -- %%%%%%%%%%%%%%

   -- SPI Interface
      Serial_Clock        : Out STD_Logic ;
      Master_Out_Slave_In : Out STD_Logic ;
      Master_In_Slave_Out : In  STD_Logic ;
      Slave_Select        : Out STD_Logic_Vector(Number_Of_Slaves-1 Downto 0)
   -- %%%%%%%%%%%%%
   ) ;

End SPI_Master ;

Architecture Behavioral Of SPI_Master Is

   Signal Synchronous_Reset_Register   : STD_Logic                                                                     := '0' ;
   Signal Input_Data_Register          : STD_Logic_Vector(Word_Size-1 Downto 0)                                        := (Others=>'0') ;
   Signal Slave_Address_Register       : Unsigned(((Integer(Ceil(Log2(Real(Number_Of_Slaves)))))-1) Downto 0)          := (Others=>'0') ;
   Signal Available_Input_Register     : STD_Logic                                                                     := '0' ;
   Signal Output_Data_Register         : STD_Logic_Vector(Word_Size-1 Downto 0)                                        := (Others=>'0') ;
   Signal Valid_Output_Register        : STD_Logic                                                                     := '0' ;
   Signal Busy_Register                : STD_Logic                                                                     := '0' ;
   Signal Serial_Clock_Register        : STD_Logic                                                                     := STD_LOGIC(SPI_Mode(1)) ;
   Signal Master_Out_Slave_In_Register : STD_Logic                                                                     := '0' ;
   Signal Master_In_Slave_Out_Register : STD_Logic                                                                     := '0' ;
   Signal Slave_Select_Register        : STD_Logic_Vector(Number_Of_Slaves-1 Downto 0)                                 := (Others=>'1') ;

   Signal Serial_Clock_1_Delay         : STD_Logic                                                                     := STD_LOGIC(SPI_Mode(1)) ;

   Signal Data                         : STD_Logic_Vector(Word_Size-1 Downto 0)                                        := (Others=>'0') ;

   Signal Time_Counter                 : Integer Range 0 To (((Clock_Frequency*(10**3))/(2*Serial_Clock_Frequency))-1) := 0 ;
   Signal Clock_Counter                : Integer Range 0 To (2*Word_Size)                                              := 0 ;

Begin

   Process(Clock)
   Begin

      If Rising_Edge(Clock) Then

      -- Registering Input Ports
         Synchronous_Reset_Register   <= Synchronous_Reset ;
         Input_Data_Register          <= Input_Data ;
         Slave_Address_Register       <= Slave_Address ;
         Available_Input_Register     <= Available_Input ;

         Master_In_Slave_Out_Register <= Master_In_Slave_Out ;
      -- %%%%%%%%%%%%%%%%%%%%%%%

      -- Reset
         If Synchronous_Reset_Register='1' Then

            Serial_Clock_1_Delay           <= STD_LOGIC(SPI_Mode(1)) ;
            Data                           <= (Others=>'0') ;
            Time_Counter                   <= 0 ;
            Clock_Counter                  <= 0 ;
            Output_Data_Register           <= (Others=>'0') ;
            Valid_Output_Register          <= '0' ;
            Busy_Register                  <= '0' ;
            Serial_Clock_Register          <= STD_LOGIC(SPI_Mode(1)) ;
            Master_Out_Slave_In_Register   <= '0' ;
            Slave_Select_Register          <= (Others=>'1') ;
      -- %%%%%

         Else

            Valid_Output_Register <= '0' ;

         -- Wait For New Connection
            If Busy_Register='0' And Available_Input_Register='1' Then
               Data          <= Input_Data_Register ;
               Busy_Register <= '1' ;
               Slave_Select_Register(To_Integer(Slave_Address_Register)) <= '0' ;
            End If ;
         -- %%%%%%%%%%%%%%%%%%%%%%%

         -- Connection Is Established
            If Busy_Register='1' Then

            -- Timing
               Time_Counter             <= Time_Counter + 1 ;
               If Time_Counter=(((Clock_Frequency*(10**3))/(2*Serial_Clock_Frequency))-1) Then
                  Time_Counter          <= 0 ;
						Serial_Clock_Register <= Not Serial_Clock_Register ;
                  Clock_Counter         <= Clock_Counter + 1 ;

               -- End Connection
                  If Clock_Counter=(2*Word_Size)Then
                     Clock_Counter         <= 0 ;
                     Serial_Clock_Register <= STD_LOGIC(SPI_Mode(1)) ;
                     Valid_Output_Register <= '1' ;
                     Busy_Register         <= '0' ;
                     Slave_Select_Register(To_Integer(Slave_Address_Register)) <= '1' ;
                  End If ;
               -- %%%%%%%%%%%%%%

               End If ;
            -- %%%%%%

               Serial_Clock_1_Delay <= Serial_Clock_Register ;

            -- Send And Receive
            -- SPI_Mode=0
               If SPI_Mode=To_Unsigned(0,2) Then
                  Master_Out_Slave_In_Register <= Data(Word_Size-1) ;
                  If Serial_Clock_1_Delay='1' And Serial_Clock_Register='0' Then    -- Send Next Bit In Falling_Edge(Serial_Clock)
                     Data                      <= Data(Word_Size-2 Downto 0) & '0' ;
                  Elsif Serial_Clock_1_Delay='0' And Serial_Clock_Register='1' Then -- Sampling In Rising_Edge(Serial_Clock)
                     Output_Data_Register      <= Output_Data_Register(Word_Size-2 Downto 0) & Master_In_Slave_Out_Register ;
                  End If ;
            -- %%%%%%%%%%

            -- SPI_Mode=1
               Elsif SPI_Mode=To_Unsigned(1,2) Then
                  If Serial_Clock_1_Delay='0' And Serial_Clock_Register='1' Then    -- Send Next Bit In Rising_Edge(Serial_Clock)
                     Data                         <= Data(Word_Size-2 Downto 0) & '0' ;
                     Master_Out_Slave_In_Register <= Data(Word_Size-1) ;
                  Elsif Serial_Clock_1_Delay='1' And Serial_Clock_Register='0' Then -- Sampling In Falling_Edge(Serial_Clock)
                     Output_Data_Register         <= Output_Data_Register(Word_Size-2 Downto 0) & Master_In_Slave_Out_Register ;
                  End If ;
            -- %%%%%%%%%%

            -- SPI_Mode=2
               Elsif SPI_Mode=To_Unsigned(2,2) Then
                  Master_Out_Slave_In_Register <= Data(Word_Size-1) ;
                  If Serial_Clock_1_Delay='0' And Serial_Clock_Register='1' Then    -- Send Next Bit In Rising_Edge(Serial_Clock)
                     Data                      <= Data(Word_Size-2 Downto 0) & '0' ;
                  Elsif Serial_Clock_1_Delay='1' And Serial_Clock_Register='0' Then -- Sampling In Falling_Edge(Serial_Clock)
                     Output_Data_Register      <= Output_Data_Register(Word_Size-2 Downto 0) & Master_In_Slave_Out_Register ;
                  End If ;
            -- %%%%%%%%%%

            -- SPI_Mode=3
               Elsif SPI_Mode=To_Unsigned(3,2) Then
                  If Serial_Clock_1_Delay='1' And Serial_Clock_Register='0' Then    -- Send Next Bit In Falling_Edge(Serial_Clock)
                     Data                         <= Data(Word_Size-2 Downto 0) & '0' ;
                     Master_Out_Slave_In_Register <= Data(Word_Size-1) ;
                  Elsif Serial_Clock_1_Delay='0' And Serial_Clock_Register='1' Then -- Sampling In Rising_Edge(Serial_Clock)
                     Output_Data_Register         <= Output_Data_Register(Word_Size-2 Downto 0) & Master_In_Slave_Out_Register ;
                  End If ;
            -- %%%%%%%%%%

               End If ;
            -- %%%%%%%%%%%%%%%%

            End If ;
         -- %%%%%%%%%%%%%%%%%%%%%%%%%

         End If ;

      End If ;

   End Process ;

-- Registering Output Ports
   Output_Data         <= Output_Data_Register ;
   Valid_Output        <= Valid_Output_Register ;
   Busy                <= Busy_Register ;
   
   Serial_Clock        <= Serial_Clock_Register ;
   Master_Out_Slave_In <= Master_Out_Slave_In_Register ;
   Slave_Select        <= Slave_Select_Register ;
-- %%%%%%%%%%%%%%%%%%%%%%%%

End Behavioral ;