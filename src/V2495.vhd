-- V2495.vhd
-- -----------------------------------------------------------------------
-- V2495 User Template (top level)
-- -----------------------------------------------------------------------
--  Date        : 08/06/2016
--  Contact     : support.nuclear@caen.it
-- (c) CAEN SpA - http://www.caen.it   
-- -----------------------------------------------------------------------
--
--                   
--------------------------------------------------------------------------------
-- $Id$ 
--------------------------------------------------------------------------------

library IEEE;
    use IEEE.std_logic_1164.all;
    use IEEE.numeric_std.all;

    use work.V2495_pkg.all;

-- ----------------------------------------------
entity V2495 is
-- ----------------------------------------------
    port (

        CLK    : in     std_logic;                         -- System clock 
                                                           -- (50 MHz)

    -- ------------------------------------------------------
    -- Mainboard I/O ports
    -- ------------------------------------------------------   
      -- Port A : 32-bit LVDS/ECL input
         A        : in    std_logic_vector (31 DOWNTO 0);  -- Data bus 
      -- Port B : 32-bit LVDS/ECL input                    
         B        : in    std_logic_vector (31 DOWNTO 0);  -- Data bus
      -- Port C : 32-bit LVDS output                       
         C        : out   std_logic_vector (31 DOWNTO 0);  -- Data bus
      -- Port G : 2 NIM/TTL input/output                   
         GIN      : in    std_logic_vector ( 1 DOWNTO 0);  -- In data
         GOUT     : out   std_logic_vector ( 1 DOWNTO 0);  -- Out data
         SELG     : out   std_logic;                       -- Level select
         nOEG     : out   std_logic;                       -- Output Enable

    -- ------------------------------------------------------
    -- Expansion slots
    -- ------------------------------------------------------                                                                  
      -- PORT D Expansion control signals                  
         IDD      : in    std_logic_vector ( 2 DOWNTO 0);  -- Card ID
         SELD     : out   std_logic;                       -- Level select
         nOED     : out   std_logic;                       -- Output Enable
         D        : inout std_logic_vector (31 DOWNTO 0);  -- Data bus
                                                           
      -- PORT E Expansion control signals                  
         IDE      : in    std_logic_vector ( 2 DOWNTO 0);  -- Card ID
         SELE     : out   std_logic;                       -- Level select
         nOEE     : out   std_logic;                       -- Output Enable
         E        : inout std_logic_vector (31 DOWNTO 0);  -- Data bus
                                                           
      -- PORT F Expansion control signals                  
         IDF      : in    std_logic_vector ( 2 DOWNTO 0);  -- Card ID
         SELF     : out   std_logic;                       -- Level select
         nOEF     : out   std_logic;                       -- Output Enable
         F        : inout std_logic_vector (31 DOWNTO 0);  -- Data bus

    -- ------------------------------------------------------
    -- Gate & Delay
    -- ------------------------------------------------------
      --G&D I/O
        GD_START   : out  std_logic_vector(31 downto 0);   -- Start of G&D
        GD_DELAYED : in   std_logic_vector(31 downto 0);   -- G&D Output
      --G&D SPI bus                                        
        SPI_MISO   : in   std_logic;                       -- SPI data in
        SPI_SCLK   : out  std_logic;                       -- SPI clock
        SPI_CS     : out  std_logic;                       -- SPI chip sel.
        SPI_MOSI   : out  std_logic;                       -- SPI data out
      
    -- ------------------------------------------------------
    -- LED
    -- ------------------------------------------------------
        LED        : out std_logic_vector(7 downto 0);     -- User led    
    
    -- ------------------------------------------------------
    -- Local Bus in/out signals
    -- ------------------------------------------------------
      -- Communication interface
        nLBRES     : in     std_logic;                     -- Bus reset
        nBLAST     : in     std_logic;                     -- Last cycle
        WnR        : in     std_logic;                     -- Read (0)/Write(1)
        nADS       : in     std_logic;                     -- Address strobe
        nREADY     : out    std_logic;                     -- Ready (active low) 
        LAD        : inout  std_logic_vector (15 DOWNTO 0);-- Address/Data bus
      -- Interrupt requests  
        nINT       : out    std_logic                      -- Interrupt request
  );
end V2495;

-- ---------------------------------------------------------------
architecture rtl of V2495 is
-- ---------------------------------------------------------------

    signal mon_regs    : MONITOR_REGS_T;
    signal ctrl_regs   : CONTROL_REGS_T;

    -- Gate & Delay control bus signals
    signal gd_write     :  std_logic;
    signal gd_read      :  std_logic;
    signal gd_ready     :  std_logic;
    signal reset        :  std_logic;
    signal gd_data_wr   :  std_logic_vector(31 downto 0);
    signal gd_data_rd   :  std_logic_vector(31 downto 0);
    signal gd_command   :  std_logic_vector(15 downto 0);
	 signal spill			:  std_logic := '0';
	 signal ext_clk		:	std_logic := '0';
	 signal async_spill  :  std_logic := '0';
          
-----\
begin --
-----/			
	 
    -- Unused output ports are explicitally set to HiZ 
    -- ----------------------------------------------------
    SELD <= 'Z';
    nOED <= 'Z';
    D    <= (others => 'Z');
	 
	 -- Setting of G ports (G0 output, G1 input)
	 -- nOEG must be set to 0 for mixed input/output
	 -- GOUT(NGinput) must be also set to 0
	 SELG    <= '0';
	 nOEG		<= '0';
	 GOUT(1)	<= '0';
	 GOUT(0)	<= '1';
    
	 nim_ttl_sel: process(clk)
	 begin
		if rising_edge(clk) then
			if(GIN(1) = '0') then	--LEMO connected from G0 to G1
				--Set ports E/F to TTL => '1'
				SELE <= '1';
				SELF <= '1';
				-- All LEDs ON TTL mode
				LED <= "11111111";
			else 					--default, no LEMO attached
				--Set ports E/F to NIM => '1'
				SELE <= '0';
				SELF <= '0';
				-- All LEDs OFF NIM mode
				LED <= "00000000";
			end if;	
		end if;	
	 end process;

    -- Setting boards in input (0) or output mode (1)
    nOEE <= '0';
    nOEF <= '1';
    
    GD_START <= (others => 'Z');
    
    -- Local bus Interrupt request
    nINT <= '1';
    
    
    reset <= not(nLBRES);

    -- Routing NIM to LVDS and viceversa following CAEN specifications
    -- LVDS to NIM (using channels 0 to 7 from LVDS slot B as input)
    --E(0)    <= B(0); --B(0) is used now for clock inputs and latch on SPILL
    E(16)   <= B(1);
    E(1)    <= B(2);
    E(17)   <= B(3);
    E(12)   <= B(4);
    E(28)   <= B(5);
    E(13)   <= B(6);
    E(29)   <= B(7);
    
	 --	 Last LEMO channel on the left on board C used for ext clock
	 -- 	 Only spill is made synchronous to ext clock
	 ext_clk    <= B(0); -- was F(31) as last lemo channel, but testing with direct LVDS signal due to speed issues	 
	 
    -- NIM to LVDS (First 10 LVDS channels are used for the spill)
	 -- Breakout board 1 contains only 8

	 
	 spill_set: process(ext_clk)
	 begin
		if(rising_edge(ext_clk)) then
			if (F(2) = '0') then
				spill <= '1';
			else
				spill <= '0';
			end if;	
		end if;
	 end process;
	 
	 with F(2) select async_spill <=
		'1' when '0',
		'0' when others;
	 
	 C(0)    <= spill ; 
	 C(1)    <= spill ;
	 C(2)    <= spill ;
	 C(3)    <= spill ;
	 C(4)    <= spill ;
	 C(5)    <= spill ;
	 C(6)    <= spill ;
	 C(7)    <= spill ;
	 
	 --Breakout board 2 contains 4 spill signals + 4 triggers
	 C(16)    <= spill ;
	 C(17)    <= spill ;
    C(18)    <= spill ;
	 C(19)    <= spill ;
	 -- Here the triggers are routed to the LVDS channels
  
	 with F(18) select C(20) <=
		'1' when '1',
		'0' when others;
		
	 with F(3) select C(21) <=
		'1' when '1',
		'0' when others;
		
	 with F(19) select C(22) <=
		'1' when '1',
		'0' when others;
	 
	 with F(14) select C(23) <=
		'1' when '1',
		'0' when others;

--  C(24) LVDS output is now the async SPILL signal

	 C(24) <= async_spill;
		
--	   C(18)    <= F(18);
--    C(19)    <= F(3) ;
--    C(20)    <= F(19);
--    C(21)    <= F(14);
--    C(22)    <= F(30);
--    C(23)    <= F(15);

    -- --------------------------
    --  Local Bus slave interface
    -- --------------------------  
    I_LBUS_INTERFACE: entity work.lb_int  
        port map (
            clk         => CLK,   
            reset       => reset,
            -- Local Bus            
            nBLAST      => nBLAST,   
            WnR         => WnR,      
            nADS        => nADS,     
            nREADY      => nREADY,   
            LAD         => LAD,
            -- Register interface  
            ctrl_regs   => ctrl_regs,
            mon_regs    => mon_regs,      
            -- Gate and Delay controls
            gd_data_wr  => gd_data_wr,       
            gd_data_rd  => gd_data_rd,         
            gd_command  => gd_command,
            gd_write    => gd_write,
            gd_read     => gd_read,
            gd_ready    => gd_ready
        );
        
    -- --------------------------
    --  Gate and Delay controller
    -- --------------------------  
    I_GD: entity  work.gd_control
        port map  (
            reset       => reset,
            clk         => clk,                
            -- Programming interface
            write       => gd_write,
            read        => gd_read,
            writedata   => gd_data_wr,
            command     => gd_command,
            ready       => gd_ready,
            readdata    => gd_data_rd,  
            -- Gate&Delay control interface (SPI)        
            spi_sclk    => spi_sclk,
            spi_cs      => spi_cs,  
            spi_mosi    => spi_mosi,
            spi_miso    => spi_miso    
        );

end rtl;
   