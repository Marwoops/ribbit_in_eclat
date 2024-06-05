-- code generated from the following source code:
--   ../benchs/examples/mixed_computations.ecl
--
-- with the following command:
--
--    ./eclat ../benchs/examples/mixed_computations.ecl -arg=1;2;3

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.runtime.all;


entity main is
  
  port(signal clk    : in std_logic;
       signal reset  : in std_logic;
       signal run    : in std_logic;
       signal rdy    : out value(0 to 0);
       signal argument : in value(0 to 31);
       signal result : out value(0 to 0));
       
end entity;
architecture rtl of main is

  type t_state is (compute68, \$185_collatz16\, \$222_loop20\);
  signal state: t_state;
  type t_state_var88 is (compute81, \$199_collatz16\, \$202_loop20\);
  signal state_var88: t_state_var88;
  type t_state_var87 is (compute76);
  signal state_var87: t_state_var87;
  type t_state_var86 is (compute72);
  signal state_var86: t_state_var86;
  
  begin
    process(clk)
      variable \$187\ : value(0 to 32) := (others => '0');
      variable \$202_loop20_arg\, \$222_loop20_arg\ : value(0 to 63) := (others => '0');
      variable \$v85\, rdy80, \$v70\, rdy71, \$v83\, rdy75, result66, \$v78\, 
               rdy67, \$v74\, \$v69\, \$v82\ : value(0 to 0) := (others => '0');
      variable result79, \$185_collatz16_arg\, \$195\, \$222_loop20_result\, 
               \$197\, \$190_partial_count\, \$202_loop20_result\, 
               \$199_collatz16_arg\, \$199_collatz16_result\, 
               \$191_full_count\ : value(0 to 31) := (others => '0');
      
    begin
      
      if rising_edge(clk) then
        if (reset = '1') then
          default_zero(\$191_full_count\); default_zero(\$v82\); 
          default_zero(\$v69\); default_zero(\$222_loop20_arg\); 
          default_zero(\$v74\); default_zero(\$187\); 
          default_zero(\$199_collatz16_result\); default_zero(rdy67); 
          default_zero(\$202_loop20_arg\); 
          default_zero(\$199_collatz16_arg\); default_zero(\$v78\); 
          default_zero(\$202_loop20_result\); 
          default_zero(\$190_partial_count\); default_zero(\$197\); 
          default_zero(result66); default_zero(\$222_loop20_result\); 
          default_zero(rdy75); default_zero(\$v83\); default_zero(\$195\); 
          default_zero(rdy71); default_zero(\$v70\); default_zero(rdy80); 
          default_zero(\$185_collatz16_arg\); default_zero(\$v85\); 
          default_zero(result79); 
          rdy <= "1";
          rdy67 := "0";
          state <= compute68;
          state_var88 <= compute81;
          state_var87 <= compute76;
          state_var86 <= compute72;
          
        else if run = '1' then
          case state is
          when \$185_collatz16\ =>
            \$222_loop20_arg\ := \$185_collatz16_arg\ & X"0000000" & X"1";
            state <= \$222_loop20\;
          when \$222_loop20\ =>
            \$v70\ := eclat_eq(\$222_loop20_arg\(0 to 31) & X"0000000" & X"1");
            if \$v70\(0) = '1' then
              \$222_loop20_result\ := \$222_loop20_arg\(32 to 63);
              state <= \$185_collatz16\;
            else
              \$v69\ := eclat_eq(eclat_mod(\$222_loop20_arg\(0 to 31) & X"0000000" & X"2") & X"0000000" & X"0");
              if \$v69\(0) = '1' then
                \$222_loop20_arg\ := eclat_div(\$222_loop20_arg\(0 to 31) & X"0000000" & X"2") & eclat_add(\$222_loop20_arg\(32 to 63) & X"0000000" & X"1");
                state <= \$222_loop20\;
              else
                \$222_loop20_arg\ := eclat_add(eclat_mult(X"0000000" & X"3" & \$222_loop20_arg\(0 to 31)) & X"0000000" & X"1") & eclat_add(\$222_loop20_arg\(32 to 63) & X"0000000" & X"1");
                state <= \$222_loop20\;
              end if;
            end if;
          when compute68 =>
            rdy67 := eclat_false;
            case state_var88 is
            when \$199_collatz16\ =>
              \$202_loop20_arg\ := \$199_collatz16_arg\ & X"0000000" & X"1";
              state_var88 <= \$202_loop20\;
            when \$202_loop20\ =>
              \$v83\ := eclat_eq(\$202_loop20_arg\(0 to 31) & X"0000000" & X"1");
              if \$v83\(0) = '1' then
                \$202_loop20_result\ := \$202_loop20_arg\(32 to 63);
                \$199_collatz16_result\ := \$202_loop20_result\;
                result79 := \$199_collatz16_result\;
                rdy80 := eclat_true;
                state_var88 <= compute81;
              else
                \$v82\ := eclat_eq(eclat_mod(\$202_loop20_arg\(0 to 31) & X"0000000" & X"2") & X"0000000" & X"0");
                if \$v82\(0) = '1' then
                  \$202_loop20_arg\ := eclat_div(\$202_loop20_arg\(0 to 31) & X"0000000" & X"2") & eclat_add(\$202_loop20_arg\(32 to 63) & X"0000000" & X"1");
                  state_var88 <= \$202_loop20\;
                else
                  \$202_loop20_arg\ := eclat_add(eclat_mult(X"0000000" & X"3" & \$202_loop20_arg\(0 to 31)) & X"0000000" & X"1") & eclat_add(\$202_loop20_arg\(32 to 63) & X"0000000" & X"1");
                  state_var88 <= \$202_loop20\;
                end if;
              end if;
            when compute81 =>
              rdy80 := eclat_false;
              \$199_collatz16_arg\ := argument;
              state_var88 <= \$199_collatz16\;
            end case;
            \$v85\ := eclat_not(rdy80);
            if \$v85\(0) = '1' then
              result79 := X"0000000" & X"0";
            end if;
            \$187\ := result79 & rdy80;
            \$v78\ := eclat_not(rdy75);
            if \$v78\(0) = '1' then
              \$197\ := X"0000000" & X"0";
            end if;
            case state_var87 is
            when compute76 =>
              rdy75 := eclat_false;
              \$197\ := eclat_if(""&\$187\(32) & eclat_add(\$197\ & X"0000000" & X"1") & \$197\);
              rdy75 := eclat_true;
              state_var87 <= compute76;
            end case;
            \$197\ := \$197\;
            \$190_partial_count\ := \$197\;
            \$v74\ := eclat_not(rdy71);
            if \$v74\(0) = '1' then
              \$195\ := X"0000000" & X"0";
            end if;
            case state_var86 is
            when compute72 =>
              rdy71 := eclat_false;
              \$195\ := eclat_if(eclat_true & eclat_add(\$195\ & X"0000000" & X"1") & \$195\);
              rdy71 := eclat_true;
              state_var86 <= compute72;
            end case;
            \$195\ := \$195\;
            \$191_full_count\ := \$195\;
            eclat_print_int(\$190_partial_count\);
            
            eclat_print_string(of_string(" / "));
            
            eclat_print_int(\$191_full_count\);
            
            result66 := eclat_unit;
            eclat_print_newline(eclat_unit);
            
            rdy67 := eclat_true;
            state <= compute68;
          end case;
          
          result <= result66;
          rdy <= rdy67;
          
        end if;
      end if;
    end if;
  end process;
end architecture;
