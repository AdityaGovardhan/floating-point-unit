/*

-------------------------------------------------------------------------------
-- 
-- Copyright (C) 2013 Indian Institute of Technology Madras (IITM)
-- All rights reserved.
-- 
-- This source file may be used and distributed without
-- restriction provided that this copyright statement is not
-- removed from the file and that any derivative work contains
-- the original copyright notice and the associated disclaimer.
-- 
-- This source file is free software; you can redistribute it
-- and/or modify it under the terms of the GNU General
-- Public License as published by the Free Software Foundation;
-- either version 3.0 of the License, or (at your option) any
-- later version.
-- 
-- 
-- You should have received a copy of the GNU General
-- Public License along with this source;
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER ``AS IS'' AND ANY EXPRESS
-- OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
-- OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
-- NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
-- THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
-- The views and conclusions contained in the software and documentation are
-- those of the authors and should not be interpreted as representing official
-- policies, either expressed or implied, of the copyright holder.
-- 
-------------------------------------------------------------------------------



Module Name : Single Precision Floating Point Multiplication unit
Author Name: Neel Gala, Arjun C. Menon, Vinod.G, Aditya Govardhan
Email ID: neelgala@gmail.com, c.arjunmenon@gmail.com, g.vinod1993@gmail.com,dtgovardhan@gmail.com
Last updated on : 17th June, 2016


This unit carries out the multiplication of two normal floating point numbers. A pipelined architecture is
used implement this module. The algorithm strictly follows the IEEE 754 standard. This unit takes 10
clock cycles to compute the final value. This means there are totally 10 buffers which store the state of the module.
 These include the input register, the ff_output register and 8 intermediate registers. After the
result is computed, the ready signal is asserted indicating that the result is valid and also the exception
flags are raised if any exception has been generated.
The intermediate registers are implemented using the FIFO libraries of the Azure library.
Multiplication of floating point number requires integer multiplication of the mantissas. This is
achieved by using the integer multiplier module as discussed earlier. This module takes 4 clock cycle to
compute the value.

The inputs "rob_number" and "program_counter" do not affect the working of the multiplier.
They have been kept for use when integrating this code with that of a microprocessor.
Also specific care has been taken to ensure that the ff_output register holds the value of the result
until it is read by the top module. This may lead to stalling the pipeline at certain instance. Thus
once the top module reads the ff_output register value through the method result it then asserts
the method deque_buffer_reset_ready_signal . This method empties the ff_output register and thus the
pipeline continues execution.
Multiplication of floating point number can create the following exceptions (mentioned is
decreasing priority order): Invalid, Overflow, Underflow and Inexact. Based on these exceptions, the result and also
the combinations of the inputs appropriate flags (overflow, invalid, underflow, etc) in the fsr are also
set.

The following sites have been used to verify the obtained results: 
1. http://www.h-schmidt.net/FloatConverter/
2. http://www.binaryconvert.com/convert_float.html
3. http://keisan.casio.com/calculator
*/
package fpu_dpfloating_multiplier;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
import riscv_types::*;
import RegFile::*;
import integermultiplier_for_dpfpu::*;


interface Ifc_fpu_dpfloating_multiplier;
	/*Input Methods */
	method Action _start(Bit#(64) _operand1,Bit#(64) _operand2,Bit#(5) _destination, Bit#(4) _rob_number, Bit#(32) _fsr, Bit#(3) rounding_mode, Bit#(32) _program_counter);
	method Action _deque_buffer_reset_ready_signal();
	method Action _set_flush(Bool _flush);
	/* Output Methods */
	method Output_type result_multiplication_();
	method Bool ready_();
endinterface:Ifc_fpu_dpfloating_multiplier

typedef struct{
	Bit#(5) destination;	// holds the destination address of the operation in the registerfile
	Bit#(4) rob;		// holds the rob_number alloted to the operations during issue stage.
	Bit#(32) fsr;		// the floating-point status register containing all the flags and control bits for dynamic rounding.
    Bit#(3) rounding_mode;  // static rounding mode encoded in the instruction
	Bit#(32) program_counter;// the program_counter value of the instruction.
	Bit#(13) summed_exponent;// exponent of the resultant
	bit sign; 		// sign bit of the result
	Bit#(1) infinity;	// indicating that the ff_output is infinity.
	Bit#(1) invalid;	// indicating that the ff_output is NaN.
	Bit#(1) zero;		// indicating that the ff_output is zero.
	Bit#(53) mantissa1; 	// mantissa of operand1
	Bit#(53) mantissa2; 	// mantissa of operand2
	}Input_data_type deriving (Bits,Eq);


(*synthesize*)
module mkfpu_dpfloating_multiplier(Ifc_fpu_dpfloating_multiplier);

Wire#(Bool) wr_flush <-mkDWire(False); // wire to indicate that a wr_flush has occured in the processor and all buffer need to be cleared.

FIFO#(Input_data_type) ff_input_register<-mkPipelineFIFO();	// input FIFO
FIFOF#(Output_type) output_<-mkFIFOF(); 	// ff_output FIFO which holds the final result. This is of type FIFOF.

Ifc_integer_multiplier_for_dpfmul integer_multiplier<-mkinteger_multiplier_for_dpfmul; //Instance of the 4-Cylce Integer Multiplier.


// this rule is used to clear all the buffers/signals when the top module initiates a FLUSH 
rule rl_flush_all_fifos(wr_flush);
	// ff_stage3.clear();
	ff_input_register.clear();
	output_.clear();
	integer_multiplier._set_flush(True);
endrule:rl_flush_all_fifos

/*
this rule will transfer data to the integer multiplier from the data recieved from the input register.
since the integer multiplier is a 32 bit multiplier and the mantissas are only 24 bits wide, 8 zeroz
are appended at the MSB of each mantissa and then sent into the integer multiplier.
since the input of the integer mulitplier is a FIFO, this rule can fire only when the input FIFO of the
integer multiplier is empty. else it will not fire.
all data like exponent, excpetion falgs etc. provided by the input register are also transfered to the integer
multiplier. these datas are simply buffered along the stages of the integer multiplier and are not used for any 
computation.
*/
rule rl_stage1_after_input_stage(!wr_flush);
	//$display("Executing FMUL stage 2");
	//$display("exp= %b", ff_input_register.first().summed_exponent);
	integer_multiplier._start({11'b0,ff_input_register.first().mantissa1}, {11'b0,ff_input_register.first().mantissa2},ff_input_register.first().destination,ff_input_register.first().program_counter, ff_input_register.first().fsr, ff_input_register.first().rounding_mode, ff_input_register.first().rob, ff_input_register.first().sign, ff_input_register.first().summed_exponent, ff_input_register.first().invalid,ff_input_register.first().infinity,ff_input_register.first().zero);
	
	ff_input_register.deq();	
endrule:rl_stage1_after_input_stage

/*
in this rule the number of leading zeros are counted and stored in the register rg_leftzeros.
also the final exponent is incremented by 1 if the result from the integer mulitplier has set the carry bit.


this is the final stage of the operation where the ff_output is decided based on the exception flags
which where generate in the previous stages.
also the new fsr is generated based no the new flags that are set.
in case no exception is generated and all the flags are set to 0 then the ff_output is the same as that generated
in the previous stage and the normal flag is set to 1.
the ready signal is also made true to indicate that the ff_output buffer now holds valid data.
*/


rule rl_stage_3(!wr_flush);
	//$display("Executing FMUL stage 3");
	Bit#(13) lv_exponent= integer_multiplier.result_().summed_exponent;
    bit lv_sign = integer_multiplier.result_().sign;
    let rounding_mode = integer_multiplier.result_().rounding_mode;

 	let lv_new_mantissa = {integer_multiplier.result_().final_result,1'b0};
	Bit#(54) lv_rounded_mantissa= 0;

    $display("lv_exponent: %b lv_new_mantissa: %b", lv_exponent,lv_new_mantissa);
//	Bit#(10) lv_v_shr= 0;
//	Bit#(6) lv_shr;
	bit lv_sticky= 0;
	bit lv_underflow= 0;
	bit lv_overflow= 0;
	bit lv_roundup= 0;
	bit lv_guard;
	bit lv_round;
	Bit#(64) lv_final_output=0;

	Exception e = tagged No_exception;

       if(lv_new_mantissa[106] == 1) begin
         lv_exponent = lv_exponent+1;
         lv_new_mantissa = lv_new_mantissa >> 1;
       end 

       if(lv_exponent[12] == 1) 
          lv_underflow = 1;

 	lv_guard= lv_new_mantissa[52];
	lv_round= lv_new_mantissa[51];

	if(lv_new_mantissa[50:0]!=0)	
		lv_sticky= 1;
	//$display("G=%d R=%d S=%d",lv_guard, lv_round,lv_sticky);

	bit lv_inexact= lv_guard | lv_round | lv_sticky;
 
	if(rounding_mode == 'b000) 			lv_roundup = lv_guard & (lv_new_mantissa[53] | lv_round | lv_sticky);
	else if(rounding_mode == 'b100) 	lv_roundup = lv_guard & (lv_round | lv_sticky | integer_multiplier.result_().sign);
	else if(rounding_mode == 'b011) 	lv_roundup = (lv_guard | lv_round | lv_sticky) & (~integer_multiplier.result_().sign);
	else if(rounding_mode == 'b010) 	lv_roundup = (lv_guard | lv_round | lv_sticky) & (integer_multiplier.result_().sign);  
	else if(rounding_mode == 'b111) begin
		if(integer_multiplier.result_().fsr[7:5] == 'b000)// round to nearest, ties to even
			lv_roundup = lv_guard & (lv_new_mantissa[53] | lv_round | lv_sticky);
		else if(integer_multiplier.result_().fsr[7:5] == 'b100)// round to nearest, ties to max magnitude
			lv_roundup = lv_guard & (lv_round | lv_sticky | integer_multiplier.result_().sign);
		else if(integer_multiplier.result_().fsr[7:5] == 'b011 )// round up
			lv_roundup = (lv_guard | lv_round | lv_sticky) & (~integer_multiplier.result_().sign);
		else if(integer_multiplier.result_().fsr[7:5] == 'b010)// round down		
			lv_roundup = (lv_guard | lv_round | lv_sticky) & (integer_multiplier.result_().sign);
	end
	// else if the rounding mode is round_to_zero, roundup should be zero. Since the default value is zero, we needn't have an else if statement for that.

	if( lv_roundup==1)begin
		lv_rounded_mantissa = {1'b0,lv_new_mantissa[105:53]}+1;
		//$display("Roundup is made 1.");
	end
	else
		lv_rounded_mantissa = {1'b0,lv_new_mantissa[105:53]};


	if(lv_rounded_mantissa[53]==1)
		lv_exponent= lv_exponent+1;


	if(integer_multiplier.result_().invalid==1) begin
		lv_final_output= 64'h7fff_ffff_ffff_ffff;
		e= tagged Invalid True;
	end
	else if(integer_multiplier.result_().zero==1) 
		lv_final_output= {integer_multiplier.result_().sign,63'd0};
	else if(integer_multiplier.result_().infinity==1)
		lv_final_output={integer_multiplier.result_().sign,11'd-1,52'd0};
	else if(lv_underflow==1) begin
		lv_final_output= {integer_multiplier.result_().sign,11'd0,lv_rounded_mantissa[51:0]};
		e=tagged Underflow True;
	end
	else if(lv_exponent[11]==1 || lv_exponent[10:0]=='d-1) begin
		lv_overflow= 1;
		e= tagged Overflow True;
            if(rounding_mode == 'b001)
                    lv_final_output = {integer_multiplier.result_().sign,'h7FEF_FFFF_FFFF_FFFF};
            else if(rounding_mode == 'b010 && integer_multiplier.result_().sign==0)
                    lv_final_output = {integer_multiplier.result_().sign,'h7FEF_FFFF_FFFF_FFFF};
            else if(rounding_mode == 'b011 && integer_multiplier.result_().sign==1)
                    lv_final_output = {integer_multiplier.result_().sign,'h7FEF_FFFF_FFFF_FFFF};

            else if(rounding_mode == 'b111) begin
					if(integer_multiplier.result_().fsr[7:5] == 'b001)
						lv_final_output={integer_multiplier.result_().sign,'h7FEF_FFFF_FFFF_FFFF};
					else if(integer_multiplier.result_().fsr[7:5] == 'b010 && integer_multiplier.result_().sign==0)
						lv_final_output={integer_multiplier.result_().sign,'h7FEF_FFFF_FFFF_FFFF};
					else if(integer_multiplier.result_().fsr[7:5] == 'b011 && integer_multiplier.result_().sign==1)
						lv_final_output={integer_multiplier.result_().sign,'h7FEF_FFFF_FFFF_FFFF};
					else
	           			lv_final_output={integer_multiplier.result_().sign,11'd-1,52'd0};
                end
            else
            	lv_final_output = {integer_multiplier.result_().sign,11'd-1,52'd0};
       

        end
	else begin
 		lv_final_output= {integer_multiplier.result_().sign,lv_exponent[10:0], lv_rounded_mantissa[51:0]};
		if(lv_inexact==1)
   			e= tagged Inexact True;
	end

	Bit#(32) lv_fsr ={integer_multiplier.result_().fsr[31:10],integer_multiplier.result_().infinity,integer_multiplier.result_().zero,integer_multiplier.result_().fsr[7:5],integer_multiplier.result_().invalid,1'b0,lv_overflow,lv_underflow,lv_inexact};

	output_.enq(Output_type{destination		: integer_multiplier.result_().destination,
							fsr				: lv_fsr,
							final_result	: lv_final_output,
							exception: e
							});

		 
	integer_multiplier._deque();// dequing the output buffer of the integer multiplier.
endrule:rl_stage_3

/*
this is input stage.
Here the flag values are decided based on the inputs.
Once all the flag variables are set, using the 32 bit operands, various fields of the stage1 buffer are filled.
the new exponent is calcualted by adding the exponents and subtracting the bias from it.
the sign bit of the result is nothing but the xor of the sign bits of the two operands.
*/
method Action _start(Bit#(64) _operand1,Bit#(64) _operand2,Bit#(5) _destination,Bit#(4) _rob_number, Bit#(32) _fsr, Bit#(3) rounding_mode, Bit#(32) _program_counter);
	//$display("Executing FMUL stage 1");
	//$display("op1: %h, op2: %h, fsr: %h", _operand1, _operand2, _fsr);
		Bit#(1) lv_inf=0;
		Bit#(1) lv_inv=0;
		Bit#(1) lv_zero =0;
		Bit#(1) lv_exp1= _operand1[62:52]=='b11111111111 ? 1:0;			//1 if all the bits of exponent are set; used to check if op1 is infinity or NaN
		Bit#(1) lv_exp2= _operand2[62:52]=='b11111111111 ? 1:0;			//1 if all the bits of exponent are set; used to check if op2 is infinity or NaN
		Bit#(1) lv_man1_is_zero= _operand1[51:0]== 0 ? 1:0;			//1 if mantissa of op1 is 0
		Bit#(1) lv_man2_is_zero= _operand2[51:0]== 0 ? 1:0;			//1 if mantissa of op2 is 0
		Bit#(1) lv_exp1_is_zero= _operand1[62:52]==0? 1:0;				//1 if exponent of operand1 is 0
		Bit#(1) lv_exp2_is_zero= _operand2[62:52]==0? 1:0;				//1 if exponent of operand2 is 0
		Bit#(1) lv_op1_is_zero= lv_man1_is_zero & lv_exp1_is_zero;			//1 when operand1=0
		Bit#(1) lv_op2_is_zero= lv_man2_is_zero & lv_exp2_is_zero;
		

		if((lv_exp1==1 && lv_man1_is_zero==0) || (lv_exp2==1 && lv_man2_is_zero==0))		// either of the operands are NaN
			lv_inv=1;
		else if((lv_exp1==1 && lv_man1_is_zero==1) || (lv_exp2==1 && lv_man2_is_zero==1))	// checks if op1 or op2 are infinite
		begin
			if(lv_op2_is_zero==1 || lv_op1_is_zero==1)				// if either op1 or op2 are zero, then 0*infinity results in NaN
				lv_inv=1;
			else 								//if both are infinite, result is infinite
				lv_inf=1;
		end
		else if(lv_op1_is_zero==1 || lv_op2_is_zero==1)
			lv_zero=1;
		
		Bit#(1) lv_sign= _operand1[63]^_operand2[63];
		Bit#(13) lv_exponent= {2'b0,_operand1[62:52]}+{2'b0,_operand2[62:52]} - 13'b0001111111111;

		//$display("lv_exp1= %b, lv_exp2= %b",lv_exp1,lv_exp2);
	if(lv_exp1_is_zero ==0 && lv_exp2_is_zero==0)					//checking if both op1 and op2 are NOT denormal
		ff_input_register.enq(Input_data_type{	
												destination: _destination,
												rob:_rob_number,
												fsr:_fsr,
						                        rounding_mode : rounding_mode,
												program_counter:_program_counter,
												summed_exponent: lv_exponent,
												sign:lv_sign,
												infinity:lv_inf,
												invalid:lv_inv,
												zero:lv_zero,
												mantissa1:{1'b1,_operand1[51:0]},
												mantissa2:{1'b1,_operand2[51:0]}
												});
	else if(lv_exp1_is_zero==1 && lv_exp2_is_zero==0)				//checking if op1 is denormal
		ff_input_register.enq(Input_data_type{	
												destination: _destination,
												rob:_rob_number,
												fsr:_fsr,
						                        rounding_mode : rounding_mode,
												program_counter:_program_counter,
												summed_exponent: lv_exponent,
												sign:lv_sign,
												infinity:lv_inf,
												invalid:lv_inv,
												zero:lv_zero,
												mantissa1:{1'b0,_operand1[51:0]},	//implicit bit is 0 since op1 is denormal
												mantissa2:{1'b1,_operand2[51:0]}
												});
	else if(lv_exp1_is_zero==0 && lv_exp2_is_zero==1)				//checking if op2 is denormal
		ff_input_register.enq(Input_data_type{	
												destination: _destination,
												rob:_rob_number,
												fsr:_fsr,
						                        rounding_mode : rounding_mode,
												summed_exponent: lv_exponent,
												program_counter:_program_counter,
												sign:lv_sign,
												infinity:lv_inf,
												invalid:lv_inv,
												zero:lv_zero,
												mantissa1:{1'b1,_operand1[51:0]},
												mantissa2:{1'b0,_operand2[51:0]}	//implicit bit is 0 since op2 is denormal
												});
	else 										//if both operand are denormal
		ff_input_register.enq(Input_data_type{	
												destination: _destination,
												rob:_rob_number,
												fsr:_fsr,
						                        rounding_mode : rounding_mode,
												program_counter:_program_counter,
												summed_exponent: lv_exponent,
												sign:lv_sign,
												infinity:lv_inf,
												invalid:lv_inv,
												zero:lv_zero,
												mantissa1:{1'b0,_operand1[51:0]},	//implicit bit is 0 since op1 is denormal
												mantissa2:{1'b0,_operand2[51:0]}	//implicit bit is 0 since op2 is denormal
												});
	
endmethod

// this method sends out the valid result after performing the valid operation.
// this method will only fire as long as the output_ FIFO is not empty.
// if empty then the rule calling this method will also not fire.
method Output_type result_multiplication_();
	return output_.first();
endmethod


// this method is called once the data from the output_ FIFO has been read in the top module.
// this method will hence empty the output_ FIFO
method Action _deque_buffer_reset_ready_signal()if(!wr_flush);
	output_.deq();
endmethod

// when a wr_flush is initiated in the processor this method will also be called.
// it sets the flsuh wire to True, hence no rule other flush_all_fifos will
// fire and hence clear all the buffer and intermediate register/ wires ...
method Action _set_flush(Bool _flush);
		wr_flush<=_flush;
endmethod
endmodule

		/*		TEST BENCH 		*/


//(*synthesize*)
module mkTb_fpu_dpfloating_multiplier(Empty);
Reg#(Bit#(64)) rg_operand1<-mkReg(64'h4014000000000000); 
Reg#(Bit#(64)) rg_operand2<-mkReg(64'h4014000000000000); 
Reg#(Bit#(32)) rg_clock<-mkReg(0); 
Ifc_fpu_dpfloating_multiplier multiplier<-mkfpu_dpfloating_multiplier();

Reg#(Bit#(32)) rg_arbit <-mkReg(0);
rule rl_clk_count; //Increment clock after each cycle.
	rg_clock<=rg_clock+1;
	//$display("%d",rg_clock);
endrule:rl_clk_count


rule rl_start_1(rg_clock=='d0);
	//$display("Giving inputs rg_operand 1 : %h rg_operand 2 : %h",rg_operand1,rg_operand2);
	multiplier._start(rg_operand1,rg_operand2,0,0,{'h0,3'b000,5'b0},3'b000,0); //The Operands passed to the instance of the mkMultiply module. Rounding mode: Round to nearest, ties to even
endrule:rl_start_1

rule rl_display_result;
		let abc = multiplier.result_multiplication_();
		multiplier._deque_buffer_reset_ready_signal();
		//$display("Final result= %h, fsr: %h", abc.final_result, abc.fsr);
		$display("Multiplication result = %h", abc.final_result);
endrule:rl_display_result

rule rl_finish_(rg_clock=='d26); //Determines When to finish execution.
	$finish(0);
endrule:rl_finish_

endmodule:mkTb_fpu_dpfloating_multiplier 


endpackage:fpu_dpfloating_multiplier