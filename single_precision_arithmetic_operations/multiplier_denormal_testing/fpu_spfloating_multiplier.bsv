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
Email ID: neelgala@gmail.com, c.arjunmenon@gmail.com, g.vinod1993@gmail.com, dtgovardhan@gmail.com
Last updated on : 12th July, 2016


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
package fpu_spfloating_multiplier;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
import defined_types::*;
import RegFile::*;
import integermultiplier_for_spfpu::*;


interface Ifc_fpu_spfloating_multiplier;
	/*Input Methods */
	method Action _start(Bit#(32) _operand1, Bit#(32) _operand2, Bit#(32) _fsr, Bit#(3) rounding_mode);
	method Action _deque_buffer_reset_ready_signal();
	/* Output Methods */
	method Floating_output result_multiplication_();
endinterface:Ifc_fpu_spfloating_multiplier

typedef struct{
	Bit#(24) mantissa1; 	// mantissa of operand1
	Bit#(24) mantissa2; 	// mantissa of operand2
	bit sign; 		// sign bit of the result
	Bit#(10) summed_exponent;// exponent of the resultant
	Bit#(1) infinity;	// indicating that the ff_output is infinity.
	Bit#(1) invalid;	// indicating that the ff_output is NaN.
	Bit#(1) zero;		// indicating that the ff_output is zero.
	Bit#(32) fsr;		// the floating-point status register containing all the flags and control bits for dynamic rounding.
    Bit#(3) rounding_mode;  // static rounding mode encoded in the instruction
	}Input_data_type deriving (Bits,Eq);
	
typedef struct{
	bit sign;			// sign bit of the result
	Bit#(10) exponent;		// exponent of the resultant
	Bit#(25) mantissa;	// manstissa of the result
	Bit#(32) fsr;			// the floating-point status register containing all the flags and control bits for dynamic rounding.
    Bit#(3) rounding_mode;          // static rounding mode encoded in the instruction
	Bit#(1) infinity;		// indicating that the ff_output is infinity.
	Bit#(1) invalid;		// indicating that the ff_output is NaN.
	Bit#(1) zero;			// indicating that the ff_output is zero.
	Bit#(1) underflow;		// indicating that the ff_output has an underflow.
	bit inexact;
	}Stage3_data_type deriving (Bits,Eq);


(*synthesize*)
module mkfpu_spfloating_multiplier(Ifc_fpu_spfloating_multiplier);

FIFOF#(Floating_output) output_<-mkFIFOF(); 	// ff_output FIFO which holds the final result. This is of type FIFOF.
FIFO#(Stage3_data_type) ff_stage3 <-mkPipelineFIFO();	// intermediate FIFO
FIFO#(Input_data_type) ff_input_register<-mkPipelineFIFO();	// input FIFO

Ifc_integer_multiplier_for_spfmul integer_multiplier<-mkinteger_multiplier_for_spfmul; //Instance of the 4-Cylce Integer Multiplier.

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
rule rl_stage1_after_input_stage;
	// $display();
	// $display("Input to integer_multiplier %b and %b", {8'b0,ff_input_register.first().mantissa1}, {8'b0,ff_input_register.first().mantissa2});
	integer_multiplier._start({8'b0,ff_input_register.first().mantissa1}, {8'b0,ff_input_register.first().mantissa2}, ff_input_register.first().fsr, ff_input_register.first().rounding_mode, ff_input_register.first().sign, ff_input_register.first().summed_exponent, ff_input_register.first().invalid,ff_input_register.first().infinity,ff_input_register.first().zero);
	
	ff_input_register.deq();	
endrule:rl_stage1_after_input_stage

/*
STRATEGY USED FOR DEALING WITH NORMAL AS WELL AS SUBNORMAL NUMBERS:
-The integer multiplier is given two 32bit inputs, both having leading 8 bits zero
-32bit integer multiplier gives 64bit output out of which leading 16 bits are always zero, so the vector [47:0] is used

I. 	Both inputs normal and summed exponent greater than -126 (inputs are normal and output is normal):
1)	If both the inputs are normal, the 48bit output is of the form 1.xxxxxx or 10.xxxxxx i.e. 48th bit would be zero or one.
2)	A zero is appended at LSB of lv_new_mantissa making it 49 bits, to handle the case of 10.xxxxxx
3)	If it is 10.xxxxxx case, then mantissa is shifted right once making it 01.xxxxxx and the exponent is increased by one.

II. Both inputs normal and summed exponent is smaller than -126 (inputs are normal but output is subnormal):
1)	Step 1) to 3) of case I is followed here also.
2)	Here the exponent is made -126 by shifting the mantissa right as much as required (use of mantissa_shift)
3) 	The biased_exponent is forced to 8'b00000000 as in the case of subnormal numbers

III.Input is normal and subnormal and summed exponent is smaller than -126 (input normal and subnormal, output subnormal)
1)	In this case the integer multiplier output is of the form 00.xxxxxx
2)	Since the exponent is lesser than -126, the output is subnormal and the mantissa is shifted right to make the exponent -126
3)	The biased_exponent is forced to 8'b00000000 as in the case of subnormal numbers

IV.	Input is normal and subnormal and summed exponent is greater than -126 (input normal and subnormal, output normal or subnormal)
1)	In this case as well the integer multiplier is of the form 00.xxxxxx
2)	The number of leading zeros in mantissa is calculated and the number of possible shifts to make the output normal is calculated
3)	If this possible shift is greater than that can be extracted from the exponent before it goes below -126, the output is subnormal, otherwise the output is normal

V.	Both input subnormal - output is made zero, underflow flag is set :TO BE VERIFIED
*/


rule rl_stage_3;

	Bit#(49) lv_product_mantissa 	= {integer_multiplier.result_().final_result[47:0],0};
	Bit#(10) lv_product_exponent 	= integer_multiplier.result_().summed_exponent;
	bit lv_product_sign 			= integer_multiplier.result_().sign;
	Bit#(32) lv_fsr					= integer_multiplier.result_().fsr;
	Bit#(3) lv_rounding_mode 		= integer_multiplier.result_().rounding_mode;
	bit lv_infinity					= integer_multiplier.result_().infinity;
	bit lv_invalid					= integer_multiplier.result_().invalid;
	bit lv_zero 					= integer_multiplier.result_().zero;

	// $display("integer_multiplier_output[47:0] = %b", integer_multiplier.result_().final_result[47:0]);
	// $display();

	bit lv_product_underflow = 0;
	bit lv_product_overflow = 0;

	Int#(10) lv_actual_product_exponent = unpack(lv_product_exponent - 10'b0001111111);

	let msb_zeros = pack(countZerosMSB(lv_product_mantissa));
	let lsb_zeros = 0;

	// lv_product_is_subnormal construct is like a flag which can be used in difficult situations
	bit lv_product_is_subnormal = 0;
	bit lv_sticky = lv_product_mantissa[0];

	/*
	if exponent is > 127 then obviously none of the numbers are subnormal
	so the product is of the form 1x.xxxx or 01.xxxx
	the overflow conditions are handled in the following if condition accordingly
	*/
	if(lv_actual_product_exponent > 127 || (msb_zeros == 0 && lv_actual_product_exponent == 127)) begin
		lv_product_overflow = 1;
		//When the product overflows, the FMA result is an overflow
		// $display("lv_product_overflow!!!");
	end

	/*
	-151 = -126 -23 -2
	-2 is for the implicit bit and the carry bit
	i.e. if all the bits are shifted out then its an underflow
	*/
	else if(lv_actual_product_exponent < -151) begin
		lv_product_underflow = 1;
		lv_product_mantissa = 1;
		lv_product_exponent = 0;
		//When the exponent is < -151, sticky bit is automatically set to one
		// $display("lv_product_underflow!!!");
	end
	 	
	else begin
		/*
		if msb of product is 1 then the case is 1x.xxxx
		product is shifted right once to make it 01.xxxx
		we don't care what is the exponent, just increase it by one
		actual exponent is also increased by one since exponent is increased by one
		this increasing of exponent leading to overflow is handled in the overflow case
		msb_zeros is increased for further arising conditions
		*/
		if(msb_zeros == 0) begin
			lv_product_mantissa = lv_product_mantissa >> 1;
			
			lv_product_exponent = lv_product_exponent + 1;
			lv_actual_product_exponent = lv_actual_product_exponent + 1;
			msb_zeros = msb_zeros + 1;
		end

		// possible shift is positive when exponent is lesser than -126
		Int#(10) possible_shift = -126 - (lv_actual_product_exponent);
		// $display("possible_shift = %0d", possible_shift);

		/*
		msb_zeros = 1 when
		i)  the product is 1x.xxxx and shifted right once
		ii) the product is 01.xxxx already
		if possible_shift is negative or zero, it means that exponent is -126 or greater
		and thus the product is already normalized
		but if possible_shift is positive, it means that exponent is < -126
		and thus product is shifted right to make exponent -126 and the result is subnormal
		*/
		if((msb_zeros == 1 || msb_zeros != 1) && possible_shift > 0) begin

			//Setting sticky if all lsb zeros are removed out
			lsb_zeros = pack(countZerosLSB(lv_product_mantissa));
			if(possible_shift > unpack(zeroExtend(lsb_zeros)) || lv_product_mantissa[0] == 1) lv_sticky = 1;

			//Handling sticky
			lv_product_mantissa = lv_product_mantissa >> pack(possible_shift);
			lv_product_mantissa = {lv_product_mantissa[48:1], lv_product_mantissa[0] | lv_sticky};
			lv_sticky = lv_product_mantissa[0];
			// $display("lv_product_mantissa = %b since exp < -126", lv_product_mantissa);
			// $display("and thus the sticky bit = %b", lv_sticky);


			lv_product_exponent = lv_product_exponent + pack(possible_shift);
			lv_product_is_subnormal = 1;
		end

		/*
		msb_zeros != 1 means product is of the form 00.xxxx, important case
		*/
		else if(msb_zeros != 1) begin
			/*
			if possible shift is < the number of leading zeros then the number can't be made normal
			*/
			if((~pack(possible_shift)+1) < zeroExtend(msb_zeros - 1)) begin
				
				lv_product_mantissa = lv_product_mantissa << (~pack(possible_shift)+1);
				lv_product_exponent = lv_product_exponent - (~pack(possible_shift)+1);
				lv_product_is_subnormal = 1;
			end
			/*
			if exponent affords to give away enough such that shifting left leads to 01.xxxx and exponent >= -126
			*/
			else begin

				lv_product_mantissa = lv_product_mantissa << (msb_zeros - 1);
				lv_product_exponent = lv_product_exponent - (zeroExtend(msb_zeros) - 1);
				lv_product_is_subnormal = 0;
 		end
		end
	end

	if(lv_product_is_subnormal == 1) lv_product_exponent = 0;

	// $display("lv_product_sign = %b lv_product_exponent = %b lv_product_mantissa = %b", lv_product_sign, lv_product_exponent, lv_product_mantissa);

	Bit#(25) lv_rounded_mantissa = lv_product_mantissa[48:24];

	bit lv_guard = lv_product_mantissa[23];
	bit lv_round = lv_product_mantissa[22];
	lv_sticky = |(lv_product_mantissa[21:0]);
	bit lv_roundup = 0;

	Bit#(32) lv_final_output = 0;
	Exception e = None;

	bit lv_inexact = lv_guard | lv_round | lv_sticky;

	// $display();
	// $display("lv_guard = %b, lv_round = %b, lv_sticky = %b", lv_guard, lv_round, lv_sticky);

	if(lv_rounding_mode == 'b000)
		lv_roundup = lv_guard & (lv_product_mantissa[24] | lv_round | lv_sticky);
	else if(lv_rounding_mode == 'b100)
		lv_roundup = lv_guard & (lv_round | lv_sticky | lv_product_sign);
	else if(lv_rounding_mode == 'b011)
		lv_roundup = (lv_guard | lv_round | lv_sticky) & (~lv_product_sign);
	else if(lv_rounding_mode == 'b010)
		lv_roundup = (lv_guard | lv_round | lv_sticky) & (lv_product_sign);  
	
	else if(lv_rounding_mode == 'b111) begin
		if(lv_fsr[7:5] == 'b000)// round to nearest, ties to even
			lv_roundup = lv_guard & (lv_product_mantissa[24] | lv_round | lv_sticky);
		else if(lv_fsr[7:5] == 'b100)// round to nearest, ties to max magnitude
			lv_roundup = lv_guard & (lv_round | lv_sticky | lv_product_sign);
		else if(lv_fsr[7:5] == 'b011 )// round up
			lv_roundup = (lv_guard | lv_round | lv_sticky) & (~lv_product_sign);
		else if(lv_fsr[7:5] == 'b010)// round down		
			lv_roundup = (lv_guard | lv_round | lv_sticky) & (lv_product_sign);
	end

	// else if the rounding mode is round_to_zero, roundup should be zero. Since the default value is zero, we needn't have an else if statement for that.

	// $display("lv_roundup = %b", lv_roundup);
	if( lv_roundup == 1)begin
		lv_rounded_mantissa = lv_rounded_mantissa + 1;
	end

	if(lv_rounded_mantissa[24] == 1 ) begin
		lv_product_exponent = lv_product_exponent + 1;
		lv_rounded_mantissa = lv_rounded_mantissa >> 1;
	end
	else if(lv_product_mantissa[48:47] == 0 && lv_rounded_mantissa[23] == 1) begin
		lv_product_exponent = lv_product_exponent + 1;
	end

	// $display("lv_product_sign = %b lv_product_exponent = %b lv_rounded_mantissa = %b", lv_product_sign, lv_product_exponent, lv_rounded_mantissa);


	if(lv_invalid == 1) begin
		lv_final_output = {1'b0, 8'd-1, 23'd-1};
		e = Invalid;
	end
	else if(lv_zero == 1) 
		lv_final_output = {lv_product_sign, 31'd0};

	else if(lv_infinity == 1)
		lv_final_output = {lv_product_sign, 8'd-1, 23'd0};

	else if(lv_product_underflow == 1) begin
		lv_final_output = {lv_product_sign, 8'd0, lv_rounded_mantissa[22:0]};
		e = Underflow;
	end
	else if(lv_product_overflow == 1 || lv_product_exponent[7:0]=='d-1) begin

		e = Overflow;

		if(lv_rounding_mode == 'b001)
			lv_final_output = {lv_product_sign,'h7f7fffff};
		else if(lv_rounding_mode == 'b010 && lv_product_sign == 0)
			lv_final_output = {lv_product_sign,'h7f7fffff};
		else if(lv_rounding_mode == 'b011 && lv_product_sign == 1)
			lv_final_output = {lv_product_sign,'h7f7fffff};
		else if(lv_rounding_mode == 'b111) begin
			if(lv_fsr[7:5] == 'b001)
				lv_final_output = {lv_product_sign,'h7f7fffff};
			else if(lv_fsr[7:5] == 'b010 && lv_product_sign == 0)
				lv_final_output={lv_product_sign,'h7f7fffff};
			else if(lv_fsr[7:5] == 'b011 && lv_product_sign == 1)
				lv_final_output = {lv_product_sign,'h7f7fffff};
			else
				lv_final_output = {lv_product_sign,8'd-1,23'd0};
			end
		else
			lv_final_output = {lv_product_sign,8'd-1,23'd0};
       
	end
	else begin
 		lv_final_output = {lv_product_sign,lv_product_exponent[7:0], lv_rounded_mantissa[22:0]};
		if(lv_inexact == 1)
   			e = Inexact;
	end
	// $display("lv_sign = %b, lv_exponent = %b, lv_mantissa = %b", lv_product_sign,lv_product_exponent[7:0], lv_rounded_mantissa[22:0]);

	lv_fsr = {lv_fsr[31:10], lv_infinity, lv_zero, lv_fsr[7:5], lv_invalid, 1'b0, lv_product_overflow, lv_product_underflow, lv_inexact};

	output_.enq(Floating_output{
							fsr				: lv_fsr,
							final_result	: zeroExtend(lv_final_output),
							exception 		: e
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
method Action _start(Bit#(32) _operand1,Bit#(32) _operand2, Bit#(32) _fsr, Bit#(3) rounding_mode);

	Int#(8) actual_exponent1 = unpack(_operand1[30:23] - 8'b01111111);
	Int#(8) actual_exponent2 = unpack(_operand2[30:23] - 8'b01111111);

	Bit#(1) lv_inf = 0;
	Bit#(1) lv_inv = 0;
	Bit#(1) lv_zero = 0;

	Bit#(1) lv_exp1 = _operand1[30:23]=='b11111111 ? 1:0;				//1 if all the bits of exponent are set; used to check if op1 is infinity or NaN
	Bit#(1) lv_exp2 = _operand2[30:23]=='b11111111 ? 1:0;				//1 if all the bits of exponent are set; used to check if op2 is infinity or NaN
	
	Bit#(1) lv_man1_is_zero = _operand1[22:0]== 0 ? 1:0;					//1 if mantissa of op1 is 0
	Bit#(1) lv_man2_is_zero = _operand2[22:0]== 0 ? 1:0;					//1 if mantissa of op2 is 0
	
	Bit#(1) lv_exp1_is_zero = _operand1[30:23]==0? 1:0;					//1 if exponent of operand1 is 0
	Bit#(1) lv_exp2_is_zero = _operand2[30:23]==0? 1:0;					//1 if exponent of operand2 is 0
	
	Bit#(1) lv_op1_is_zero = lv_man1_is_zero & lv_exp1_is_zero;			//1 when operand1=0
	Bit#(1) lv_op2_is_zero = lv_man2_is_zero & lv_exp2_is_zero;			//1 when operand2=0

	Bit#(1) lv_op1_subnormal = lv_exp1_is_zero & ~lv_man1_is_zero;		//1 when operand1 is subnormal
	Bit#(1) lv_op2_subnormal = lv_exp2_is_zero & ~lv_man2_is_zero;		//1 when operand2 is subnormal

	// $display("op1 is subnormal = %b , op2 is subnormal = %b", lv_op1_subnormal, lv_op2_subnormal);
	// $display("sign1 = %b exponent1 = %b actual_exponent1 = %0d mantissa1 = %b.%b", _operand1[31], _operand1[30:23], actual_exponent1, ~lv_op1_subnormal, _operand1[22:0]);
	// $display("sign2 = %b exponent2 = %b actual_exponent2 = %0d mantissa2 = %b.%b", _operand2[31], _operand2[30:23], actual_exponent2, ~lv_op2_subnormal, _operand2[22:0]);
	

	if((lv_exp1 == 1 && lv_man1_is_zero == 0) || (lv_exp2 == 1 && lv_man2_is_zero == 0))		// either of the operands are NaN
		lv_inv=1;
	else if((lv_exp1 == 1 && lv_man1_is_zero == 1) || (lv_exp2 == 1 && lv_man2_is_zero == 1))	// checks if op1 or op2 are infinite
	begin
		if(lv_op2_is_zero == 1 || lv_op1_is_zero == 1)											// if either op1 or op2 are zero, then 0*infinity results in NaN
			lv_inv = 1;
		else 																					// if both are infinite, result is infinite
			lv_inf = 1;
	end
	else if(lv_op1_is_zero == 1 || lv_op2_is_zero == 1 || (lv_op1_subnormal == 1 && lv_op2_subnormal == 1)) //if either of the operand are zero or both are denormal
		lv_zero = 1;
	
	Bit#(1) lv_sign = _operand1[31] ^ _operand2[31];
	
/*
	When normal and denormal number is multiplied, exponent is
	(biased_exponent - bias) + (1 - bias) + bias = biased_exponent - bias + 1;
	either _operand1[30:23] == 0 or _operand2[30:23] == 0 for the above if condition so no harm in adding both
*/
	Bit#(10) lv_exponent = {2'b0,_operand1[30:23]} + {2'b0,_operand2[30:23]} - 10'b0001111111 + {'b0, lv_op1_subnormal} + {'b0, lv_op2_subnormal};

	Int#(9) lv_actual_exponent = unpack(lv_exponent[8:0] - 9'b001111111);
	// $display("lv_exponent = %b, lv_actual_exponent = %0d", lv_exponent, lv_actual_exponent);

	ff_input_register.enq(Input_data_type{	summed_exponent: lv_exponent,
											sign:lv_sign,
											mantissa1:{~lv_op1_subnormal,_operand1[22:0]},	//implicit bit is 0 since op1 is denormal
											mantissa2:{~lv_op2_subnormal,_operand2[22:0]},	//implicit bit is 0 since op2 is denormal
											fsr:_fsr,
                                            rounding_mode : rounding_mode,
											zero:lv_zero,
											infinity:lv_inf,
											invalid:lv_inv});

endmethod

// this method sends out the valid result after performing the valid operation.
// this method will only fire as long as the output_ FIFO is not empty.
// if empty then the rule calling this method will also not fire.
method Floating_output result_multiplication_();
	return output_.first();
endmethod


// this method is called once the data from the output_ FIFO has been read in the top module.
// this method will hence empty the output_ FIFO
method Action _deque_buffer_reset_ready_signal();
	output_.deq();
endmethod

endmodule

		/*		TEST BENCH 		*/

//(*synthesize*)
module mkTb_fpu_spfloating_multiplier(Empty);
	Reg#(Bit#(32)) rg_operand1<-mkReg(32'hC2206000); 
	Reg#(Bit#(32)) rg_operand2<-mkReg(32'h80033148); 
	Reg#(Bit#(32)) rg_clock<-mkReg(0); 
	Ifc_fpu_spfloating_multiplier multiplier<-mkfpu_spfloating_multiplier();

	Reg#(Bit#(32)) rg_arbit <-mkReg(0);
	rule rl_clk_count; //Increment clock after each cycle.
		rg_clock<=rg_clock+1;
	endrule:rl_clk_count


	rule rl_start_1(rg_clock=='d0);
		// $display("Giving inputs rg_operand 1 : %b rg_operand 2 : %b through testbench",rg_operand1,rg_operand2);
		// $display();
		multiplier._start(rg_operand1,rg_operand2,{'b0,3'b000,5'b0},3'b000); //The Operands passed to the instance of the mkMultiply module. Rounding mode: Round to nearest, ties to even
	endrule:rl_start_1

	rule rl_display_result;
		let abc = multiplier.result_multiplication_();
		multiplier._deque_buffer_reset_ready_signal();
		// $display("Final result= %h, fsr: %h", abc.final_result, abc.fsr);
	endrule:rl_display_result

	rule rl_finish_(rg_clock=='d26); //Determines When to finish execution.
		$finish(0);
	endrule:rl_finish_

endmodule:mkTb_fpu_spfloating_multiplier



module mkTb_fpu_spfloating_multiplier_2(Empty);
	
	RegFile #(Bit #(10), Bit #(68))  input_data <- mkRegFileFullLoad("regfile_operands1.hex");
	Reg #(Bit #(10)) index <- mkReg(0);
	
	Reg #(Bit #(32)) state_clock <- mkReg(1);
 
 	/*****************Module Instantiation******************************/
	Ifc_fpu_spfloating_multiplier multiplier <- mkfpu_spfloating_multiplier();


	/******************File Creation************************************/
	Reg#(int) cnt <- mkReg(0);                  //File Creation counter
	let fh <- mkReg(InvalidFile) ;				//File Handler
	rule open (cnt == 0 ) ;
		File tb_mul_output <- $fopen("tb_mul_output.hex", "w+"); 
		fh <= tb_mul_output;
		cnt <= 1 ;
	endrule

	/******************clock_count**************************************/
	rule state_clock_count;
		state_clock <= state_clock + 1;
	endrule

	/*******************input******************************************/
	rule take_input_in (state_clock <= 593);
		// $display("The input1 %h and input2 %h with rounding %b", input_data.sub(index)[67:36], input_data.sub(index)[35:4], input_data.sub(index)[2:0]);
		multiplier._start(input_data.sub(index)[67:36],input_data.sub(index)[35:4],0,input_data.sub(index)[2:0]);
		index <= index + 1;
	endrule

	/*******************output*****************************************/
	rule display_output;
		let abc = multiplier.result_multiplication_();
		multiplier._deque_buffer_reset_ready_signal();
		$fwrite(fh, "%h\n", abc.final_result[31:0]);
		// $display("Final result= %h, fsr: %h", abc.final_result, abc.fsr);
	endrule

	/******************end testing*************************************/
	rule end_testing (state_clock == 941);
		$finish(0);
	endrule

endmodule


endpackage:fpu_spfloating_multiplier
