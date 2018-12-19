/*
Copyright (c) 2013-2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name   	: Single Precision Floating Point Number Fused Multiply-Add Unit
Author's Name 	: Aditya Govardhan, Vinod.G
e-mail id	: dtgovardhan@gmail.com, g.vinod1993@gmail.com
Last updated on : 12th July 2016

Paper Reference: Floating Point Fused Multiply-Add Architectures (http://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=4487224)

*/

package fpu_fm_add_sub;

import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*; 
import integermultiplier_for_fma::*;
import defined_types::*;
import RegFile::*;

interface Ifc_fpu_fm_add_sub;

	//Input Methods
	method Action _start(Bit#(32) operand1, Bit#(32) operand2, Bit#(32) _operand3, Bit#(32) fsr, Bit#(3) rounding_mode, bit operation, bit _negate);  
	//Output Methods
	method Action deque_buffer();
	method Floating_output get_result();
	
endinterface

typedef struct{
	Bit#(24) mantissa1; 	// mantissa of operand1
	Bit#(24) mantissa2; 	// mantissa of operand2
	Bit#(10) lv_summed_exponent;// exponent of the resultant
	bit sign; 		// sign bit of the result
    Bit#(32) _operand3;
	Bit#(1) invalid;	// indicating that the ff_output is NaN.
	Bit#(1) infinity;	// indicating that the ff_output is infinity.
	Bit#(1) zero;		// indicating that the ff_output is zero.
	Bit#(32) fsr;		// the floating-point status register containing all the flags and control bits for dynamic rounding.
    Bit#(3) rounding_mode;  // static rounding mode encoded in the instruction
    bit _operation;
    bit _negate;
	}Input_data_type deriving (Bits,Eq);

typedef struct{
    bit sign2;
    bit sign3;
    Bit#(10) exponent2;
    Bit#(10) exponent3;
    Bit#(73) mantissa2;
    Bit#(73) mantissa3;
	Bit#(32) fsr;		// the floating-point status register containing all the flags and control bits for dynamic rounding.
    Bit#(3) rounding_mode;
    bit operation;
    bit _negate;
    bit result_is_invalid;
    Bit#(2) result_is_infinity;
    Bit#(2) result_is_zero;
    bit product_overflow;
    bit product_underflow;
    }Stage3_data_type deriving (Bits,Eq);

typedef struct{
	bit resultant_sign;
	Bit#(10) resultant_exponent;
	Bit#(73) resultant_mantissa;
	Bit#(32) fsr;
	Bit#(3) rounding_mode;
	bit _negate;
	bit result_is_invalid;
	Bit#(2) result_is_infinity;
	Bit#(2) result_is_zero;
	Bit#(2) add_sub_is_zero;
	bit product_overflow;
	bit product_underflow;
	}Stage4_data_type deriving (Bits,Eq);

module mkfpu_fm_add_sub(Ifc_fpu_fm_add_sub);
	FIFOF#(Floating_output) ff_final_out<-mkFIFOF(); 	// ff_output FIFO which holds the final result. This is of type FIFOF.
	FIFO#(Stage3_data_type) ff_stage3 <-mkPipelineFIFO();	// intermediate FIFO
	FIFO#(Stage4_data_type) ff_stage4 <-mkPipelineFIFO();
	FIFO#(Input_data_type) ff_input_register<-mkPipelineFIFO();	// input FIFO

	Ifc_integer_multiplier_for_fma integer_multiplier<-mkinteger_multiplier_for_fma; //Instance of the 4-Cylce Integer Multiplier.

	rule rl_stage1_after_input_stage;
		integer_multiplier._start(	zeroExtend(	ff_input_register.first().mantissa1), 
									zeroExtend(	ff_input_register.first().mantissa2),
												ff_input_register.first()._operand3, 
												ff_input_register.first().fsr, 
												ff_input_register.first().rounding_mode, 
												ff_input_register.first().sign, 
												ff_input_register.first().lv_summed_exponent, 
												ff_input_register.first().invalid,
												ff_input_register.first().infinity,
												ff_input_register.first().zero,
												ff_input_register.first()._operation,
												ff_input_register.first()._negate);
		ff_input_register.deq();	
	endrule:rl_stage1_after_input_stage

	rule rl_stage_3_after_integer_multiplier_stage;

		Bit#(49) lv_product_mantissa	= {integer_multiplier.result_().final_result[47:0],0}; // extra zero for 10.xxxx case
		Bit#(10) lv_product_exponent 	= integer_multiplier.result_().summed_exponent();
		bit lv_product_sign				= integer_multiplier.result_().sign;

		Bit#(1) lv_product_is_invalid 	= integer_multiplier.result_().invalid;
		Bit#(1) lv_product_is_infinity 	= integer_multiplier.result_().infinity;
		Bit#(1) lv_product_is_zero 		= integer_multiplier.result_().zero;
		
		Bit#(32) lv_operand3			= integer_multiplier.result_()._operand3;

		bit operation					= integer_multiplier.result_().operation;
		bit lv_negate 					= integer_multiplier.result_()._negate;
		
		Bit#(32) lv_fsr					= integer_multiplier.result_().fsr;                   
		Bit#(3) lv_rounding_mode		= integer_multiplier.result_().rounding_mode;

	 	integer_multiplier._deque();

	 	$display("lv_product_mantissa = %b", lv_product_mantissa);

	 	bit lv_product_underflow = 0;
	 	bit lv_product_overflow = 0;

	 	Int#(10) lv_actual_product_exponent = unpack(lv_product_exponent - 10'b0001111111);

	 	let msb_zeros = pack(countZerosMSB(lv_product_mantissa));
	 	let lsb_zeros = 0;

	 	// lv_product_is_subnormal construct is like a flag which can be used in difficult situations
	 	// bit lv_product_is_subnormal = 0;
	 	bit lv_sticky = lv_product_mantissa[0];
	 	$display("and thus the sticky bit = %b", lv_sticky);
	 	$display();

	 	/*
	 	if exponent is > 127 then obviously none of the numbers are subnormal
	 	so the product is of the form 1x.xxxx or 01.xxxx
	 	the overflow conditions are handled in the following if condition accordingly
		*/
	 	if(lv_actual_product_exponent > 127 || (msb_zeros == 0 && lv_actual_product_exponent == 127)) begin
	 		lv_product_overflow = 1;
	 		//When the product overflows, the FMA result is an overflow
	 		$display("lv_product_overflow!!!");
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
	 		$display("lv_product_underflow!!!");
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
 				$display("lv_product_mantissa = %b since exp < -126", lv_product_mantissa);
 				$display("and thus the sticky bit = %b", lv_sticky);


 				lv_product_exponent = lv_product_exponent + pack(possible_shift);
 				// lv_product_is_subnormal = 1;
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
	 				// lv_product_is_subnormal = 1;
	 			end
	 			/*
	 			if exponent affords to give away enough such that shifting left leads to 01.xxxx and exponent >= -126
	 			*/
	 			else begin

		 			lv_product_mantissa = lv_product_mantissa << (msb_zeros - 1);
		 			lv_product_exponent = lv_product_exponent - (zeroExtend(msb_zeros) - 1);
		 			// lv_product_is_subnormal = 0;
		 		end
	 		end
	 	end

 		// if(lv_product_is_subnormal == 1) lv_product_exponent = 0;
		
		Bit#(1) sign2 = lv_product_sign;
		Bit#(10) exponent2 = lv_product_exponent;
		Bit#(73) mantissa2 = {lv_product_mantissa, 24'b0};

		Bit#(1) sign3 = lv_operand3[31];
		Bit#(10) exponent3 = {2'b0, lv_operand3[30:23]};
		Bit#(73) mantissa3 = 0;

		bit lv_op3_is_invalid = 0;
		bit lv_op3_is_infinity = 0;
		bit lv_op3_is_zero = 0;

		if(exponent3 == 10'b0011111111) begin
			if(lv_operand3[22:0] == 0) 	lv_op3_is_infinity = 1;
			else 						lv_op3_is_invalid = 1;
		end
		else if(exponent3 == 0) begin
			if(lv_operand3[22:0] == 0) 	lv_op3_is_zero = 1;
			else 						mantissa3 = {2'b00, lv_operand3[22:0], 48'b000};
		end
		else mantissa3 = {2'b01, lv_operand3[22:0], 48'b000};

		Bit#(1) lv_result_is_invalid = 0;
		Bit#(2) lv_result_is_infinity = 0;
		Bit#(2) lv_result_is_zero = 0;

    	//Result is invalid cases
		if(lv_op3_is_invalid == 1 || lv_product_is_invalid == 1)
			lv_result_is_invalid = 1;

		//Result is zero cases
		else if(lv_op3_is_zero == 1 && lv_product_is_zero == 1) begin

			if(lv_rounding_mode == 'b010 && (sign2 | (operation ^ sign3)) == 1) 		lv_result_is_zero = 2'b11; 
			else if(lv_rounding_mode != 'b010 && (sign2 & (operation ^ sign3)) == 1) 	lv_result_is_zero = 2'b11;
			else 																		lv_result_is_zero = 2'b01;

		end

		//Result is infinity cases
		else if(lv_product_is_infinity == 1 && lv_op3_is_infinity == 1) begin
			lv_result_is_infinity 	= {sign2, ~(sign2 ^ (operation ^ sign3))};
			lv_result_is_invalid 	= ~lv_result_is_infinity[0];
		end
		else if(lv_product_is_infinity == 1 || lv_op3_is_infinity == 1) begin
			lv_result_is_infinity = {((lv_product_is_infinity & ~lv_op3_is_infinity) & sign2) | ((~lv_product_is_infinity & lv_op3_is_infinity) & (operation ^ sign3)), 1};
		end

		$display("sign2 = %b exponent2 = %b mantissa2 = %b", sign2, exponent2, mantissa2);
		$display("sign3 = %b exponent3 = %b mantissa3 = %b", sign3, exponent3, mantissa3);
		$display();

		$display("lv_result_is_zero = %b lv_result_is_infinity = %b lv_result_is_invalid = %b", lv_result_is_zero, lv_result_is_infinity, lv_result_is_invalid);
		$display();

		ff_stage3.enq(Stage3_data_type{ 	
											sign2: sign2,
    										sign3: sign3,
    										exponent2: exponent2, 
    										exponent3: exponent3,
    										mantissa2: mantissa2,
    										mantissa3: mantissa3,
											fsr: lv_fsr,
											rounding_mode: lv_rounding_mode,
    										operation: operation,
											_negate: lv_negate,
    										result_is_invalid: lv_result_is_invalid,
    										result_is_infinity: lv_result_is_infinity,
    										result_is_zero: lv_result_is_zero,
    										product_overflow: lv_product_overflow,
    										product_underflow: lv_product_underflow
    										});

	endrule:rl_stage_3_after_integer_multiplier_stage

	rule rl_stage_4 ;

	    bit lv_sign2					= ff_stage3.first().sign2;
	    bit lv_sign3					= ff_stage3.first().sign3;
	    Bit#(73) lv_mantissa2			= ff_stage3.first().mantissa2;
	    Bit#(73) lv_mantissa3			= ff_stage3.first().mantissa3;
	    Bit#(10) lv_exponent2			= ff_stage3.first().exponent2;
	    Bit#(10) lv_exponent3			= ff_stage3.first().exponent3;

		Bit#(32) lv_fsr					= ff_stage3.first().fsr;		
	    Bit#(3) lv_rounding_mode		= ff_stage3.first().rounding_mode;

	    bit lv_operation				= ff_stage3.first().operation;
	    bit lv_negate 					= ff_stage3.first()._negate;

	    bit lv_result_is_invalid		= ff_stage3.first().result_is_invalid;
	    Bit#(2) lv_result_is_infinity	= ff_stage3.first().result_is_infinity;
	    Bit#(2) lv_result_is_zero		= ff_stage3.first().result_is_zero;

	    bit lv_product_overflow			= ff_stage3.first().product_overflow;
	    bit lv_product_underflow		= ff_stage3.first().product_underflow;

	    ff_stage3.deq();

	    Bit#(10) lv_minuend, lv_subtrahend;
	    Bit#(10) exponent_difference = 0;
	    Bit#(10) resultant_exponent = 0;
	    bit op2_gt_op3 = 0;

	    Bit#(73) mantissa_to_shift;
	    let lv_zeros_on_right;
	    bit lv_sticky = 0;
	    
	    if(lv_exponent2 > lv_exponent3) begin
	    	lv_minuend = lv_exponent2;
	    	lv_subtrahend = lv_exponent3;
	    	mantissa_to_shift = lv_mantissa3;
	    	op2_gt_op3 = 1;
	    end
	    else begin
	    	lv_minuend = lv_exponent3;
	    	lv_subtrahend = lv_exponent2;
	    	mantissa_to_shift = lv_mantissa2;
	    	op2_gt_op3 = 0;
	    end

	    resultant_exponent = lv_minuend;
	    exponent_difference = lv_minuend - lv_subtrahend;

		lv_zeros_on_right = zeroExtend(pack(countZerosLSB(mantissa_to_shift)));

		Bit#(1) shifted_operand_zero = mantissa_to_shift == 0? 1:0;
		
		mantissa_to_shift = mantissa_to_shift >> exponent_difference;

		//Handling sticky
		if(((lv_zeros_on_right < exponent_difference) || (mantissa_to_shift[0] == 1)) && shifted_operand_zero != 1)
			lv_sticky = 1;

		mantissa_to_shift = {mantissa_to_shift[72:1], lv_sticky};

	    if(op2_gt_op3 == 1) begin
	    	lv_mantissa3 = mantissa_to_shift;
	    end
	    else begin
	    	lv_mantissa2 = mantissa_to_shift;
	    end

	    $display("lv_sign2 = %b lv_exponent2 = %b lv_mantissa2 = %b", lv_sign2, resultant_exponent, lv_mantissa2);
		$display("lv_sign3 = %b lv_exponent3 = %b lv_mantissa3 = %b", lv_sign3, resultant_exponent, lv_mantissa3);
		$display();

		bit man2_gt_man3 = 0;

	    if(lv_mantissa2 > lv_mantissa3) man2_gt_man3 = 1;

	    Bit#(73) resultant_mantissa = 0;
	    bit resultant_sign = (man2_gt_man3 & lv_sign2) | (~man2_gt_man3 & (lv_operation ^ lv_sign3)); 	// Using Karnaugh maps
	    bit actual_operation = lv_sign2 ^ (lv_operation ^ lv_sign3); 									// 0 for addition 1 for subtraction


		if(actual_operation == 0) 	resultant_mantissa = lv_mantissa2 + lv_mantissa3;
		else if(man2_gt_man3 == 1) 	resultant_mantissa = lv_mantissa2 - lv_mantissa3;
		else						resultant_mantissa = lv_mantissa3 - lv_mantissa2;

		//Case when Mantissa2 = Mantissa3 and hence the result is zero
		Bit#(2) add_sub_is_zero = 0;

		if(resultant_mantissa == 0) begin
			if(lv_rounding_mode == 3'b010) 	add_sub_is_zero = 2'b11;
			else							add_sub_is_zero = 2'b01;			// checks the resultant mantissa for zero
		end

		$display("resultant_sign = %b resultant_exponent = %b resultant_mantissa = %b", resultant_sign, resultant_exponent, resultant_mantissa);
		$display();

		lv_sticky = resultant_mantissa[0];
		let lv_zeros_on_left = pack(countZerosMSB(resultant_mantissa));

		if(resultant_mantissa[72] == 1) begin
			resultant_mantissa = resultant_mantissa >> 1;
			resultant_mantissa = {resultant_mantissa[72:1], lv_sticky | resultant_mantissa[0]};
			resultant_exponent = resultant_exponent + 1;
		end

		else if(resultant_mantissa[71] != 1) begin
			if((zeroExtend(lv_zeros_on_left) - 1) > (resultant_exponent - 'b0000000001)) begin

				resultant_mantissa = resultant_mantissa << (resultant_exponent - 'b0000000001);
				resultant_exponent = 0;
				$display("add_sub subnormal!!!");
			end
			else begin

				resultant_mantissa = resultant_mantissa << (lv_zeros_on_left - 1);
				resultant_exponent = resultant_exponent - (zeroExtend(lv_zeros_on_left) - 1);
			end
		end

		$display("resultant_sign = %b resultant_exponent = %b resultant_mantissa = %b", resultant_sign, resultant_exponent, resultant_mantissa);
		$display();
		

		ff_stage4.enq(Stage4_data_type{	
										resultant_sign:		resultant_sign,
										resultant_exponent:	resultant_exponent,
										resultant_mantissa:	resultant_mantissa,
										fsr:				lv_fsr,
										rounding_mode:		lv_rounding_mode,
										_negate:			lv_negate,
										result_is_invalid:	lv_result_is_invalid,
										result_is_infinity:	lv_result_is_infinity,
										result_is_zero:		lv_result_is_zero,
										add_sub_is_zero:	add_sub_is_zero,
										product_overflow:	lv_product_overflow,
										product_underflow:	lv_product_underflow
										});

	endrule:rl_stage_4

	rule rl_stage_5_final_stage;

	    bit lv_resultant_sign			= ff_stage4.first().resultant_sign;
	    Bit#(10) lv_resultant_exponent	= ff_stage4.first().resultant_exponent;
	    Bit#(73) lv_resultant_mantissa	= ff_stage4.first().resultant_mantissa;
		
		Bit#(32) lv_fsr					= ff_stage4.first().fsr;		
	    Bit#(3) lv_rounding_mode		= ff_stage4.first().rounding_mode;

	    bit lv_result_is_invalid		= ff_stage4.first().result_is_invalid;
	    Bit#(2) lv_result_is_infinity	= ff_stage4.first().result_is_infinity;
	    Bit#(2) lv_result_is_zero		= ff_stage4.first().result_is_zero;
	    Bit#(2) lv_add_sub_is_zero		= ff_stage4.first().add_sub_is_zero;

	    bit lv_product_overflow			= ff_stage4.first().product_overflow;
	    bit lv_product_underflow		= ff_stage4.first().product_underflow;

		bit lv_negate 					= ff_stage4.first()._negate;

	    ff_stage4.deq();

    	Bit#(25) lv_rounded_mantissa = lv_resultant_mantissa[72:48];
	    bit lv_guard = lv_resultant_mantissa[47];
    	bit lv_round = lv_resultant_mantissa[46];
    	bit lv_sticky = |(lv_resultant_mantissa[45:0]);
	    bit lv_round_up = 0;

    	bit lv_inexact = lv_guard | lv_round | lv_sticky;

	    if(lv_rounding_mode == 'b000) 		lv_round_up = lv_guard & (lv_resultant_mantissa[48] | lv_round | lv_sticky);
	    else if(lv_rounding_mode == 'b100) 	lv_round_up = lv_guard & (lv_round | lv_sticky | ~lv_resultant_sign);
	    else if(lv_rounding_mode == 'b010) 	lv_round_up = lv_inexact & (lv_resultant_sign);
	    else if(lv_rounding_mode == 'b011) 	lv_round_up = lv_inexact & (~lv_resultant_sign);

	    else if(lv_rounding_mode == 'b111) begin
			if(lv_fsr[7:5] == 'b000)		lv_round_up = lv_guard & (lv_resultant_mantissa[48] | lv_round | lv_sticky);                    	
			else if(lv_fsr[7:5] == 'b100)	lv_round_up = lv_guard & (lv_round | lv_sticky | ~lv_resultant_sign);                     	
			else if(lv_fsr[7:5] == 'b010)	lv_round_up = lv_inexact & (lv_resultant_sign);                                          	
			else if(lv_fsr[7:5] == 'b011)	lv_round_up = lv_inexact & (~lv_resultant_sign); 
		end

		$display("lv_guard = %b lv_round = %b lv_sticky = %b", lv_guard, lv_round, lv_sticky);
		$display("lv_round_up = %b", lv_round_up);
		$display();

		$display("lv_rounded_mantissa = %b", lv_rounded_mantissa);

	    if(lv_round_up == 1) lv_rounded_mantissa = lv_rounded_mantissa + 1;

		$display("lv_rounded_mantissa = %b after roundup", lv_rounded_mantissa);
		$display();

		if(lv_rounded_mantissa[24] == 1 ) begin
			lv_resultant_exponent = lv_resultant_exponent + 1;
			lv_rounded_mantissa = lv_rounded_mantissa >> 1;
		end
		else if(lv_resultant_mantissa[72:71] == 0 && lv_rounded_mantissa[23] == 1) begin
			lv_resultant_exponent = lv_resultant_exponent + 1;
		end

	    Exception e = None;

	    if(lv_product_underflow == 1) begin
	    	e = Underflow;
	    end

	    Bit#(32) lv_final_output = 0;

	    if(lv_product_overflow == 1 || lv_resultant_exponent[7:0] == 'd-1) begin
	    	e = Overflow;
		    if(lv_rounding_mode == 'b001) 									lv_final_output={lv_resultant_sign,'h7f7fffff}; //??
			else if(lv_rounding_mode == 'b010 && lv_resultant_sign == 0)	lv_final_output={lv_resultant_sign,'h7f7fffff}; //??
			else if(lv_rounding_mode == 'b011 && lv_resultant_sign == 1)	lv_final_output={lv_resultant_sign,'h7f7fffff}; //??
			else begin															
				lv_final_output={lv_resultant_sign,8'd-1,23'd0};
			end
        end
	    else if(lv_result_is_zero[0] == 1) begin
		    lv_final_output = {lv_result_is_zero[1], 8'b0, 23'b0};
		end
	    else if(lv_add_sub_is_zero[0] == 1) begin
	    	lv_final_output = {lv_add_sub_is_zero[1], 8'b0, 23'b0};
	    end
	    else if(lv_result_is_infinity[0] == 1) begin
	    	lv_final_output = {lv_result_is_infinity[1], 8'b11111111, 23'b0};
	    end
	    else if(lv_result_is_invalid == 1) begin
	    	lv_final_output = {1'b0, 8'b11111111, 'd-1};
	    	e = Invalid;
	    end
        else begin
	    	if(lv_negate == 0)	lv_final_output = {lv_resultant_sign, lv_resultant_exponent[7:0], lv_rounded_mantissa[22:0]};
	    	else				lv_final_output = {~lv_resultant_sign, lv_resultant_exponent[7:0], lv_rounded_mantissa[22:0]};
	    end


	    $display("resultant_sign = %b resultant_exponent = %b resultant_mantissa = %b", lv_final_output[31], lv_final_output[30:23], lv_final_output[22:0]);

	    ff_final_out.enq(Floating_output{ 	fsr:			lv_fsr,
	    									final_result:	{32'b0, lv_final_output},
	    									exception:		e});

	endrule

	method Action _start(Bit#(32) _operand1,Bit#(32) _operand2, Bit#(32) _operand3, Bit#(32) _fsr, Bit#(3) rounding_mode, bit operation, bit _negate);
		

	Bit#(1) lv_exp1 = _operand1[30:23] == 'b11111111 ? 1:0;				//1 if all the bits of exponent are set; used to check if op1 is infinity or NaN
	Bit#(1) lv_exp2 = _operand2[30:23] == 'b11111111 ? 1:0;				//1 if all the bits of exponent are set; used to check if op2 is infinity or NaN
	
	Bit#(1) lv_man1_is_zero = _operand1[22:0] == 0 ? 1:0;				//1 if mantissa of op1 is 0
	Bit#(1) lv_man2_is_zero = _operand2[22:0] == 0 ? 1:0;				//1 if mantissa of op2 is 0
	
	Bit#(1) lv_exp1_is_zero = _operand1[30:23] == 0? 1:0;				//1 if exponent of operand1 is 0
	Bit#(1) lv_exp2_is_zero = _operand2[30:23] == 0? 1:0;				//1 if exponent of operand2 is 0
	
	Bit#(1) lv_op1_is_zero = lv_man1_is_zero & lv_exp1_is_zero;			//1 when operand1=0
	Bit#(1) lv_op2_is_zero = lv_man2_is_zero & lv_exp2_is_zero;			//1 when operand2=0

	Bit#(1) lv_op1_infinity = lv_exp1 & lv_man1_is_zero;				//1 when operand1=inf
	Bit#(1) lv_op2_infinity = lv_exp2 & lv_man2_is_zero;				//1 when operand2=inf
	
	Bit#(1) lv_op1_subnormal = lv_exp1_is_zero & ~lv_man1_is_zero;		//1 when operand1 is subnormal
	Bit#(1) lv_op2_subnormal = lv_exp2_is_zero & ~lv_man2_is_zero;		//1 when operand2 is subnormal

	Bit#(1) lv_product_sign = _operand1[31]^_operand2[31];
	
	Int#(8) actual_exponent1 = unpack(_operand1[30:23] - 8'b01111111);
	Int#(8) actual_exponent2 = unpack(_operand2[30:23] - 8'b01111111);
	Int#(8) actual_exponent3 = unpack(_operand3[30:23] - 8'b01111111);
	
	$display("op1 is subnormal = %b , op2 is subnormal = %b", lv_op1_subnormal, lv_op2_subnormal);
	$display("sign1 = %b exponent1 = %b actual_exponent1 = %0d mantissa1 = %b.%b", _operand1[31], _operand1[30:23], actual_exponent1, ~lv_op1_subnormal, _operand1[22:0]);
	$display("sign2 = %b exponent2 = %b actual_exponent2 = %0d mantissa2 = %b.%b", _operand2[31], _operand2[30:23], actual_exponent2, ~lv_op2_subnormal, _operand2[22:0]);
	$display("sign3 = %b exponent3 = %b actual_exponent3 = %0d mantissa3 = %0b.%b", _operand3[31], _operand3[30:23], actual_exponent3, _operand3[30:23] == 0? 0:1, _operand3[22:0]);

	Bit#(1) lv_inf = 0;
	Bit#(1) lv_inv = 0;
	Bit#(1) lv_zero = 0;

	if((lv_exp1 == 1 && lv_man1_is_zero == 0) || (lv_exp2 == 1 && lv_man2_is_zero == 0))		// either of the operands are NaN
		lv_inv=1;

	else if(lv_op1_infinity == 1 || lv_op2_infinity == 1) begin									// checks if op1 or op2 are infinite

		if(lv_op2_is_zero == 1 || lv_op1_is_zero == 1)											// if either op1 or op2 are zero, then 0*infinity results in NaN
			lv_inv=1;
		else 																					//if both are infinite, result is infinite
			lv_inf=1;
	end

	else if(lv_op1_is_zero == 1 || lv_op2_is_zero == 1)
		lv_zero=1;
	
/*
	When normal and denormal number is multiplied, exponent is
	(biased_exponent - bias) + (1 - bias) + bias = biased_exponent - bias + 1;
	either _operand1[30:23] == 0 or _operand2[30:23] == 0 for the above if condition so no harm in adding both
*/
	Bit#(10) lv_summed_exponent = {2'b0,_operand1[30:23]}+{2'b0,_operand2[30:23]} - 10'b0001111111 + {'b0, lv_op1_subnormal} + {'b0, lv_op2_subnormal};
	Bit#(1) lv_sign = _operand1[31] ^ _operand2[31];

	Int#(9) lv_actual_exponent = unpack(lv_summed_exponent[8:0] - 9'b001111111);
	$display("lv_summed_exponent = %b, lv_actual_exponent = %0d", lv_summed_exponent, lv_actual_exponent);
	$display();

	ff_input_register.enq(Input_data_type{	
											mantissa1:{~lv_op1_subnormal, _operand1[22:0]},	//implicit bit is 0 since op1 is denormal
											mantissa2:{~lv_op2_subnormal, _operand2[22:0]},	//implicit bit is 0 since op2 is denormal
											lv_summed_exponent: lv_summed_exponent,
											sign:lv_sign,
											_operand3:_operand3,
											fsr:_fsr,
                                            rounding_mode : rounding_mode,
											infinity:lv_inf,
											invalid:lv_inv,
											zero:lv_zero,
											_operation:operation,
											_negate:_negate});
	endmethod


	method Action deque_buffer();
		ff_final_out.deq();
	endmethod
	
	method Floating_output get_result();
		return ff_final_out.first();
	endmethod

endmodule


module mkTb_fpu_fm_add_sub(Empty);

	Ifc_fpu_fm_add_sub uut <- mkfpu_fm_add_sub();

    Reg#(Bit#(32)) rg_clock <-mkReg(0);
    Reg#(Bit#(32)) operand1 <- mkReg(32'hAFB78DF1);
    Reg#(Bit#(32)) operand2 <- mkReg(32'hB6C1C311);
    Reg#(Bit#(32)) operand3 <- mkReg(32'h8F800001);

    rule rl_count_clock ;
      	rg_clock<=rg_clock+1;
      	if(rg_clock=='d20) $finish(0);
    endrule

    rule rl_input1(rg_clock==1);
        $display("giving inputs at %0d", rg_clock);
        uut._start(operand1, operand2, operand3, 0, 3'b010, 0, 0);

    endrule

    rule rl_finish;
        $display("Output = %h at %0d",uut.get_result().final_result[31:0], rg_clock);
        uut.deque_buffer();
    endrule

endmodule

module mkTb_fpu_fm_add_sub_2 (Empty);
	
	RegFile #(Bit #(16), Bit #(100))  input_data <- mkRegFileFullLoad("regfile_operands1.hex");
	Reg #(Bit #(16)) index <- mkReg(0);
 
	Ifc_fpu_fm_add_sub multiplier <- mkfpu_fm_add_sub();
	Reg #(Bit #(32)) state_clock <- mkReg(1);

	Reg#(int) cnt <- mkReg(0);                  //File Variable
	let fh <- mkReg(InvalidFile) ;				//File handler		

	//rule for file creation
	rule open (cnt == 0 ) ;
		File tb_mul_output <- $fopen("tb_madd_output.hex", "w+"); 
		fh <= tb_mul_output;
		cnt <= 1 ;
	endrule

	rule state_clock_count;
		state_clock <= state_clock + 1;
	endrule

	rule take_input_in (state_clock <= 18732);
		multiplier._start(input_data.sub(index)[99:68],input_data.sub(index)[67:36],input_data.sub(index)[35:4],0,input_data.sub(index)[2:0],0,0);
		index <= index + 1;
	
	endrule

	rule display_output;

		let abc = multiplier.get_result();
		multiplier.deque_buffer();
		$fwrite(fh, "%h\n", abc.final_result[31:0]);
		
	endrule

	rule end_testing (state_clock == 18744);

		$finish(0);

	endrule : end_testing

endmodule

endpackage
