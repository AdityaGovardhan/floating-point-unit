/*

Copyright (c) 2013-2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module Name 	: Floating Point addition_subtraction Single and Double precision 
Author Name 	: Vinod.G, Aditya Govardhan
e-mail Id	: g.vinod1993@gmail.com, dtgovardhan@gmail.com
Last updated on : 12th July 2016


*/

package fpu_add_sub;
   import FIFOF::*;
   import FIFO::*;
   import RegFile::*;
   import SpecialFIFOs::*;
   import defined_types::*;

typedef struct{ 
  	bit sign1;
	bit sign2;
	Bit#(TAdd#(fpman,5)) mantissa1;
	Bit#(TAdd#(fpman,5)) mantissa2;
	Bit#(fpexp) exponent1;
	Bit#(fpexp) exponent2;
	bit operation;
	bit lv_is_invalid;
	Bit#(2) lv_is_infinity;
	Bit#(2) lv_is_zero;
	Bit#(3) _rounding_mode;
	Bit#(32) fsr;
    } Stage1_data#(numeric type fpman, numeric type fpexp) deriving(Bits,Eq);

typedef struct { 
	Bit#(TAdd#(fpman,5)) new_mantissa;
	bit sign_bit_output;
	Bit#(fpexp2) exponent_out;
	bit is_invalid; 
	Bit#(2) is_infinity;
	Bit#(2) is_zero;
	bit _underflow;
	bit _overflow;
	Bit#(3) _rounding_mode;
	Bit#(32) fsr;
	} Stage2_data#(numeric type fpman, numeric type fpexp2) deriving(Bits,Eq);

interface Ifc_fpu_add_sub#(numeric type fpinp, numeric type fpman, numeric type fpexp);
	method Action _start(Bit#(fpinp) operand1, Bit#(fpinp) operand2, bit operation, Bit#(3) rounding_mode, Bit#(32) fsr);
	method Action _deque_buffer_();
	method Floating_output _result(); 
endinterface 	
 
  	module mkfpu_add_sub(Ifc_fpu_add_sub#(fpinp,fpman,fpexp))
        provisos( 
                Add#(TAdd#(fpexp,fpman),1,fpinp),	                //Defining fpinp to be fpexp + fpman + 1
                Add#(fpexp,2,fpexp2),
                Add#(fpman,5,fpman5),
                Add#(fpman,4,fpman4),
                Log#(fpman,fplog),
                Add#(fpinp1,1,fpinp),
 
		//Provisos required by the bsc
				Add#(a__, TLog#(TAdd#(1, fpman5)), fpexp),
                Add#(b__, 1, fpman5),
                Add#(2, c__, fpman5),
                Add#(d__, 3, c__),
                Add#(e__, 1, b__),
                Add#(f__, 1, fpexp2),
                Add#(g__, fplog, fpexp2),
                Log#(TAdd#(1, fpman4), fplog),
                Add#(1, i__, fpman),
                Add#(h__,fpinp,64),
                Add#(fpexp2, j__, TAdd#(fpexp, fpman)),

                Log#(TAdd#(1, fpman5), fplog),
                Add#(k__, 3, e__),
                Add#(l__, 1, fpexp)        
                );

        let fPINP  = valueOf(fpinp);
		let fPMAN  = valueOf(fpman);
		let fPEXP  = valueOf(fpexp);

        FIFO#(Floating_output) ff_final_out <- mkPipelineFIFO();
        FIFO#(Stage1_data#(fpman,fpexp)) ff_stage1 <- mkPipelineFIFO();
		FIFO#(Stage2_data#(fpman,fpexp2)) ff_stage2 <- mkPipelineFIFO();
       

        rule rl_stage2;

        	let lv_data_stage1 = ff_stage1.first();
			ff_stage1.deq;

            let sign1 = lv_data_stage1.sign1;
            let sign2 = lv_data_stage1.sign2;
            let mantissa1 = lv_data_stage1.mantissa1;
            let mantissa2 = lv_data_stage1.mantissa2;
            let exponent1 = {2'b00, lv_data_stage1.exponent1};
            let exponent2 = {2'b00, lv_data_stage1.exponent2};
            let operation = lv_data_stage1.operation;

            // $display();
            // $display("sign1 = %b exponent1 = %b mantissa1 = %b", sign1, exponent1, mantissa1);
            // $display("sign2 = %b exponent2 = %b mantissa2 = %b", sign2, exponent2, mantissa2);

            let lv_is_invalid = lv_data_stage1.lv_is_invalid;
            let lv_is_infinity = lv_data_stage1.lv_is_infinity;
            let lv_is_zero = lv_data_stage1.lv_is_zero;

			let _rounding_mode = lv_data_stage1._rounding_mode;
			let fsr = lv_data_stage1.fsr;


			Bit#(fpexp2) lv_minuend, lv_subtrahend;			// lv_minuend has the higher exponent and lv_subtrahend has the lower exponent
			bit op1_gt_op2;									// is set if exponent1 is greater than exponent2
			Bit#(fpexp2) exp_diff;                           // exp_diff has the difference between the lv_minuend and lv_subtrahend
			Bit#(fpexp2) exponent_out;				    	// exponent_out has the resultant exponent of add/sub

			Bit#(fpman5) mantissa_to_shift;					// stores the value of mantissa to be shifted
			Bit#(fpexp2) lv_zeros_on_right;	            	// store the zeros on the right of the last 1 from MSB
			bit lv_sticky = 0;								// =1 => result is inexact

			if(exponent1>exponent2) begin		    		// When exp1 > exp2
				lv_minuend = exponent1;
				lv_subtrahend = exponent2;
				mantissa_to_shift = mantissa2;
				op1_gt_op2 = 1;
			end
			else begin										// When exp2 > exp1                                          
				lv_minuend = exponent2;
				lv_subtrahend = exponent1;
				mantissa_to_shift = mantissa1;
				op1_gt_op2 = 0;
			end
		
			exponent_out = lv_minuend;						// ouput exponent == higher exponent
			exp_diff = lv_minuend - lv_subtrahend;
		
			// $display();
			// $display("op1_gt_op2 = %b", op1_gt_op2);

			lv_zeros_on_right = zeroExtend(pack(countZerosLSB(mantissa_to_shift)));

			mantissa_to_shift = (mantissa_to_shift >> exp_diff);
			
			// this step is carried out to set the sticky bit when all the zeros on the LSB side are shifted out of mantissa
			// all the zeros on the LSB side are shifted out of mantissa when exponent difference is higher than the number of zeros on LSB side
			if((lv_zeros_on_right < exp_diff) || mantissa_to_shift[0] == 1)
				lv_sticky = 1;
				
			mantissa_to_shift = {mantissa_to_shift[fPMAN+4:1],lv_sticky};
		
			// setting back the mantissa that was shifted
			if(op1_gt_op2 == 1)								
				mantissa2 = mantissa_to_shift;				
			else
				mantissa1 = mantissa_to_shift;


  
			bit man1_gt_man2 = 0;

			if(mantissa1 > mantissa2)
				man1_gt_man2 = 1;

			// $display("man1_gt_man2 = %b", man1_gt_man2);
				
            Bit#(fpman5) lv_sum_mantissa;
            bit lv_sign_bit_output = (man1_gt_man2 & sign1) | (~man1_gt_man2 & (operation ^ sign2));
			bit actual_operation = sign1 ^ (operation ^ sign2); //0 for add and 1 for sub,determined using k Maps

			// $display("actual_operation = %b", actual_operation);

			// $display("mantissa1 = %b", mantissa1);
			// $display("mantissa2 = %b", mantissa2);

			if(actual_operation == 0) 	lv_sum_mantissa = mantissa1 + mantissa2;
			else if(man1_gt_man2 == 1) 	lv_sum_mantissa = mantissa1 - mantissa2;
			else 						lv_sum_mantissa = mantissa2 - mantissa1;

			// $display("op_result = %b", lv_sum_mantissa);


			Bit#(fplog) lv_zeros_on_left = pack(countZerosMSB(lv_sum_mantissa));
			lv_sticky = lv_sum_mantissa[0];

			if(lv_sum_mantissa[fPMAN+4] == 1) begin
				lv_sum_mantissa = lv_sum_mantissa >> 1;
				lv_sum_mantissa = {lv_sum_mantissa[fPMAN+4:1], lv_sticky | lv_sum_mantissa[0]};
				exponent_out = exponent_out + 1;
			end
			else if(lv_sum_mantissa[fPMAN+3] != 1) begin
				if(({'b0,lv_zeros_on_left} - 1) > (exponent_out - 'b0000000001)) begin
					lv_sum_mantissa = lv_sum_mantissa << (exponent_out - 'b0000000001);
					exponent_out = 0;
				end
				else begin
					lv_sum_mantissa = lv_sum_mantissa << (lv_zeros_on_left - 1);
					exponent_out = exponent_out - (zeroExtend(lv_zeros_on_left) - 1);
				end
			end

			// $display("op_result = %b shifted", lv_sum_mantissa);

			bit lv_overflow= 0;						
			bit lv_underflow = 0;

			if(exponent_out[fPEXP+1] == 1) begin
				lv_underflow = 1;
				lv_sum_mantissa = 0;
			end
			else if(exponent_out[fPEXP] == 1 || exponent_out[fPEXP-1:0] == 8'd-1) begin
				lv_overflow = 1;
				lv_sum_mantissa = 0;
			end

			// $display();
			// $display("lv_sign_bit_output = %b exponent_out = %b lv_sum_mantissa = %b", lv_sign_bit_output, exponent_out, lv_sum_mantissa);
			// $display("lv_underflow = %b lv_overflow = %b", lv_underflow, lv_overflow);

			ff_stage2.enq (Stage2_data{ 
										new_mantissa 	: lv_sum_mantissa, 
                                        exponent_out	: exponent_out,
                                        sign_bit_output : lv_sign_bit_output,
                                        is_invalid      : lv_is_invalid,       
                                        is_infinity     : lv_is_infinity,      
                                        is_zero         : lv_is_zero,
				 					    _underflow      : lv_underflow,        
                                        _overflow       : lv_overflow,
				   					    _rounding_mode  : _rounding_mode,
										fsr             : fsr
                           });
                
 
	    endrule

	    rule rl_stage3;
            let lv_data_stage2 = ff_stage2.first();
	    	ff_stage2.deq;

            let lv_new_mantissa = lv_data_stage2.new_mantissa;
            let lv_sign_bit_output = lv_data_stage2.sign_bit_output;
            let lv_exp_out = lv_data_stage2.exponent_out;

            let lv_is_invalid = lv_data_stage2.is_invalid;
            let lv_is_infinity= lv_data_stage2.is_infinity;
            let lv_is_zero = lv_data_stage2.is_zero;

            let lv_underflow = lv_data_stage2._underflow;
            let lv_overflow = lv_data_stage2._overflow;

            let rounding_mode = lv_data_stage2._rounding_mode;
			let fsr  = lv_data_stage2.fsr;
	    
		    bit lv_guard = lv_new_mantissa[2];				//guard bit
		    bit lv_round = lv_new_mantissa[1];				//round bit
            bit lv_sticky = lv_new_mantissa[0];

            // $display();
            // $display("lv_guard = %b lv_round = %b lv_sticky = %b", lv_guard, lv_round, lv_sticky);

		    bit lv_roundup = 0;					//=1 => the mantissa needs to be incremented by 1
		    bit lv_inexact = lv_guard|lv_round|lv_sticky;	//=1 => the result is inexact	
		    bit lv_final_inexact=0;


			
			if(rounding_mode == 'b000)				// round to nearest, ties to even
				lv_roundup = lv_guard & (lv_new_mantissa[3] | lv_round | lv_sticky);                     	
			else if(rounding_mode == 'b100)			// round to nearest, ties to max magnitude	
				lv_roundup = lv_guard & (lv_round | lv_sticky | ~lv_sign_bit_output);                     	
			else if(rounding_mode == 'b010)			// round down                               	
				 lv_roundup = lv_inexact & (lv_sign_bit_output);                                          	
			else if(rounding_mode == 'b011)				// round up		                  	
				 lv_roundup = lv_inexact & (~lv_sign_bit_output); 

			else if(rounding_mode == 'b111) begin
				if(fsr[7:5] == 'b000)				// round to nearest, ties to even
				lv_roundup = lv_guard & (lv_new_mantissa[3] | lv_round | lv_sticky);                    	
			else if(fsr[7:5] == 'b100)			// round to nearest, ties to max magnitude	
				lv_roundup = lv_guard & (lv_round | lv_sticky | ~lv_sign_bit_output);                     	
			else if(fsr[7:5] == 'b010)			// round down                               	
				 lv_roundup = lv_inexact & (lv_sign_bit_output);                                          	
			else if(fsr[7:5] == 'b011)				// round up		                  	
				 lv_roundup = lv_inexact & (~lv_sign_bit_output); 
			end

			// $display("lv_roundup = %b", lv_roundup);

			if(lv_roundup == 1) begin	
				lv_new_mantissa = lv_new_mantissa + 'b1000;	
			end
			if(lv_new_mantissa[fPMAN+4] == 1) begin
				lv_new_mantissa = lv_new_mantissa >> 1;
				lv_exp_out = lv_exp_out + 1;
			end
			// $display("lv_sign_bit_output = %b lv_exp_out = %b lv_new_mantissa = %b", lv_sign_bit_output, lv_exp_out, lv_new_mantissa);


	       /*********************************************Final Output Based on Exception Flags*****************************************************/
			Bit#(fpinp) lv_final_output = 0;
			Bit#(fpexp) all_ones = 'd-1;

			Bit#(fpinp1) lv_intermediate_result = 0;
			Bit#(fpexp) max_finite_exp = 'd-2;

			Bit#(fpexp) final_exp = lv_exp_out[fPEXP-1:0];

			// Invalid Case
			if(lv_is_invalid == 1) begin
				lv_final_output = {1'b0, all_ones, 1'b1,'d0};				
			end
			// Infinity Case
	       	else if(lv_is_infinity != 0) begin
				lv_final_output={lv_is_infinity[1], all_ones, 'd0};
			end
			// Overflow Case
			else if(lv_overflow == 1 || lv_exp_out == 'd-1) begin 
				
				lv_final_inexact = 1;
				lv_overflow=1;
				if((rounding_mode == 'b011 && lv_sign_bit_output==0) || (rounding_mode == 'b010 && lv_sign_bit_output==1) || (rounding_mode=='b000))		// (round up and +ve result) or (round down and -ve result) or (round to even)
				begin
					lv_intermediate_result = {all_ones, 'd0};
				end
				else begin
					lv_intermediate_result = {max_finite_exp, 'd-1};
				end
				lv_final_output = {lv_sign_bit_output, lv_intermediate_result};
			end
			// Underflow Case
			else if(lv_underflow == 1) begin
				lv_underflow = 1;
				lv_final_inexact = 1;
				lv_final_output= {lv_sign_bit_output,'d0};	
			end
			// Zero Case
			else if(lv_is_zero[0] != 0) begin
				if(lv_is_zero == 2'b11) begin  //minus zero
					lv_final_output = {1'b1,'d0};
				end
				else if(lv_is_zero == 2'b01) begin //Plus zero
					lv_final_output = 'd0;
				end
			end
			// Normal Result
			else begin
				lv_final_output = {lv_sign_bit_output, final_exp, lv_new_mantissa[fPMAN+2:3]};	
				if (lv_inexact == 1)
					lv_final_inexact = 1;
			end
				
	        Exception lv_exception = None;
			if(lv_underflow == 1)
				lv_exception = Underflow;
			else if(lv_is_invalid == 1)
				lv_exception = Invalid;
			else if(lv_overflow == 1)
	            lv_exception = Overflow;
			else if(lv_final_inexact == 1)
				lv_exception = Inexact;
	
		//***************Determining the floating point status register value*******************//

		//Details for FSR are in the riscv manual
		fsr = {24'b0,rounding_mode,2'b0,lv_overflow,lv_underflow,lv_final_inexact}; 
                
        ff_final_out.enq(Floating_output{	fsr 		 : fsr,
					     					final_result : zeroExtend(lv_final_output),
                                            exception  	 : lv_exception
                     	});	

    endrule


    method Action _start (Bit#(fpinp) operand1, Bit#(fpinp) operand2, bit operation, Bit#(3) rounding_mode, Bit#(32) fsr);

        bit sign1 = operand1[fPINP-1];
        Bit#(fpexp) exponent1 = operand1[fPINP-2:fPMAN];
        Bit#(fpman5) mantissa1;
        
        bit sign2 = operand2[fPINP-1];
        Bit#(fpexp) exponent2 = operand2[fPINP-2:fPMAN];
        Bit#(fpman5) mantissa2;

		Bit#(fpinp1) lv_mod_op1_value = operand1[fPINP-2:0];
		Bit#(fpinp1) lv_mod_op2_value = operand2[fPINP-2:0];

		Bit#(fpman) lv_man11 = operand1[fPMAN-1:0];
		Bit#(fpman) lv_man22 = operand2[fPMAN-1:0];
				
		bit exp1_is_ones = &(exponent1);
		bit exp2_is_ones = &(exponent2);

		bit man1_is_zero = ~ (|(lv_man11));
		bit man2_is_zero = ~ (|(lv_man22));

		bit exp1_is_zero = ~ (|(exponent1));
		bit exp2_is_zero = ~ (|(exponent2));

		bit op1_is_zero = man1_is_zero & exp1_is_zero;
		bit op2_is_zero = man2_is_zero & exp2_is_zero;

		bit op1_is_subnormal = ~man1_is_zero & exp1_is_zero;
		bit op2_is_subnormal = ~man2_is_zero & exp2_is_zero;

		bit op1_is_infinity = exp1_is_ones & man1_is_zero;
		bit op2_is_infinity = exp2_is_ones & man2_is_zero;

		// $display("op1 is subnormal = %b , op2 is subnormal = %b", op1_is_subnormal, op2_is_subnormal);

		Bit#(1) lv_is_invalid=0;						//=1 => Invalid result
		Bit#(1) lv_is_infinity=0;						//=01 => result is plus infinity and 11=> minus infinity
		Bit#(1) lv_infinity_sign = 0;
        Bit#(2) lv_is_zero=0;							//=01 => result is plus zero & =11=> minus zero
		Bit#(1) round_down =0;							//=1 => when rounddown rounding mode

		if(rounding_mode == 'b010)
			round_down = 1;


		// Infinity Cases - Both are infinity
		if (op1_is_infinity == 1 && op2_is_infinity == 1) begin
			lv_is_infinity = ~(sign1 ^ (operation ^ sign2));
			lv_infinity_sign = sign1;
			lv_is_invalid = ~ lv_is_infinity;
		end

		// Infinity Cases - One is infinity
		else if(op1_is_infinity == 1 || op2_is_infinity == 1) begin
			lv_is_infinity = 1;
			lv_infinity_sign = ((op1_is_infinity & ~op2_is_infinity) & sign1) | ((~op1_is_infinity & op2_is_infinity) & (operation ^ sign2));
		end

		// Invalid Cases - Any of them is invalid
		else if((exp1_is_ones == 1 && man1_is_zero == 0) || (exp2_is_ones == 1 && man2_is_zero == 1))  // Any is NaN ->Invalid
			lv_is_invalid = 1;

		//Zero Cases - Both are zero
		else if(op1_is_zero == 1 && op2_is_zero == 1) begin                     				//Both are zeros -> Zero

			if(round_down == 1 && (sign1|(operation^sign2)) == 1) 		lv_is_zero = 2'b11;   	//Using Karnaugh maps, determining if its -0 or +0
  			else if(round_down == 0 && (sign1&(operation^sign2)) == 1) 	lv_is_zero = 2'b11;   	//Using Karnaugh maps, determining if its -0 or +0
			else 														lv_is_zero = 2'b01;		//Plus zero

		end

		//Zero Cases - Both numbers are equal and effective operation is subtraction
		else if(lv_mod_op1_value == lv_mod_op2_value && (operation^sign2) != sign1) begin //For Sub instruction, when operand1==operand2

			if (round_down==1) lv_is_zero = 2'b11;
			lv_is_zero = 2'b01;

		end

        mantissa1 = {1'b0, ~op1_is_subnormal, operand1[fPMAN-1:0],3'b000};               // 3'b000 - Guard, Round and Sticky bit ; 2'b00 - Carry and Hidden
        mantissa2 = {1'b0, ~op2_is_subnormal, operand2[fPMAN-1:0],3'b000};

        exponent1 = exponent1 + {'b0, op1_is_subnormal};
        exponent2 = exponent2 + {'b0, op2_is_subnormal};

		// $display();
		// $display("lv_is_infinity = %b lv_is_invalid = %b lv_is_zero = %b", {lv_infinity_sign, lv_is_infinity}, lv_is_invalid, lv_is_zero);

		ff_stage1.enq(Stage1_data {	sign1: sign1,
		              				sign2: sign2,
									mantissa1: mantissa1,
									mantissa2: mantissa2,
									exponent1: exponent1,
									exponent2: exponent2,
		             				operation: operation,
									lv_is_invalid:lv_is_invalid,
									lv_is_infinity:{lv_infinity_sign, lv_is_infinity},
									lv_is_zero : lv_is_zero,
									_rounding_mode : rounding_mode,
									fsr       : fsr
		              });                                                    
       
	endmethod 
 

	method Action _deque_buffer_();	
        ff_final_out.deq();
	endmethod

	method Floating_output _result();
		return ff_final_out.first();
	endmethod

endmodule



(*synthesize*)
module mkTb_fpu_add_sub(Empty);


	//32 bit inputs
	Ifc_fpu_add_sub#(32,23,8) instance_fpu_add_sub <- mkfpu_add_sub();
	Reg#(Bit#(32)) rg_clock <- mkReg(0);
	Reg#(Bit#(32)) rg_operand1<-mkReg('hff7fffe5); 
	Reg#(Bit#(32)) rg_operand2<-mkReg('hf5ceab35); 

	/* 
	//64 bit inputs
	Ifc_fpu_add_sub#(64,52,11) instance_fpu_add_sub <- mkfpu_add_sub();

	Reg#(Bit#(32)) rg_clock <- mkReg(0);
	Reg#(Bit#(64)) rg_operand1<-mkReg('hfff00000_00000000); 
	Reg#(Bit#(64)) rg_operand2<-mkReg('hfff00000_00000000); 
	*/

	rule get_input(rg_clock == 0);
		// $display("operand1 = %h operand2 = %h", rg_operand1, rg_operand2);
		// $display();
		instance_fpu_add_sub._start(rg_operand1,rg_operand2,0,3'b001,0);
		rg_clock <= rg_clock + 1;
	endrule


	rule get_output;
		let lv_result = instance_fpu_add_sub._result();  
		instance_fpu_add_sub._deque_buffer_();
		// $display();
		// $display("Result is: %h",lv_result.final_result);
		$finish(0);
	endrule

endmodule

module mkTb_fpu_add_sub_2(Empty);
	RegFile#(Bit#(16), Bit#(68))  input_data <- mkRegFileFullLoad("Add_testcases.hex");
	Reg#(Bit#(16)) index <- mkReg(0);
 
	Ifc_fpu_add_sub#(32,23,8) adder <- mkfpu_add_sub();
	Reg#(Bit#(32)) state_clock <- mkReg(1);

	Reg#(int) cnt <- mkReg(0);                  //File Variable
	let fh <- mkReg(InvalidFile) ;				//File handler		
	//rule for file creation
	rule open (cnt == 0 ) ;
		File tb_mul_output <- $fopen("tb_add_sub_output.hex", "w+"); 
		fh <= tb_mul_output;
		cnt <= 1 ;
	endrule

	rule state_clock_count;
		state_clock <= state_clock + 1;
	endrule

	rule take_input_in (state_clock <= 16561);
		adder._start(input_data.sub(index)[67:36],input_data.sub(index)[35:4],0,input_data.sub(index)[2:0],0);
		index <= index + 1;
	endrule

	rule display_output;
		let abc = adder._result();
		adder._deque_buffer_();
		$fwrite(fh, "%h\n", abc.final_result[31:0]);

	endrule

	rule end_testing (state_clock == 16570);
		$finish(0);
	endrule

endmodule

endpackage