/*

Copyright (c) 2013-2016, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



Module Name     : Single Precision Floating Point Divider
Author Name     : Arjun C. Menon, Vinod.G, Aditya Govardhan
Email ID        : c.arjunmenon@gmail.com, g.vinod1993@gmail.com, dtgovardhan@gmail.com
Last updated on : 12th July, 2016

    This unit carries out division of two floating point numbers. A pipelined architecture is
used implement this module. The algorithm strictly follows the IEEE 754 standard. This unit takes
18 clock cycles to calculate the result; in which 16 clocks are taken to divide the two mantissa. 
Also, the integer divider module is not pipelined. Therefore, after it gets a set of inputs, the module
will not take any further inputs for next 16 cycles.
    The inputs "rob_number" and "pc" do not affect the working of the divider.
They have been kept for use when integrating this code with that of a microprocessor.
Also specific care has been taken to ensure that the ff_output register holds the value of the result
until it is read by the top module. This may lead to stalling the pipeline at certain instance. Thus
once the top module reads the ff_output register value through the method 'final_result' it then asserts
the method deque_buffer_reset_ready_signal. This method empties the ff_output register and resets the
ready signal. Thus, the pipeline continues execution.
    Division of floating point number can create the following exceptions (mentioned is
decreasing priority order): Invalid, Underflow, Overflow, Divide by zero and Inexact. Based on these
exceptions, the result and also the combinations of the inputs appropriate flags (invalid,underflow, 
overflow, divide by zero, inexact) in the fsr are also set.
*/
package fpu_spfloating_divider;



    import FIFO::*;                                     //importing FIFO library
	import FIFOF::*;
    import SpecialFIFOs::*;                             //library which contains PipelineFIFO
    import defined_types::*;                              //contains typedef of exceptions which are generated here
    import integer_divider_for_spdiv::*;               //divider module
    import RegFile::*;

    typedef struct{
    	Bit#(10) exponent;                              
    	Bit#(24) dividend;
    	Bit#(24) divisor;
        bit sign;
    	bit invalid;
    	bit infinity;
    	bit dz;
    	bit zero;
    	Bit#(32) fsr;
		Bit#(3) rounding_mode;
    } Stage1_type deriving (Bits,Eq);                   //Structure of 1st Stage of the pipeline

    interface Ifc_fpu_spfloating_divider;
    	method Action _start(Bit#(32) operand1, Bit#(32) operand2, Bit#(32) fsr, Bit#(3) rounding_mode); // input method to start the floating point operation
        method Action _deque_buffer_reset_ready_signal();// input method to deque output buffer and reset the ready signal
    	
    	method Floating_output final_result_();				 // Output method
    endinterface

(*synthesize*)
module mkfpu_spfloating_divider(Ifc_fpu_spfloating_divider);

	Ifc_integer_divider_for_spdiv int_div <- mkinteger_divider_for_spdiv();    // instantiation of divider module

	FIFOF#(Floating_output) ff_final_out <- mkFIFOF();			// instantiation of output FIFO whose structure definition is given in riscv_types.bsv
	FIFO#(Stage1_type) ff_stage1 <- mkPipelineFIFO();       // instantiation of Stage 1 FIFO

    //This is the second stage of the pipe. Here the division of the two mantissas take place. Rest of the data are enqueued in another FIFO.
	rule rl_stage2;
	    int_div._inputs({ff_stage1.first().divisor,3'd0},
	    				{ff_stage1.first().dividend,3'd0},
	    				ff_stage1.first().exponent,
	    				ff_stage1.first().sign,
	    				ff_stage1.first().infinity,
	    				ff_stage1.first().invalid,
	    				ff_stage1.first().dz,
	    				ff_stage1.first().zero,
	    				ff_stage1.first().fsr,
	    				ff_stage1.first().rounding_mode
	    				);
	    
	    ff_stage1.deq();        //Freeing the previous FIFO so that it can accept new inp

	endrule

	rule rl_stage3;

		Bit#(27) lv_quotient 		= int_div.result_().data[26:0];	//Quotient from the integer divider
		Bit#(28) lv_remainder 		= int_div.result_().data[55:28]; //Remainder from the integer divider
		Bit#(10) lv_exponent 		= int_div.result_().exponent;
		Bit#(1)  lv_sign			= int_div.result_().sign;
		Bit#(1)  lv_infinity    	= int_div.result_().infinity;
		Bit#(1)  lv_invalid 		= int_div.result_().invalid;
		Bit#(1)  lv_dz 				= int_div.result_().dz;
		Bit#(1)  lv_zero 			= int_div.result_().zero;
		Bit#(32)  lv_fsr 			= int_div.result_().fsr;
		Bit#(3)  lv_rounding_mode	= int_div.result_().rounding_mode;

		int_div._remove_last_entry();						//Frees the integer divider

		bit lv_underflow = 0;
		bit lv_overflow = 0;

		Int#(10) lv_actual_exponent = unpack(lv_exponent - 10'b0001111111);

		let msb_zeros = pack(countZerosMSB(lv_quotient));
		let lsb_zeros = 0;

		// lv_quotient_is_subnormal construct is like a flag which can be used in difficult situations
		bit lv_quotient_is_subnormal = 0;
		bit lv_sticky = lv_quotient[0];

		/*
		if exponent is > 128 then obviously none of the numbers are subnormal
		so the product is of the form 1x.xxxx or 01.xxxx
		the overflow conditions are handled in the following if condition accordingly
		*/
		if(lv_actual_exponent > 128) begin
			lv_overflow = 1;
			//When the product overflows, the FMA result is an overflow
			// $display("lv_overflow!!!");
		end

		/*
		-151 = -126 -23 -1
		-1 is for the implicit bit
		i.e. if all the bits are shifted out then its an underflow
		*/
		else if(lv_actual_exponent < -150) begin
			lv_underflow = 1;
			lv_quotient = 1;
			lv_exponent = 0;
			//When the exponent is < -151, sticky bit is automatically set to one
			// $display("lv_underflow!!!");
		end
		 	
		else begin

			// possible shift is positive when exponent is lesser than -126
			Int#(10) possible_shift = -126 - (lv_actual_exponent);
			// $display("possible_shift = %0d", possible_shift);

			if(possible_shift > 0) begin

				//Setting sticky if all lsb zeros are removed out
				lsb_zeros = pack(countZerosLSB(lv_quotient));

				if(possible_shift > unpack(zeroExtend(lsb_zeros)) || lv_quotient[0] == 1) lv_sticky = 1;

				//Handling sticky
				lv_quotient = lv_quotient >> pack(possible_shift);
				lv_quotient = {lv_quotient[26:1], lv_quotient[0] | lv_sticky};
				lv_sticky = lv_quotient[0];

				// $display("lv_quotient = %b since exp < -126", lv_quotient);
				// $display("and thus the sticky bit = %b", lv_sticky);

				lv_exponent = lv_exponent + pack(possible_shift);
				lv_quotient_is_subnormal = 1;
			end

			/*
			msb_zeros != 1 means product is of the form 00.xxxx, important case
			*/
			else if(msb_zeros != 0) begin
				/*
				if possible shift is < the number of leading zeros then the number can't be made normal
				*/
				if((~pack(possible_shift)+1) < zeroExtend(msb_zeros)) begin
					
					lv_quotient = lv_quotient << (~pack(possible_shift)+1);
					lv_exponent = lv_exponent - (~pack(possible_shift)+1);
					lv_quotient_is_subnormal = 1;
				end
				/*
				if exponent affords to give away enough such that shifting left leads to 01.xxxx and exponent >= -126
				*/
				else begin

					lv_quotient = lv_quotient << (msb_zeros);
					lv_exponent = lv_exponent - (zeroExtend(msb_zeros));
					lv_quotient_is_subnormal = 0;
	 			end
			end
		end

		if(lv_quotient_is_subnormal == 1) lv_exponent = 0;

		// $display();
		// $display("lv_quotient = %b, lv_remainder = %b, lv_exponent = %b", lv_quotient, lv_remainder, lv_exponent);

		bit lv_guard = lv_quotient[2];  
		bit lv_round = lv_quotient[1];			
 		bit lv_inexact = 0;				
		bit lv_round_up = 0;						
           
		if(lv_remainder!=0 || lv_quotient[0] == 1) // if the remainder is zero, sticky bit is set to 1.
			lv_sticky = 1;

		if((lv_sticky | lv_guard | lv_round) == 1)// if any of the sticky,guard or round bit is set, the value is inexact.
			lv_inexact = 1;

		// Following if-else condition determine the value of lv_round_up. If set, the mantissa needs to be incremented, else the mantissa remains unchanged.
		if(lv_rounding_mode == 'b000) 
			lv_round_up = lv_guard & (lv_round|lv_sticky|lv_quotient[3]);
		else if(lv_rounding_mode == 'b100)
			lv_round_up = lv_guard & (lv_round|lv_sticky|lv_sign);
		else if(lv_rounding_mode == 'b011) 
			lv_round_up = (lv_guard|lv_round|lv_sticky) & ~lv_sign;
		else if(lv_rounding_mode == 'b010)
            lv_round_up = (lv_guard|lv_round|lv_sticky) & lv_sign;

		else if(lv_rounding_mode == 'b111) begin
			if(lv_fsr[7:5] == 'b000)				// round to nearest, ties to even
				lv_round_up = lv_guard & (lv_round|lv_sticky|lv_quotient[3]);
			else if(lv_fsr[7:5] == 'b100)		// round to nearest, ties to max magnitude
				lv_round_up = lv_guard & (lv_round|lv_sticky|lv_sign);
			else if(lv_fsr[7:5] == 'b011)		// round up 
				lv_round_up = (lv_guard|lv_round|lv_sticky) & ~lv_sign;
			else if(lv_fsr[7:5] == 'b010)		// round down			
				lv_round_up = (lv_guard|lv_round|lv_sticky) & lv_sign;
		end
        // otherwise if round to zero mode, then do nothing

		Bit#(25) lv_rounded_quotient = {1'b0,lv_quotient[26:3]};

		if( lv_round_up == 1) begin
			lv_rounded_quotient = lv_rounded_quotient + 1;
		end

		if(lv_rounded_quotient[24] == 1 ) begin
			lv_exponent = lv_exponent + 1;
			lv_rounded_quotient = lv_rounded_quotient >> 1;
		end
		if(lv_quotient[26] == 0 && lv_rounded_quotient[23] == 1) begin
			lv_exponent = lv_exponent + 1;
		end

		
		Bit#(32) lv_final_output= 0;
		Exception e = None;       

		// result is infinity
		if(lv_infinity == 1)              
			lv_final_output = {lv_sign, 8'd-1, 23'd0};

		// the result is invalid
		else if(lv_invalid == 1) begin              
            lv_final_output = 32'h7fffffff;
    		e = Invalid;
		end

		// operation is divide by zero
        else if(lv_dz==1) begin
            lv_final_output= {lv_sign,8'd-1,23'd0};
            e = Divide_by_Zero;
        end

		// result is zero
        else if(lv_zero == 1)                  
            lv_final_output={lv_sign,31'd0};

        // result is underflow
        else if(lv_underflow == 1) begin
            lv_final_output= {lv_sign,8'd0,lv_rounded_quotient[22:0]};       	//TODO to verify if it needs to be lv_rounded_quotient[22:1] and lv_inexact bit.
    		e = Underflow;
        end

        // result is overflow
        else if(lv_overflow == 1 || lv_exponent[7:0] == 'd-1) begin
    		e = Overflow;

			if(lv_rounding_mode == 'b001)
				lv_final_output = {lv_sign,'h7f7fffff};
			else if(lv_rounding_mode == 'b010 && lv_sign ==0)
				lv_final_output = {lv_sign,'h7f7fffff};
			else if(lv_rounding_mode == 'b011 && lv_sign==1)
				lv_final_output = {lv_sign,'h7f7fffff};
			else if(lv_rounding_mode == 'b111) begin
				if(lv_fsr[7:5] == 'b001)                                     	// round to nearest
					lv_final_output={lv_sign,'h7f7fffff};
				else if(lv_fsr[7:5] == 'b010 && lv_sign==0)   					// round down a positive number
					lv_final_output={lv_sign,'h7f7fffff};
				else if(lv_fsr[7:5] == 'b011 && lv_sign==1)   					// round up a negative number
					lv_final_output={lv_sign,'h7f7fffff};
				else 															// rest of the cases the result is infinity
            		lv_final_output={lv_sign,8'd-1,23'd0};
			end
			else 
				lv_final_output ={lv_sign,8'd-1,23'd0};
        end

       	else begin
            lv_final_output = {lv_sign, lv_exponent[7:0], lv_rounded_quotient[22:0]};
			if(lv_inexact==1)
    			e = Inexact;
		end

        // Forming the new Floating point Status Register
		Bit#(32) lv_fsr_ ={lv_fsr[31:10],lv_infinity,lv_zero,lv_fsr[7:5],lv_invalid,lv_dz,lv_overflow,lv_underflow,lv_inexact}; 		
         
        // Enqueing the final result into the output FIFO
    	ff_final_out.enq(Floating_output{ 
    		                   	  	  fsr             : lv_fsr_,
    		                     	  final_result    : {'d0,lv_final_output},            //Appending zeros at the MSB since the result is a Single Precision number which is 32-bits wide whereas the rob entries are 64-bits.
    		                     	  exception       : e
                                    });

	endrule

	method Action _start(Bit#(32) _operand1, Bit#(32) _operand2,  Bit#(32) fsr, Bit#(3) rounding_mode);

		Int#(8) actual_exponent1 = unpack(_operand1[30:23] - 8'b01111111);
		Int#(8) actual_exponent2 = unpack(_operand2[30:23] - 8'b01111111);

		Bit#(1) lv_inf = 0;
		Bit#(1) lv_inv = 0;
		Bit#(1) lv_zero = 0;
		Bit#(1) lv_dz = 0;

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

		Bit#(1) lv_op1_is_infinity = lv_exp1 & lv_man1_is_zero;
		Bit#(1) lv_op2_is_infinity = lv_exp2 & lv_man2_is_zero;

		// $display("op1 is subnormal = %b , op2 is subnormal = %b", lv_op1_subnormal, lv_op2_subnormal);
		// $display("sign1 = %b exponent1 = %b actual_exponent1 = %0d mantissa1 = %b.%b", _operand1[31], _operand1[30:23], actual_exponent1, ~lv_op1_subnormal, _operand1[22:0]);
		// $display("sign2 = %b exponent2 = %b actual_exponent2 = %0d mantissa2 = %b.%b", _operand2[31], _operand2[30:23], actual_exponent2, ~lv_op2_subnormal, _operand2[22:0]);


	    if((lv_exp1 == 1 && lv_man1_is_zero == 0) || (lv_exp2 == 1 && lv_man2_is_zero == 0) || (lv_op1_is_infinity == 1 && lv_op2_is_infinity == 1) || (lv_op1_is_zero == 1 && lv_op2_is_zero == 1)) begin  	//op1 or op2 are NaN (or) both are infinity (or) both are zero
			lv_inv = 1;                           					//result is invalid
		end
		else if(lv_op1_is_infinity ==1) begin       				//op 2 is neither NaN nor infinity, and op1 is infinity
	        lv_inf=1;                          						//result is infinity
		end
	    else if(lv_op2_is_zero==1) begin            				//op 1 is neither NaN nor infinity, and op2 is zero
            lv_inf=1;                          						//result is infinity
         	lv_dz=1;                                				//setting the divide by zero flag
        end
        else if(lv_op2_is_infinity == 1 || lv_op1_is_zero == 1)   	//{op1 and op2 are not NaN} (and) {op1 is zero and op2 is not zero (or) op2 is infinity and op1 is not infinity}
            lv_zero=1;                              				//result is zero


        Bit#(1) lv_sign= _operand1[31] ^ _operand2[31];

        /*
        total_baised_exponent = (biased_exponent - bias) - (1 - bias) + bias 					in the case of normal divided by subnormal
        total_biased_exponent = (1 - bias) - (biased_exponent - bias) + bias 					in the case of subnormal divided by normal
        total_biased_exponent = (biased_exponent - bias) - (biased_exponent - bias) + bias 		in the case of normal divided by normal
        total_biased_exponent = (1 - bias) - (1 - bias) + bias 									in the case of subnormal divided by normal

		SO equivalently to handle all the cases:
		total_biased_exponent = bias + (op1_biased_expo + is_op1_denorm) - (op2_biased_expo + is_op2_denorm)
        */
        Bit#(10) lv_exponent = 10'b0001111111 + (({2'b0, _operand1[30:23]} + {'b0, lv_op1_subnormal}) - ({2'b0, _operand2[30:23]} + {'b0, lv_op2_subnormal}));


		Int#(10) lv_actual_exponent = unpack(lv_exponent - 10'b0001111111);
		// $display("lv_exponent = %b, lv_actual_exponent = %0d", lv_exponent, lv_actual_exponent);


	    ff_stage1.enq( Stage1_type  {	    exponent		: lv_exponent,
	  			               				dividend		: {~lv_op1_subnormal, _operand1[22:0]},
				       	        			divisor			: {~lv_op2_subnormal, _operand2[22:0]},
                                        	sign        	: lv_sign,
        				        			invalid			: lv_inv,
        				        			infinity		: lv_inf,
        				        			dz	    		: lv_dz,
        				        			zero			: lv_zero,
        				        			fsr		    	: fsr,
											rounding_mode 	: rounding_mode
                                     });
    
	endmethod

    // Output method which send the result
	method Floating_output final_result_();
	    return ff_final_out.first();
	endmethod
	
    // This method needs to be called whenever the method final_result is called.
    // This method frees the final FIFO and resets the ready signal.
    method Action _deque_buffer_reset_ready_signal();
        ff_final_out.deq();
    endmethod

endmodule

		/*		TEST BENCH 		*/

(*synthesize*)
module mkTb_fpu_spfloating_divider(Empty);
	Reg#(Bit#(32)) rg_operand1<-mkReg(32'hB412D941); 
	Reg#(Bit#(32)) rg_operand2<-mkReg(32'hFEB06562); 
	Reg#(Bit#(32)) rg_clock<-mkReg(0); 
	Ifc_fpu_spfloating_divider divider<-mkfpu_spfloating_divider();

	Reg#(Bit#(32)) rg_arbit <-mkReg(0);

	rule rl_clk_count;
		rg_clock<=rg_clock+1;
	endrule


	rule rl_start_1(rg_clock=='d0);
		// $display("Giving inputs rg_operand 1 : %b rg_operand 2 : %b through testbench",rg_operand1,rg_operand2);
		divider._start(rg_operand1,rg_operand2,{'b0,3'b000,5'b0},3'b010);
	endrule

	rule rl_display_result;
		let abc = divider.final_result_();
		divider._deque_buffer_reset_ready_signal();

		// $display("Final result= %h, fsr: %h", abc.final_result, abc.fsr);
	endrule

	rule rl_finish_(rg_clock=='d40);
		$finish(0);
	endrule

endmodule:mkTb_fpu_spfloating_divider



module mkTb_fpu_spfloating_divider_2(Empty);
	
	RegFile #(Bit #(10), Bit #(68))  input_data <- mkRegFileFullLoad("regfile_operands1.hex");
	Reg #(Bit #(10)) index <- mkReg(0);
	
	Reg #(Bit #(32)) state_clock <- mkReg(1);
 
 	/*****************Module Instantiation******************************/
	Ifc_fpu_spfloating_divider divider <- mkfpu_spfloating_divider();


	/******************File Creation************************************/
	Reg#(int) cnt <- mkReg(0);                  //File Creation counter
	let fh <- mkReg(InvalidFile) ;				//File Handler
	rule open (cnt == 0 ) ;
		File tb_mul_output <- $fopen("tb_div_output.hex", "w+"); 
		fh <= tb_mul_output;
		cnt <= 1 ;
	endrule

	/******************clock_count**************************************/
	rule state_clock_count;
		state_clock <= state_clock + 1;
	endrule

	/*******************input******************************************/
	rule take_input_in (state_clock <= 406);
		divider._start(input_data.sub(index)[67:36],input_data.sub(index)[35:4],0,input_data.sub(index)[2:0]);
		index <= index + 1;
	endrule

	/*******************output*****************************************/
	rule display_output;
		let abc = divider.final_result_();
		divider._deque_buffer_reset_ready_signal();

		$fwrite(fh, "%h\n", abc.final_result[31:0]);
	endrule

	/******************end testing*************************************/
	rule end_testing (state_clock == 850);
		$finish(0);
	endrule

endmodule
endpackage
