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


Module Name: Interger Multiplier used in Floating Point Unit.
Author Name: Neel Gala , Kalyan Kumar, Aditya Govardhan
Email Id: neelgala@gmail.com kalyavkumar.suvvada@gmail.com dtgovardhan@gmail.com
Last updated on : 27th June, 2016

This is a pipelined integer multiplier that multiplies in four clock cycles. 
Pipelining is implemented using FIFO registers in between the different stages of the multiplier.
This helps in improving the through put of the multiplier.

The Four stages of the multiplier are as follows

Stage 1:

The Booth's 2nd algorithm is used to generate the partial products. Since the input size is 32 bits, the number of partial products generated is 16. Alternate partial products are negated in order to generate redundant binary numbers. Two more partial products are used for Redundant binary addition. These partial products are then stored in the FIFO 'stage2'.

Stage 2:

The Redundant binary numbers are generated by grouping the i th and the (i+1)th partial products. Then these Redundant Binary Partial Products are added using a Wallace tree of redundant binary adders.Due to the high delay of the RB Adders, the wallace tree multiplication is split into two cycles.

The leaf of the wallace tree has 4 RB Adders that adds 8 RB numbers producing 4 RB numbers. These four RB numbers are the inputs to the 2 RB Adders in the next level of the wallace tree. Finally, this stage produces 2 RB. Now, totally we have added 8 RB numbers. We still have 1 un_added RB number and two results produced from this stage.

Stage 3
 These 3 RB numbers are added with a 2 level RB Adder with one RB Adder in each level. This stage finally produces the result of the addition process. The result, also a redundant binary number, is stored in the FIFO 'stage3'.

Stage 4

The redundant binary product generated in the previous stage is converted into Normal Binary form by using RB to NB converter. The resultant product is stored in the FIFO 'prod'.

*/

package integermultiplier_for_dpfpu;

import RBA::*;
import Vector::*;
import FIFO::*;

// Function to generate the Booth's partial products.
(*noinline*)
function Bit#(128) gen_pp(Bit#(128) multp, Bit#(128) lmultp, Bit#(3) sel, int inv);
	
	Bit#(128) result='d0;
	case(sel)
		'd0: result=128'b0; 	//zero
		'd1: result=multp; 	// M
		'd2: result=multp; 	// M
		'd3: result=lmultp;	// 2M
		'd4: result=~lmultp+1;	// -2M			
		'd5: result=~multp+1;	// -M
		'd6: result=~multp+1;	// -M
		'd7: result=128'b0;	// zero
	endcase

	//This inversion of alternate partial products is to obtain the redundant binary numbers from this and the previous partial product.
	if(inv==1)
		result=~result;
	
	return result;

endfunction

// The interface for the Integer Multiplier which accepts 2 32 bit operands(signed) and produces the 64 bit product as the output
interface Ifc_integer_multiplier_for_dpfmul;
		/* Input Methods */
	method Action _start( Bit#(64) _operand1, Bit#(64) _operand2 , Bit#(5) _destination_address, Bit#(32) _program_counter, Bit#(32) _fsr, Bit#(3) rounding_mode, Bit#(4) _rob_number, bit _sign, Bit#(13) _summed_exponent, bit _invalid, Bit#(1) _infinity, Bit#(1) _zero);
	method Action _deque();
	method Action _set_flush(Bool _flush);

		/* Output Methods */
	method Stage5_data result_();
endinterface:Ifc_integer_multiplier_for_dpfmul

typedef struct{
	Bit#(5) destination;		// Register File address of the destination
	Bit#(4) rob_number;		// The rob number of the instruction
	Bit#(32) fsr;			// the input fsr
    Bit#(3) rounding_mode;          // static rounding mode encoded in the instruction
	Bit#(32) program_counter;	// the Program counter of the Instruction
	Bit#(13) summed_exponent;	// the sum of the exponents
	bit sign;			// the sign bit
	Bit#(1) infinity;		// the infinity Flag
	Bit#(1) invalid;		// the Invalid Flag
	Bit#(1) zero;			// The Zero Flag
	Vector#(32,Bit#(128)) stage1;	// partial product array.
	Bit#(128) stage1_extra_pp;
	}Stage1_data deriving(Eq,Bits);

typedef struct{
	Bit#(5) destination;		// Register File address of the destination
	Bit#(4) rob_number;             // The rob number of the instruction
	Bit#(32) fsr;                   // the input fsr
    Bit#(3) rounding_mode;          // static rounding mode encoded in the instruction
	Bit#(32) program_counter;       // the Program counter of the Instruction
	Bit#(13) summed_exponent;	// the sum of the exponents.
	bit sign;			// the sign bit of the result
	Bit#(1) infinity;		// the infinity Flag
	Bit#(1) invalid;                // the Invalid Flag
	Bit#(1) zero;                   // The Zero Flag
	Bit#(256) stage2;
	Vector#(16,Bit#(128)) vector_pp;
	Bit#(128) stage2_extra_pp;
	}Stage2_data deriving(Eq,Bits);

typedef struct{
	Bit#(5) destination;		// Register File address of the destination
	Bit#(4) rob_number;             // The rob number of the instruction
	Bit#(32) fsr;                   // the input fsr
    Bit#(3) rounding_mode;          // static rounding mode encoded in the instruction
	Bit#(32) program_counter;       // the Program counter of the Instruction
	Bit#(13) summed_exponent;	// the sum of the exponents.
	bit sign;			// the sign bit of the result
	Bit#(1) infinity;		// the infinity Flag
	Bit#(1) invalid;                // the Invalid Flag
	Bit#(1) zero;                   // The Zero Flag
	Bit#(256) stage2_data_forward;
	Bit#(256) stage3;
	Bit#(128) stage3_extra_pp;
	}Stage3_data deriving(Eq,Bits);

typedef struct{
	Bit#(5) destination;		// Register File address of the destination
	Bit#(4) rob_number;             // The rob number of the instruction
	Bit#(32) fsr;                   // the input fsr
    Bit#(3) rounding_mode;          // static rounding mode encoded in the instruction
	Bit#(32) program_counter;       // the Program counter of the Instruction
	Bit#(13) summed_exponent;	// the sum of the exponents.
	bit sign;			// the sign bit of the result
	Bit#(1) infinity;		// the infinity Flag
	Bit#(1) invalid;                // the Invalid Flag
	Bit#(1) zero;                   // The Zero Flag
	Bit#(256) stage4;
	Bit#(128) stage4_extra_pp;
	}Stage4_data deriving(Eq,Bits);

typedef struct{
	Bit#(5) destination;		// Register File address of the destination
	Bit#(4) rob_number;             // The rob number of the instruction
	Bit#(32) fsr;                   // the input fsr
    Bit#(3) rounding_mode;          // static rounding mode encoded in the instruction
	Bit#(32) program_counter;       // the Program counter of the Instruction
	Bit#(13) summed_exponent;	// the sum of the exponents.
	bit sign;			// the sign bit of the result
	Bit#(1) infinity;		// the infinity Flag
	Bit#(1) invalid;                // the Invalid Flag
	Bit#(1) zero;                   // The Zero Flag
	Bit#(128) final_result;
	}Stage5_data deriving(Eq,Bits);

module mkinteger_multiplier_for_dpfmul(Ifc_integer_multiplier_for_dpfmul);
	Wire#(Bool) wr_flush <-mkDWire(False);
	FIFO#(Stage1_data) ff_stage1 <- mkFIFO;  //This FIFO stores the 16 64 bit partial products produced in the previous cycle
	FIFO#(Stage2_data) ff_stage2 <- mkFIFO;	 //This FIFO stores the 3 Redundant binary numbers left to be added.3x2x64=384 bits
	FIFO#(Stage3_data) ff_stage3 <- mkFIFO;	 //This FIFO stores the result of the Redundant Binary Addition of the Redundant Binary Partial Products
	FIFO#(Stage4_data) ff_stage4 <- mkFIFO;	 //This FIFO stores the result of the Redundant Binary Addition of the Redundant Binary Partial Products
	FIFO#(Stage5_data) ff_final_result <- mkFIFO;//This FIFO stores the result of the multiplication of the two operands after converting the RB number to NB

	/* This rule is fired only when the flush signal is invoked.
	Here all teh fifos are cleared and thus any comutation is abandoned.
	*/
	rule rl_flush_data(wr_flush);
		ff_stage1.clear();
		ff_stage2.clear();
		ff_stage3.clear();
		ff_stage4.clear();
		ff_final_result.clear();
	endrule:rl_flush_data

	//Stage 2
	rule rl_wallace_tree_part1(!wr_flush);
		//$display("Executing Integer MUL stage 2"); 
		let lv_info1=ff_stage1.first;    //Get the 32 partial products from FIFO stage1
		ff_stage1.deq;	
		Vector#(16,Bit#(128)) upper_half = take(lv_info1.stage1);
		let lv_result_stage2=wallace_rba(upper_half);

		// The 3 redundant binary partial products, 2 from the result of this stage and one from the previous stage.
		ff_stage2.enq(Stage2_data{	destination:lv_info1.destination,
									rob_number:lv_info1.rob_number,
									fsr:lv_info1.fsr,
				                    rounding_mode : lv_info1.rounding_mode,
									program_counter:lv_info1.program_counter,
									summed_exponent:lv_info1.summed_exponent,
									sign : lv_info1.sign,
									infinity:lv_info1.infinity,
									invalid:lv_info1.invalid,
									zero:lv_info1.zero,
									stage2:lv_result_stage2,
									vector_pp:takeTail(lv_info1.stage1),
									stage2_extra_pp: lv_info1.stage1_extra_pp
						});

	endrule:rl_wallace_tree_part1

	//Stage 3
	rule rl_wallace_tree_part2(!wr_flush);
		//$display("Executing Integer MUL stage 3");
		let lv_info2=ff_stage2.first;	//Get result of additon of 16 pp and the 32 pp's for next step of addition
		let lv_result_stage3= wallace_rba(lv_info2.vector_pp);
		ff_stage2.deq;

		ff_stage3.enq(Stage3_data{	destination:lv_info2.destination,
									rob_number:lv_info2.rob_number,
									fsr:lv_info2.fsr,
				                    rounding_mode : lv_info2.rounding_mode,
									program_counter:lv_info2.program_counter,
									summed_exponent:lv_info2.summed_exponent,
									sign : lv_info2.sign,
									infinity:lv_info2.infinity,
									invalid:lv_info2.invalid,
									zero:lv_info2.zero,
									stage2_data_forward:lv_info2.stage2,
									stage3: lv_result_stage3,
									stage3_extra_pp: lv_info2.stage2_extra_pp
					});
	endrule:rl_wallace_tree_part2

	rule rl_wallace_tree_final(!wr_flush);
		//$display("Executing Integer MUL stage 4");
		let lv_info3=ff_stage3.first;		// Get result of additon of both 1st 16 pp and next 16 pp
		let lv_result_stage4=wallace_rba_final(lv_info3.stage2_data_forward,lv_info3.stage3); // Final RB number. An extra RB number is added in function due to logic in redundant binary addition
		ff_stage3.deq;

		ff_stage4.enq(Stage4_data{	destination:lv_info3.destination,
									rob_number:lv_info3.rob_number,
									fsr:lv_info3.fsr,
				                	rounding_mode : lv_info3.rounding_mode,
									program_counter:lv_info3.program_counter,
									summed_exponent:lv_info3.summed_exponent,
									sign :lv_info3.sign,
									infinity:lv_info3.infinity,
									invalid:lv_info3.invalid,
									zero:lv_info3.zero,
									stage4: lv_result_stage4,
									stage4_extra_pp:lv_info3.stage3_extra_pp
						});

	endrule:rl_wallace_tree_final

	rule rl_rb_nb(!wr_flush);
		// The Redundant Binary product generated in the previous stage is converted into the normal binary form.
		
		let lv_info4= ff_stage4.first(); // Get RB number generated in stage 4
		let normal_binary_product = lv_info4.stage4[255:128] + (~lv_info4.stage4[127:0] +1) + lv_info4.stage4_extra_pp;
		ff_stage4.deq();

		ff_final_result.enq (Stage5_data{	destination:lv_info4.destination,
											rob_number:lv_info4.rob_number,
											fsr:lv_info4.fsr,
							                rounding_mode:lv_info4.rounding_mode,
											program_counter:lv_info4.program_counter,
											summed_exponent:lv_info4.summed_exponent,
											sign :lv_info4.sign,
											infinity:lv_info4.infinity,
											invalid:lv_info4.invalid,
											zero:lv_info4.zero,
											final_result:normal_binary_product
						});
	
	endrule:rl_rb_nb



	method Action _start( Bit#(64) _operand1, Bit#(64) _operand2 , Bit#(5) _destination_address, Bit#(32) _program_counter, Bit#(32) _fsr, Bit#(3) _rounding_mode, Bit#(4) _rob_number, bit _sign, Bit#(13) _summed_exponent, bit _invalid, Bit#(1) _infinity, Bit#(1) _zero);

		Bit#(67) lv_mult={2'b00,_operand1,0};		// lv_mult     = multiplier appended with a zero and extended with zeros, a requirement of booth's unsigned multiplication of second order algorithm
		Bit#(128) lv_multp=zeroExtend(_operand2);	// lv_multp    = multiplicand
                
	
		Bit#(128) lv_lmultp = lv_multp<<1;	// lv_lmultp    = multiplicand left shifted one bit position/ twice of multiplicand requied for booth encoding
		/*	
		 Generation of the 32 partial products using Booth's second order algorithm. The i+1 i i-1 th bits are used as the select bits
		 These 32 partial products are converted into redundant binary partial products by taking two at a time.i.e every adjacent partial product.
		 To form a RB number, the two's complement of the second number is needed. So, every odd numbered partial product is inverted. To avoid an                   addition of 1
		 which will involve a a carry propagation, we take these 16 1's along with a 128'b0 to form the 9th partial product.
		 The last two bit positions is due to the left shifting of two bits for adjacent partial products. Since, the odd numbered partial products
		 are inverted, we append one's. For even numbered partial products, we append zeroes.
		*/
		
		Vector#(32,Bit#(128)) lv_pp = replicate(0); // Creating a vector for 32 128 bit pp's
		Bit#(128) lv_extra_pp = 0; //Extra partial product generated due to the 2 zero bit extended in multiplier for unsigned multiplication

		lv_pp[0]=gen_pp(lv_multp,lv_lmultp,lv_mult[2:0],0);
		lv_pp[1]=(gen_pp(lv_multp,lv_lmultp,lv_mult[4:2],1)<<2 | 'b11);
		lv_pp[2]=(gen_pp(lv_multp,lv_lmultp,lv_mult[6:4],0)<<4);
		lv_pp[3]=(gen_pp(lv_multp,lv_lmultp,lv_mult[8:6],1)<<2 | 'b11)<<4;
		lv_pp[4]=(gen_pp(lv_multp,lv_lmultp,lv_mult[10:8],0)<<8);
		lv_pp[5]=(gen_pp(lv_multp,lv_lmultp,lv_mult[12:10],1)<<2 | 'b11)<<8;
		lv_pp[6]=(gen_pp(lv_multp,lv_lmultp,lv_mult[14:12],0)<<12);
		lv_pp[7]=(gen_pp(lv_multp,lv_lmultp,lv_mult[16:14],1)<<2 | 'b11)<<12;
		lv_pp[8]=(gen_pp(lv_multp,lv_lmultp,lv_mult[18:16],0)<<16);
		lv_pp[9]=(gen_pp(lv_multp,lv_lmultp,lv_mult[20:18],1)<<2 | 'b11)<<16;
		lv_pp[10]=(gen_pp(lv_multp,lv_lmultp,lv_mult[22:20],0)<<20);
		lv_pp[11]=(gen_pp(lv_multp,lv_lmultp,lv_mult[24:22],1)<<2 | 'b11)<<20;
		lv_pp[12]=(gen_pp(lv_multp,lv_lmultp,lv_mult[26:24],0)<<24);
		lv_pp[13]=(gen_pp(lv_multp,lv_lmultp,lv_mult[28:26],1)<<2 | 'b11)<<24;
		lv_pp[14]=(gen_pp(lv_multp,lv_lmultp,lv_mult[30:28],0)<<28);
		lv_pp[15]=(gen_pp(lv_multp,lv_lmultp,lv_mult[32:30],1)<<2 | 'b11)<<28;
		lv_pp[16]=(gen_pp(lv_multp,lv_lmultp,lv_mult[34:32],0)<<32);
		lv_pp[17]=(gen_pp(lv_multp,lv_lmultp,lv_mult[36:34],1)<<2 | 'b11)<<32;
		lv_pp[18]=(gen_pp(lv_multp,lv_lmultp,lv_mult[38:36],0)<<36);
		lv_pp[19]=(gen_pp(lv_multp,lv_lmultp,lv_mult[40:38],1)<<2 | 'b11)<<36;
		lv_pp[20]=(gen_pp(lv_multp,lv_lmultp,lv_mult[42:40],0)<<40);
		lv_pp[21]=(gen_pp(lv_multp,lv_lmultp,lv_mult[44:42],1)<<2 | 'b11)<<40;
		lv_pp[22]=(gen_pp(lv_multp,lv_lmultp,lv_mult[46:44],0)<<44);
		lv_pp[23]=(gen_pp(lv_multp,lv_lmultp,lv_mult[48:46],1)<<2 | 'b11)<<44;
		lv_pp[24]=(gen_pp(lv_multp,lv_lmultp,lv_mult[50:48],0)<<48);
		lv_pp[25]=(gen_pp(lv_multp,lv_lmultp,lv_mult[52:50],1)<<2 | 'b11)<<48;
		lv_pp[26]=(gen_pp(lv_multp,lv_lmultp,lv_mult[54:52],0)<<52);
		lv_pp[27]=(gen_pp(lv_multp,lv_lmultp,lv_mult[56:54],1)<<2 | 'b11)<<52;
		lv_pp[28]=(gen_pp(lv_multp,lv_lmultp,lv_mult[58:56],0)<<56);
		lv_pp[29]=(gen_pp(lv_multp,lv_lmultp,lv_mult[60:58],1)<<2 | 'b11)<<56;
		lv_pp[30]=(gen_pp(lv_multp,lv_lmultp,lv_mult[62:60],0)<<60);
		lv_pp[31]=(gen_pp(lv_multp,lv_lmultp,lv_mult[64:62],1)<<2 | 'b11)<<60;

		lv_extra_pp[127:64] = gen_pp(lv_multp,lv_lmultp,lv_mult[66:64],0)[63:0];
		
		// The 16 partial products are stored in the stage1 FIFO
		ff_stage1.enq(Stage1_data{	destination:_destination_address,
									rob_number:_rob_number,
									fsr:_fsr,
					                rounding_mode:_rounding_mode,
									program_counter:_program_counter,
									summed_exponent:_summed_exponent,
									sign :_sign,
									infinity:_infinity,
									invalid:_invalid,
									zero:_zero,
									stage1:lv_pp,
									stage1_extra_pp:lv_extra_pp});
	endmethod

	method Stage5_data result_();
		return ff_final_result.first();
	endmethod

	method Action _deque();
		ff_final_result.deq();
	endmethod
	method Action _set_flush(Bool _flush);
		wr_flush<=_flush;
	endmethod
endmodule:mkinteger_multiplier_for_dpfmul

(*synthesize*)
module mkTb_integer_multiplier_for_dpfmul(Empty);

	Reg#(Bit#(32)) rg_clock <-mkReg(0);
	Ifc_integer_multiplier_for_dpfmul be <- mkinteger_multiplier_for_dpfmul();

	rule rl_increment;
		rg_clock<=rg_clock+1;
		$display("CLOCK=%d",rg_clock);

		if(rg_clock=='d20)
		$finish(0);
	endrule:rl_increment
   
	rule rl_give_input(rg_clock=='d1);
		be._start(64'h5f648cdf9885e275, 64'hc20961e24501712e, 0, 0, 0, 0,0,0,0,0,0,0);
		$display("%b %b", 64'h5f648cdf9885e275, 64'hc20961e24501712e);
	endrule:rl_give_input

	rule rl_take_output;
	$display("final result: %b",be.result_().final_result);
	endrule

endmodule:mkTb_integer_multiplier_for_dpfmul 
endpackage:integermultiplier_for_dpfpu