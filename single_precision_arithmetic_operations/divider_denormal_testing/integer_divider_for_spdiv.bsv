/*

Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



Module Name     : Integer Divider for Single Precision Floating Point Divider
Author Name     : Arjun C. Menon, Aditya Govardhan, Vinod.G
Email ID        : c.arjunmenon@gmail.com, dtgovardhan@gmail.com, g.vinod1993@gmail.com
Last updated on : 12th July, 2016

*/

package integer_divider_for_spdiv;


import FIFO :: *;
import SpecialFIFOs :: *;

typedef struct{
	Bit#(83) data;
	Bit#(10) exponent;
	Bit#(1) sign;
	Bit#(1) infinity;
	Bit#(1) invalid;
	Bit#(1) dz;
	Bit#(1) zero;
	Bit#(32) fsr;
	Bit#(3) rounding_mode;
	}Intermediate_data deriving (Bits,Eq);

(* noinline *)
function Bit#(83) fn_divide_step (Bit#(27) _divisor, Bit#(29) _remainder, Bit#(27) _dividend, Bit#(1) final_stage);
	if(final_stage == 0) begin
		Bit#(56) accumulator = 0;

	    if(_remainder[28]==1'b0) begin
			accumulator = ({_remainder,_dividend}<<1) - {1'b0,_divisor,1'b0,27'b0} ;
			accumulator[0] = 1'b1;
		end
		else begin
			accumulator = ({_remainder,_dividend}<<1) + {1'b0,_divisor,1'b0,27'b0} ;
			accumulator[0] = 1'b0;
		end
		_remainder = accumulator[55:27];
		_dividend = accumulator[26:0];
	end
	else begin
	    _dividend = _dividend - (_dividend ^ ('d-1));

	    if(_remainder[28] == 1'b1) begin
		    _remainder = _remainder + {1'b0,_divisor,1'b0};
		    _dividend = _dividend - 1;
	    end
	end
	return {_divisor, _remainder, _dividend};
endfunction

interface Ifc_integer_divider_for_spdiv;

	method Action _inputs(Bit#(27) _denominator, Bit#(27) _numerator, Bit#(10) exponent, Bit#(1) sign, Bit#(1) infinity, Bit#(1) invalid, Bit#(1) dz, Bit#(1) zero, Bit#(32) fsr, Bit#(3) rounding_mode);
	method Intermediate_data result_();
	method Action _remove_last_entry();

endinterface

(* synthesize *)
module mkinteger_divider_for_spdiv(Ifc_integer_divider_for_spdiv);

	FIFO#(Intermediate_data) ff_stage1 <-mkPipelineFIFO();         //SizedFIFO is used since PipelineFIFO causes compilation error due to dependency between two rules
	FIFO#(Intermediate_data) ff_stage2 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage3 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage4 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage5 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage6 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage7 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage8 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage9 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage10 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage11 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage12 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage13 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage14 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage15 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage16 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage17 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage18 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage19 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage20 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage21 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage22 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage23 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage24 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage25 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage26 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage27 <-mkPipelineFIFO();
	FIFO#(Intermediate_data) ff_stage28 <-mkPipelineFIFO();

	rule rl_ff_stage_2;
		ff_stage2.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage1.first().data[82:56],ff_stage1.first().data[55:27],ff_stage1.first().data[26:0], 0), 
						exponent: ff_stage1.first().exponent,
						sign: ff_stage1.first().sign,
						infinity: ff_stage1.first().infinity,
						invalid: ff_stage1.first().invalid,
						dz: ff_stage1.first().dz,
						zero: ff_stage1.first().zero,
						fsr: ff_stage1.first().fsr,
						rounding_mode: ff_stage1.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage9.first()[55:29],ff_stage9.first()[28:0]);
		ff_stage1.deq();
	endrule:rl_ff_stage_2

	rule rl_ff_stage_3;
		ff_stage3.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage2.first().data[82:56],ff_stage2.first().data[55:27],ff_stage2.first().data[26:0], 0), 
						exponent: ff_stage2.first().exponent,
						sign: ff_stage2.first().sign,
						infinity: ff_stage2.first().infinity,
						invalid: ff_stage2.first().invalid,
						dz: ff_stage2.first().dz,
						zero: ff_stage2.first().zero,
						fsr: ff_stage2.first().fsr,
						rounding_mode: ff_stage2.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage2.first()[55:29],ff_stage2.first()[28:0]);
		ff_stage2.deq();
	endrule:rl_ff_stage_3

	rule rl_ff_stage_4;
		ff_stage4.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage3.first().data[82:56],ff_stage3.first().data[55:27],ff_stage3.first().data[26:0], 0), 
						exponent: ff_stage3.first().exponent,
						sign: ff_stage3.first().sign,
						infinity: ff_stage3.first().infinity,
						invalid: ff_stage3.first().invalid,
						dz: ff_stage3.first().dz,
						zero: ff_stage3.first().zero,
						fsr: ff_stage3.first().fsr,
						rounding_mode: ff_stage3.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage3.first()[55:29],ff_stage3.first()[28:0]);
		ff_stage3.deq();
	endrule:rl_ff_stage_4

	rule rl_ff_stage_5;
		ff_stage5.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage4.first().data[82:56],ff_stage4.first().data[55:27],ff_stage4.first().data[26:0], 0), 
						exponent: ff_stage4.first().exponent,
						sign: ff_stage4.first().sign,
						infinity: ff_stage4.first().infinity,
						invalid: ff_stage4.first().invalid,
						dz: ff_stage4.first().dz,
						zero: ff_stage4.first().zero,
						fsr: ff_stage4.first().fsr,
						rounding_mode: ff_stage4.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage4.first()[55:29],ff_stage4.first()[28:0]);
		ff_stage4.deq();
	endrule:rl_ff_stage_5

	rule rl_ff_stage_6;
		ff_stage6.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage5.first().data[82:56],ff_stage5.first().data[55:27],ff_stage5.first().data[26:0], 0), 
						exponent: ff_stage5.first().exponent,
						sign: ff_stage5.first().sign,
						infinity: ff_stage5.first().infinity,
						invalid: ff_stage5.first().invalid,
						dz: ff_stage5.first().dz,
						zero: ff_stage5.first().zero,
						fsr: ff_stage5.first().fsr,
						rounding_mode: ff_stage5.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage5.first()[55:29],ff_stage5.first()[28:0]);
		ff_stage5.deq();
	endrule:rl_ff_stage_6

	rule rl_ff_stage_7;
		ff_stage7.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage6.first().data[82:56],ff_stage6.first().data[55:27],ff_stage6.first().data[26:0], 0), 
						exponent: ff_stage6.first().exponent,
						sign: ff_stage6.first().sign,
						infinity: ff_stage6.first().infinity,
						invalid: ff_stage6.first().invalid,
						dz: ff_stage6.first().dz,
						zero: ff_stage6.first().zero,
						fsr: ff_stage6.first().fsr,
						rounding_mode: ff_stage6.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage8.first()[55:29],ff_stage8.first()[28:0]);
		ff_stage6.deq();
	endrule:rl_ff_stage_7

	rule rl_ff_stage_8;
		ff_stage8.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage7.first().data[82:56],ff_stage7.first().data[55:27],ff_stage7.first().data[26:0], 0), 
						exponent: ff_stage7.first().exponent,
						sign: ff_stage7.first().sign,
						infinity: ff_stage7.first().infinity,
						invalid: ff_stage7.first().invalid,
						dz: ff_stage7.first().dz,
						zero: ff_stage7.first().zero,
						fsr: ff_stage7.first().fsr,
						rounding_mode: ff_stage7.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage7.first()[55:29],ff_stage7.first()[28:0]);
		ff_stage7.deq();
	endrule:rl_ff_stage_8

	rule rl_ff_stage_9;
		ff_stage9.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage8.first().data[82:56],ff_stage8.first().data[55:27],ff_stage8.first().data[26:0], 0), 
						exponent: ff_stage8.first().exponent,
						sign: ff_stage8.first().sign,
						infinity: ff_stage8.first().infinity,
						invalid: ff_stage8.first().invalid,
						dz: ff_stage8.first().dz,
						zero: ff_stage8.first().zero,
						fsr: ff_stage8.first().fsr,
						rounding_mode: ff_stage8.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage8.first()[55:29],ff_stage8.first()[28:0]);
		ff_stage8.deq();
	endrule:rl_ff_stage_9

	rule rl_ff_stage_10;
		ff_stage10.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage9.first().data[82:56],ff_stage9.first().data[55:27],ff_stage9.first().data[26:0], 0), 
						exponent: ff_stage9.first().exponent,
						sign: ff_stage9.first().sign,
						infinity: ff_stage9.first().infinity,
						invalid: ff_stage9.first().invalid,
						dz: ff_stage9.first().dz,
						zero: ff_stage9.first().zero,
						fsr: ff_stage9.first().fsr,
						rounding_mode: ff_stage9.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage9.first()[55:29],ff_stage9.first()[28:0]);
		ff_stage9.deq();
	endrule:rl_ff_stage_10

	rule rl_ff_stage_11;
		ff_stage11.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage10.first().data[82:56],ff_stage10.first().data[55:27],ff_stage10.first().data[26:0], 0), 
						exponent: ff_stage10.first().exponent,
						sign: ff_stage10.first().sign,
						infinity: ff_stage10.first().infinity,
						invalid: ff_stage10.first().invalid,
						dz: ff_stage10.first().dz,
						zero: ff_stage10.first().zero,
						fsr: ff_stage10.first().fsr,
						rounding_mode: ff_stage10.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage10.first()[55:29],ff_stage10.first()[28:0]);
		ff_stage10.deq();
	endrule:rl_ff_stage_11

	rule rl_ff_stage_12;
		ff_stage12.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage11.first().data[82:56],ff_stage11.first().data[55:27],ff_stage11.first().data[26:0], 0), 
						exponent: ff_stage11.first().exponent,
						sign: ff_stage11.first().sign,
						infinity: ff_stage11.first().infinity,
						invalid: ff_stage11.first().invalid,
						dz: ff_stage11.first().dz,
						zero: ff_stage11.first().zero,
						fsr: ff_stage11.first().fsr,
						rounding_mode: ff_stage11.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage11.first()[55:29],ff_stage11.first()[28:0]);
		ff_stage11.deq();
	endrule:rl_ff_stage_12

	rule rl_ff_stage_13;
		ff_stage13.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage12.first().data[82:56],ff_stage12.first().data[55:27],ff_stage12.first().data[26:0], 0), 
						exponent: ff_stage12.first().exponent,
						sign: ff_stage12.first().sign,
						infinity: ff_stage12.first().infinity,
						invalid: ff_stage12.first().invalid,
						dz: ff_stage12.first().dz,
						zero: ff_stage12.first().zero,
						fsr: ff_stage12.first().fsr,
						rounding_mode: ff_stage12.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage12.first()[55:29],ff_stage12.first()[28:0]);
		ff_stage12.deq();
	endrule:rl_ff_stage_13

	rule rl_ff_stage_14;
		ff_stage14.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage13.first().data[82:56],ff_stage13.first().data[55:27],ff_stage13.first().data[26:0], 0), 
						exponent: ff_stage13.first().exponent,
						sign: ff_stage13.first().sign,
						infinity: ff_stage13.first().infinity,
						invalid: ff_stage13.first().invalid,
						dz: ff_stage13.first().dz,
						zero: ff_stage13.first().zero,
						fsr: ff_stage13.first().fsr,
						rounding_mode: ff_stage13.first().rounding_mode}
						);
	    // $display("quotient: %b rem: %b",ff_stage13.first()[55:29],ff_stage13.first()[28:0]);
		ff_stage13.deq();
	endrule:rl_ff_stage_14

	rule rl_ff_stage_15;
		ff_stage15.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage14.first().data[82:56],ff_stage14.first().data[55:27],ff_stage14.first().data[26:0], 0), 
						exponent: ff_stage14.first().exponent,
						sign: ff_stage14.first().sign,
						infinity: ff_stage14.first().infinity,
						invalid: ff_stage14.first().invalid,
						dz: ff_stage14.first().dz,
						zero: ff_stage14.first().zero,
						fsr: ff_stage14.first().fsr,
						rounding_mode: ff_stage14.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage14.first()[55:29],ff_stage14.first()[28:0]);
		ff_stage14.deq();
	endrule:rl_ff_stage_15

	rule rl_ff_stage_16;
		ff_stage16.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage15.first().data[82:56],ff_stage15.first().data[55:27],ff_stage15.first().data[26:0], 0), 
						exponent: ff_stage15.first().exponent,
						sign: ff_stage15.first().sign,
						infinity: ff_stage15.first().infinity,
						invalid: ff_stage15.first().invalid,
						dz: ff_stage15.first().dz,
						zero: ff_stage15.first().zero,
						fsr: ff_stage15.first().fsr,
						rounding_mode: ff_stage15.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage15.first()[55:29],ff_stage15.first()[28:0]);
		ff_stage15.deq();
	endrule:rl_ff_stage_16

	rule rl_ff_stage_17;
		ff_stage17.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage16.first().data[82:56],ff_stage16.first().data[55:27],ff_stage16.first().data[26:0], 0), 
						exponent: ff_stage16.first().exponent,
						sign: ff_stage16.first().sign,
						infinity: ff_stage16.first().infinity,
						invalid: ff_stage16.first().invalid,
						dz: ff_stage16.first().dz,
						zero: ff_stage16.first().zero,
						fsr: ff_stage16.first().fsr,
						rounding_mode: ff_stage16.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage16.first()[55:29],ff_stage16.first()[28:0]);
		ff_stage16.deq();
	endrule:rl_ff_stage_17

	rule rl_ff_stage_18;
		ff_stage18.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage17.first().data[82:56],ff_stage17.first().data[55:27],ff_stage17.first().data[26:0], 0), 
						exponent: ff_stage17.first().exponent,
						sign: ff_stage17.first().sign,
						infinity: ff_stage17.first().infinity,
						invalid: ff_stage17.first().invalid,
						dz: ff_stage17.first().dz,
						zero: ff_stage17.first().zero,
						fsr: ff_stage17.first().fsr,
						rounding_mode: ff_stage17.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage17.first()[55:29],ff_stage17.first()[28:0]);
		ff_stage17.deq();
	endrule:rl_ff_stage_18

	rule rl_ff_stage_19;
		ff_stage19.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage18.first().data[82:56],ff_stage18.first().data[55:27],ff_stage18.first().data[26:0], 0), 
						exponent: ff_stage18.first().exponent,
						sign: ff_stage18.first().sign,
						infinity: ff_stage18.first().infinity,
						invalid: ff_stage18.first().invalid,
						dz: ff_stage18.first().dz,
						zero: ff_stage18.first().zero,
						fsr: ff_stage18.first().fsr,
						rounding_mode: ff_stage18.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage18.first()[55:29],ff_stage18.first()[28:0]);
		ff_stage18.deq();
	endrule:rl_ff_stage_19

	rule rl_ff_stage_20;
		ff_stage20.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage19.first().data[82:56],ff_stage19.first().data[55:27],ff_stage19.first().data[26:0], 0), 
						exponent: ff_stage19.first().exponent,
						sign: ff_stage19.first().sign,
						infinity: ff_stage19.first().infinity,
						invalid: ff_stage19.first().invalid,
						dz: ff_stage19.first().dz,
						zero: ff_stage19.first().zero,
						fsr: ff_stage19.first().fsr,
						rounding_mode: ff_stage19.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage19.first()[55:29],ff_stage19.first()[28:0]);
		ff_stage19.deq();
	endrule:rl_ff_stage_20

	rule rl_ff_stage_21;
		ff_stage21.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage20.first().data[82:56],ff_stage20.first().data[55:27],ff_stage20.first().data[26:0], 0), 
						exponent: ff_stage20.first().exponent,
						sign: ff_stage20.first().sign,
						infinity: ff_stage20.first().infinity,
						invalid: ff_stage20.first().invalid,
						dz: ff_stage20.first().dz,
						zero: ff_stage20.first().zero,
						fsr: ff_stage20.first().fsr,
						rounding_mode: ff_stage20.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage20.first()[55:29],ff_stage20.first()[28:0]);
		ff_stage20.deq();
	endrule:rl_ff_stage_21

	rule rl_ff_stage_22;
		ff_stage22.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage21.first().data[82:56],ff_stage21.first().data[55:27],ff_stage21.first().data[26:0], 0), 
						exponent: ff_stage21.first().exponent,
						sign: ff_stage21.first().sign,
						infinity: ff_stage21.first().infinity,
						invalid: ff_stage21.first().invalid,
						dz: ff_stage21.first().dz,
						zero: ff_stage21.first().zero,
						fsr: ff_stage21.first().fsr,
						rounding_mode: ff_stage21.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage21.first()[55:29],ff_stage21.first()[28:0]);
		ff_stage21.deq();
	endrule:rl_ff_stage_22

	rule rl_ff_stage_23;
		ff_stage23.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage22.first().data[82:56],ff_stage22.first().data[55:27],ff_stage22.first().data[26:0], 0), 
						exponent: ff_stage22.first().exponent,
						sign: ff_stage22.first().sign,
						infinity: ff_stage22.first().infinity,
						invalid: ff_stage22.first().invalid,
						dz: ff_stage22.first().dz,
						zero: ff_stage22.first().zero,
						fsr: ff_stage22.first().fsr,
						rounding_mode: ff_stage22.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage22.first()[55:29],ff_stage22.first()[28:0]);
		ff_stage22.deq();
	endrule:rl_ff_stage_23

	rule rl_ff_stage_24;
		ff_stage24.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage23.first().data[82:56],ff_stage23.first().data[55:27],ff_stage23.first().data[26:0], 0), 
						exponent: ff_stage23.first().exponent,
						sign: ff_stage23.first().sign,
						infinity: ff_stage23.first().infinity,
						invalid: ff_stage23.first().invalid,
						dz: ff_stage23.first().dz,
						zero: ff_stage23.first().zero,
						fsr: ff_stage23.first().fsr,
						rounding_mode: ff_stage23.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage23.first()[55:29],ff_stage23.first()[28:0]);
		ff_stage23.deq();
	endrule:rl_ff_stage_24

	rule rl_ff_stage_25;
		ff_stage25.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage24.first().data[82:56],ff_stage24.first().data[55:27],ff_stage24.first().data[26:0], 0), 
						exponent: ff_stage24.first().exponent,
						sign: ff_stage24.first().sign,
						infinity: ff_stage24.first().infinity,
						invalid: ff_stage24.first().invalid,
						dz: ff_stage24.first().dz,
						zero: ff_stage24.first().zero,
						fsr: ff_stage24.first().fsr,
						rounding_mode: ff_stage24.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage24.first()[55:29],ff_stage24.first()[28:0]);
		ff_stage24.deq();
	endrule:rl_ff_stage_25

	rule rl_ff_stage_26;
		ff_stage26.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage25.first().data[82:56],ff_stage25.first().data[55:27],ff_stage25.first().data[26:0], 0), 
						exponent: ff_stage25.first().exponent,
						sign: ff_stage25.first().sign,
						infinity: ff_stage25.first().infinity,
						invalid: ff_stage25.first().invalid,
						dz: ff_stage25.first().dz,
						zero: ff_stage25.first().zero,
						fsr: ff_stage25.first().fsr,
						rounding_mode: ff_stage25.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage25.first()[55:29],ff_stage25.first()[28:0]);
		ff_stage25.deq();
	endrule:rl_ff_stage_26

	rule rl_ff_stage_27;
		ff_stage27.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage26.first().data[82:56],ff_stage26.first().data[55:27],ff_stage26.first().data[26:0], 0), 
						exponent: ff_stage26.first().exponent,
						sign: ff_stage26.first().sign,
						infinity: ff_stage26.first().infinity,
						invalid: ff_stage26.first().invalid,
						dz: ff_stage26.first().dz,
						zero: ff_stage26.first().zero,
						fsr: ff_stage26.first().fsr,
						rounding_mode: ff_stage26.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage26.first()[55:29],ff_stage26.first()[28:0]);
		ff_stage26.deq();
	endrule:rl_ff_stage_27

	rule rl_ff_stage_28;
		ff_stage28.enq(	Intermediate_data{
						data: fn_divide_step(ff_stage27.first().data[82:56],ff_stage27.first().data[55:27],ff_stage27.first().data[26:0], 1),
						exponent: ff_stage27.first().exponent,
						sign: ff_stage27.first().sign,
						infinity: ff_stage27.first().infinity,
						invalid: ff_stage27.first().invalid,
						dz: ff_stage27.first().dz,
						zero: ff_stage27.first().zero,
						fsr: ff_stage27.first().fsr,
						rounding_mode: ff_stage27.first().rounding_mode}
						);
		// $display("quotient: %b rem: %b",ff_stage27.first()[55:29],ff_stage27.first()[28:0]);
		ff_stage27.deq();
	endrule:rl_ff_stage_28

	method Action _inputs(Bit#(27) _denominator, Bit#(27) _numerator, Bit#(10) exponent, Bit#(1) sign, Bit#(1) infinity, Bit#(1) invalid, Bit#(1) dz, Bit#(1) zero, Bit#(32) fsr, Bit#(3) rounding_mode);
		ff_stage1.enq(	Intermediate_data{
						data: fn_divide_step(_denominator, {2'b0,_numerator}, 27'b0, 0), 
						exponent: exponent, 
						sign: sign, 
						infinity: infinity, 
						invalid: invalid, 
						dz: dz, 
						zero: zero, 
						fsr: fsr, 
						rounding_mode: rounding_mode}
						);
	endmethod

	method Intermediate_data result_();
		return ff_stage28.first();
	endmethod

	method Action _remove_last_entry();
		ff_stage28.deq();
	endmethod

endmodule

// (*synthesize*)
// module mkTb (Empty);

//     Reg#(Bit#(32)) rg_clock <-mkReg(0);

//     Ifc_integer_divider_for_spdiv instance_divider <-mkinteger_divider_for_spdiv();

//     rule rl_count_clock ;
//       	rg_clock<=rg_clock+1;
//       	if(rg_clock=='d45) $finish(0);
//     endrule

//     rule rl_input1(rg_clock==1);
// 		$display("giving inputat %0d", rg_clock);
//         instance_divider._inputs(27'd7, 27'd36 ); // divisor, dividend 
//     endrule

//     rule rl_finish;
// 		$display("Quotient=%b mainder=%b at %0d",instance_divider.output_quotient,instance_divider.output_remainder, rg_clock);
//         instance_divider._remove_last_entry();
//     endrule

// endmodule

endpackage
