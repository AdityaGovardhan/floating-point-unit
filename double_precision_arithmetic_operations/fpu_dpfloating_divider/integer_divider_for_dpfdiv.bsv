/*
-------------------------------------------------------------------------------

Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
-------------------------------------------------------------------------------

Module Name     : Integer Divider used in DP FP division 
Author Name     : Arjun C. Menon, Aditya Govardhan
Email Id        : c.arjunmenon@gmail.com, dtgovardhan@gmail.com
Last updaed on  : 2nd June, 2016.

This module performs the integer division used in the DP FP division calculation. The dividend is 114 bits wide and the divisor is 56 bits.
This division is performed in 28 steps or 28 clock cycles. During every clock cycle, we perform two steps of division by subtraction.
This is performed using the function fn_divide_step. At each cycle the copmuted value is enqueued in the next FIFO in the pipe.

*/

package integer_divider_for_dpfdiv;
import FIFO::*;
import SpecialFIFOs::*;

//function to perform two steps of division by subtraction
/*(*noinline*)
function Bit#(170) fn_divide_step(Bit#(56) denominator, Bit#(114) quotient_and_remainder);
	let temp=quotient_and_remainder;
    let x= temp[56:0]-{1'b0,denominator};
	if(quotient_and_remainder[56:0]<{1'b0,denominator})         // if Dr > Nr
		temp= {temp[113:58],1'b0,temp[56:0]};     // Quotient bit is 0
	else
		temp = {temp[113:58],1'b1,x};             // Quotient bit is 1 and new remainder is calculated by subtracting denominator from numerator
	temp = temp<<1;                               // shifting {Partial Quotient, Partial Remainder} which append a zero at the LSB of the partial remainder
    
   let y= temp[56:0]-{1'b0,denominator};          // if Dr > Nr
   if(temp[56:0]<{1'b0,denominator})              // Quotient bit is 0
       temp = {temp[113:58],1'b0,temp[56:0]};                                                                                                                
   else                                           // Quotient bit is 1 and new remainder is calculated by subtracting denominator from numerator
       temp = {temp[113:58],1'b1,y};              // shifting {Partial Quotient, Partial Remainder} which append a zero at the LSB of the partial remainder
    temp = temp<<1;
    
	return {denominator,temp};
endfunction
*/

(* noinline *)
function Bit#(170) fn_divide_step (Bit#(56) _divisor, Bit#(58) _remainder, Bit#(56) _dividend, Bool final_stage);
	if(final_stage == False) begin
		Bit#(114) accumulator = 0;

	    if(_remainder[57]==1'b0) begin
			accumulator = ({_remainder,_dividend}<<1) - {1'b0,_divisor,1'b0,56'b0} ;
			accumulator[0] = 1'b1;
		end
		else begin
			accumulator = ({_remainder,_dividend}<<1) + {1'b0,_divisor,1'b0,56'b0} ;
			accumulator[0] = 1'b0;
		end
		_remainder = accumulator[113:56];
		_dividend = accumulator[55:0];
	end
	else begin
	    _dividend = _dividend - (_dividend ^ (56'b11111111111111111111111111111111111111111111111111111111));

	    if(_remainder[57] == 1'b1) begin
		    _remainder = _remainder + {1'b0,_divisor,1'b0};
		    _dividend = _dividend - 1;
	    end
	end
	return {_divisor, _remainder, _dividend};
endfunction

interface Ifc_integer_divider_for_dpfdiv;

		/* Input Methods */
	method Action _inputs(Bit#(56) _denominator, Bit#(56) _numerator);
	method Action _remove_last_entry();
    method Action _set_flush(Bool _flush);

		/* Output Methods */
	method Bit#(170) output_();
endinterface:Ifc_integer_divider_for_dpfdiv

(*synthesize*)
module mkinteger_divider_for_dpfdiv(Ifc_integer_divider_for_dpfdiv);

FIFO#(Bit#(170)) ff_stage1 <-mkFIFO;   //FIFO is used since PipelineFIFO causes compilation error due to dependency between two rules
FIFO#(Bit#(170)) ff_stage2 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage3 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage4 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage5 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage6 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage7 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage8 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage9 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage10 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage11 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage12 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage13 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage14 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage15 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage16 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage17 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage18 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage19 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage20 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage21 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage22 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage23 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage24 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage25 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage26 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage27 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage28 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage29 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage30 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage31 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage32 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage33 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage34 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage35 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage36 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage37 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage38 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage39 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage40 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage41 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage42 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage43 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage44 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage45 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage46 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage47 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage48 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage49 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage50 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage51 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage52 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage53 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage54 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage55 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage56 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage57 <-mkPipelineFIFO();
FIFO#(Bit#(170)) ff_stage58 <-mkPipelineFIFO();
Wire#(Bool) wr_flush<- mkDWire(False);			// wire that indicates when to flush the output FIFO in case of a processor pipeline flush by firing the rule rl_flush

rule rl_flush_(wr_flush);				    	//rule that clears the contents of the FIFO in case of a flush
    ff_stage1.clear();
    ff_stage2.clear();
    ff_stage3.clear();
    ff_stage4.clear();
    ff_stage5.clear();
    ff_stage6.clear();
    ff_stage7.clear();
    ff_stage8.clear();
    ff_stage9.clear();
    ff_stage10.clear();
    ff_stage11.clear();
    ff_stage12.clear();
    ff_stage13.clear();
    ff_stage14.clear();
    ff_stage15.clear();
    ff_stage16.clear();
    ff_stage17.clear();
    ff_stage18.clear();
    ff_stage19.clear();
    ff_stage20.clear();
    ff_stage21.clear();
    ff_stage22.clear();
    ff_stage23.clear();
    ff_stage24.clear();
    ff_stage25.clear();
    ff_stage26.clear();
    ff_stage27.clear();
    ff_stage28.clear();
    ff_stage29.clear();
    ff_stage30.clear();
    ff_stage31.clear();
    ff_stage32.clear();
    ff_stage33.clear();
    ff_stage34.clear();
    ff_stage35.clear();
    ff_stage36.clear();
    ff_stage37.clear();
    ff_stage38.clear();
    ff_stage39.clear();
    ff_stage40.clear();
    ff_stage41.clear();
    ff_stage42.clear();
    ff_stage43.clear();
    ff_stage44.clear();
    ff_stage45.clear();
    ff_stage46.clear();
    ff_stage47.clear();
    ff_stage48.clear();
    ff_stage49.clear();
    ff_stage50.clear();
    ff_stage51.clear();
    ff_stage52.clear();
    ff_stage53.clear();
    ff_stage54.clear();
    ff_stage55.clear();
    ff_stage56.clear();
    ff_stage57.clear();
    ff_stage58.clear();
endrule

/* The following set of rules perform the computation. Each rule fires only when the previous FIFO is
filled. and the next FIFO is empty. 
Each rule calls the function fn_divide_step to perform the single step of division.
*/

rule rl_ff_stage_2(!wr_flush);
	ff_stage2.enq(fn_divide_step(ff_stage1.first()[169:114],ff_stage1.first()[113:56],ff_stage1.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage1.first()[169:114],ff_stage1.first()[113:56],ff_stage1.first()[55:0]);
	ff_stage1.deq();
endrule:rl_ff_stage_2

rule rl_ff_stage_3(!wr_flush);
	ff_stage3.enq(fn_divide_step(ff_stage2.first()[169:114],ff_stage2.first()[113:56],ff_stage2.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage2.first()[169:114],ff_stage2.first()[113:56],ff_stage2.first()[55:0]);
	ff_stage2.deq();
endrule:rl_ff_stage_3

rule rl_ff_stage_4(!wr_flush);
	ff_stage4.enq(fn_divide_step(ff_stage3.first()[169:114],ff_stage3.first()[113:56],ff_stage3.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage3.first()[169:114],ff_stage3.first()[113:56],ff_stage3.first()[55:0]);
	ff_stage3.deq();
endrule:rl_ff_stage_4

rule rl_ff_stage_5(!wr_flush);
	ff_stage5.enq(fn_divide_step(ff_stage4.first()[169:114],ff_stage4.first()[113:56],ff_stage4.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage4.first()[169:114],ff_stage4.first()[113:56],ff_stage4.first()[55:0]);
	ff_stage4.deq();
endrule:rl_ff_stage_5

rule rl_ff_stage_6(!wr_flush);
	ff_stage6.enq(fn_divide_step(ff_stage5.first()[169:114],ff_stage5.first()[113:56],ff_stage5.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage5.first()[169:114],ff_stage5.first()[113:56],ff_stage5.first()[55:0]);
	ff_stage5.deq();
endrule:rl_ff_stage_6

rule rl_ff_stage_7(!wr_flush);
	ff_stage7.enq(fn_divide_step(ff_stage6.first()[169:114],ff_stage6.first()[113:56],ff_stage6.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage6.first()[169:114],ff_stage6.first()[113:56],ff_stage6.first()[55:0]);
	ff_stage6.deq();
endrule:rl_ff_stage_7

rule rl_ff_stage_8(!wr_flush);
	ff_stage8.enq(fn_divide_step(ff_stage7.first()[169:114],ff_stage7.first()[113:56],ff_stage7.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage7.first()[169:114],ff_stage7.first()[113:56],ff_stage7.first()[55:0]);
	ff_stage7.deq();
endrule:rl_ff_stage_8

rule rl_ff_stage_9(!wr_flush);
	ff_stage9.enq(fn_divide_step(ff_stage8.first()[169:114],ff_stage8.first()[113:56],ff_stage8.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage8.first()[169:114],ff_stage8.first()[113:56],ff_stage8.first()[55:0]);
	ff_stage8.deq();
endrule:rl_ff_stage_9

rule rl_ff_stage_10(!wr_flush);
	ff_stage10.enq(fn_divide_step(ff_stage9.first()[169:114],ff_stage9.first()[113:56],ff_stage9.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage9.first()[169:114],ff_stage9.first()[113:56],ff_stage9.first()[55:0]);
	ff_stage9.deq();
endrule:rl_ff_stage_10

rule rl_ff_stage_11(!wr_flush);
	ff_stage11.enq(fn_divide_step(ff_stage10.first()[169:114],ff_stage10.first()[113:56],ff_stage10.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage10.first()[169:114],ff_stage10.first()[113:56],ff_stage10.first()[55:0]);
	ff_stage10.deq();
endrule:rl_ff_stage_11

rule rl_ff_stage_12(!wr_flush);
	ff_stage12.enq(fn_divide_step(ff_stage11.first()[169:114],ff_stage11.first()[113:56],ff_stage11.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage11.first()[169:114],ff_stage11.first()[113:56],ff_stage11.first()[55:0]);
	ff_stage11.deq();
endrule:rl_ff_stage_12

rule rl_ff_stage_13(!wr_flush);
	ff_stage13.enq(fn_divide_step(ff_stage12.first()[169:114],ff_stage12.first()[113:56],ff_stage12.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage12.first()[169:114],ff_stage12.first()[113:56],ff_stage12.first()[55:0]);
	ff_stage12.deq();
endrule:rl_ff_stage_13

rule rl_ff_stage_14(!wr_flush);
	ff_stage14.enq(fn_divide_step(ff_stage13.first()[169:114],ff_stage13.first()[113:56],ff_stage13.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage13.first()[169:114],ff_stage13.first()[113:56],ff_stage13.first()[55:0]);
	ff_stage13.deq();
endrule:rl_ff_stage_14

rule rl_ff_stage_15(!wr_flush);
	ff_stage15.enq(fn_divide_step(ff_stage14.first()[169:114],ff_stage14.first()[113:56],ff_stage14.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage14.first()[169:114],ff_stage14.first()[113:56],ff_stage14.first()[55:0]);
	ff_stage14.deq();
endrule:rl_ff_stage_15

rule rl_ff_stage_16(!wr_flush);
	ff_stage16.enq(fn_divide_step(ff_stage15.first()[169:114],ff_stage15.first()[113:56],ff_stage15.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage15.first()[169:114],ff_stage15.first()[113:56],ff_stage15.first()[55:0]);
	ff_stage15.deq();
endrule:rl_ff_stage_16

rule rl_ff_stage_17(!wr_flush);
	ff_stage17.enq(fn_divide_step(ff_stage16.first()[169:114],ff_stage16.first()[113:56],ff_stage16.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage16.first()[169:114],ff_stage16.first()[113:56],ff_stage16.first()[55:0]);
	ff_stage16.deq();
endrule:rl_ff_stage_17

rule rl_ff_stage_18(!wr_flush);
	ff_stage18.enq(fn_divide_step(ff_stage17.first()[169:114],ff_stage17.first()[113:56],ff_stage17.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage17.first()[169:114],ff_stage17.first()[113:56],ff_stage17.first()[55:0]);
	ff_stage17.deq();
endrule:rl_ff_stage_18

rule rl_ff_stage_19(!wr_flush);
	ff_stage19.enq(fn_divide_step(ff_stage18.first()[169:114],ff_stage18.first()[113:56],ff_stage18.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage18.first()[169:114],ff_stage18.first()[113:56],ff_stage18.first()[55:0]);
	ff_stage18.deq();
endrule:rl_ff_stage_19

rule rl_ff_stage_20(!wr_flush);
	ff_stage20.enq(fn_divide_step(ff_stage19.first()[169:114],ff_stage19.first()[113:56],ff_stage19.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage19.first()[169:114],ff_stage19.first()[113:56],ff_stage19.first()[55:0]);
	ff_stage19.deq();
endrule:rl_ff_stage_20

rule rl_ff_stage_21(!wr_flush);
	ff_stage21.enq(fn_divide_step(ff_stage20.first()[169:114],ff_stage20.first()[113:56],ff_stage20.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage20.first()[169:114],ff_stage20.first()[113:56],ff_stage20.first()[55:0]);
	ff_stage20.deq();
endrule:rl_ff_stage_21

rule rl_ff_stage_22(!wr_flush);
	ff_stage22.enq(fn_divide_step(ff_stage21.first()[169:114],ff_stage21.first()[113:56],ff_stage21.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage21.first()[169:114],ff_stage21.first()[113:56],ff_stage21.first()[55:0]);
	ff_stage21.deq();
endrule:rl_ff_stage_22

rule rl_ff_stage_23(!wr_flush);
	ff_stage23.enq(fn_divide_step(ff_stage22.first()[169:114],ff_stage22.first()[113:56],ff_stage22.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage22.first()[169:114],ff_stage22.first()[113:56],ff_stage22.first()[55:0]);
	ff_stage22.deq();
endrule:rl_ff_stage_23

rule rl_ff_stage_24(!wr_flush);
	ff_stage24.enq(fn_divide_step(ff_stage23.first()[169:114],ff_stage23.first()[113:56],ff_stage23.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage23.first()[169:114],ff_stage23.first()[113:56],ff_stage23.first()[55:0]);
	ff_stage23.deq();
endrule:rl_ff_stage_24

rule rl_ff_stage_25(!wr_flush);
	ff_stage25.enq(fn_divide_step(ff_stage24.first()[169:114],ff_stage24.first()[113:56],ff_stage24.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage24.first()[169:114],ff_stage24.first()[113:56],ff_stage24.first()[55:0]);
	ff_stage24.deq();
endrule:rl_ff_stage_25

rule rl_ff_stage_26(!wr_flush);
	ff_stage26.enq(fn_divide_step(ff_stage25.first()[169:114],ff_stage25.first()[113:56],ff_stage25.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage25.first()[169:114],ff_stage25.first()[113:56],ff_stage25.first()[55:0]);
	ff_stage25.deq();
endrule:rl_ff_stage_26

rule rl_ff_stage_27(!wr_flush);
	ff_stage27.enq(fn_divide_step(ff_stage26.first()[169:114],ff_stage26.first()[113:56],ff_stage26.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage26.first()[169:114],ff_stage26.first()[113:56],ff_stage26.first()[55:0]);
	ff_stage26.deq();
endrule:rl_ff_stage_27

rule rl_ff_stage_28(!wr_flush);
	ff_stage28.enq(fn_divide_step(ff_stage27.first()[169:114],ff_stage27.first()[113:56],ff_stage27.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage27.first()[169:114],ff_stage27.first()[113:56],ff_stage27.first()[55:0]);
	ff_stage27.deq();
endrule:rl_ff_stage_28

rule rl_ff_stage_29(!wr_flush);
	ff_stage29.enq(fn_divide_step(ff_stage28.first()[169:114],ff_stage28.first()[113:56],ff_stage28.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage28.first()[169:114],ff_stage28.first()[113:56],ff_stage28.first()[55:0]);
	ff_stage28.deq();
endrule:rl_ff_stage_29

rule rl_ff_stage_30(!wr_flush);
	ff_stage30.enq(fn_divide_step(ff_stage29.first()[169:114],ff_stage29.first()[113:56],ff_stage29.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage29.first()[169:114],ff_stage29.first()[113:56],ff_stage29.first()[55:0]);
	ff_stage29.deq();
endrule:rl_ff_stage_30

rule rl_ff_stage_31(!wr_flush);
	ff_stage31.enq(fn_divide_step(ff_stage30.first()[169:114],ff_stage30.first()[113:56],ff_stage30.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage30.first()[169:114],ff_stage30.first()[113:56],ff_stage30.first()[55:0]);
	ff_stage30.deq();
endrule:rl_ff_stage_31

rule rl_ff_stage_32(!wr_flush);
	ff_stage32.enq(fn_divide_step(ff_stage31.first()[169:114],ff_stage31.first()[113:56],ff_stage31.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage31.first()[169:114],ff_stage31.first()[113:56],ff_stage31.first()[55:0]);
	ff_stage31.deq();
endrule:rl_ff_stage_32

rule rl_ff_stage_33(!wr_flush);
	ff_stage33.enq(fn_divide_step(ff_stage32.first()[169:114],ff_stage32.first()[113:56],ff_stage32.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage32.first()[169:114],ff_stage32.first()[113:56],ff_stage32.first()[55:0]);
	ff_stage32.deq();
endrule:rl_ff_stage_33

rule rl_ff_stage_34(!wr_flush);
	ff_stage34.enq(fn_divide_step(ff_stage33.first()[169:114],ff_stage33.first()[113:56],ff_stage33.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage33.first()[169:114],ff_stage33.first()[113:56],ff_stage33.first()[55:0]);
	ff_stage33.deq();
endrule:rl_ff_stage_34

rule rl_ff_stage_35(!wr_flush);
	ff_stage35.enq(fn_divide_step(ff_stage34.first()[169:114],ff_stage34.first()[113:56],ff_stage34.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage34.first()[169:114],ff_stage34.first()[113:56],ff_stage34.first()[55:0]);
	ff_stage34.deq();
endrule:rl_ff_stage_35

rule rl_ff_stage_36(!wr_flush);
	ff_stage36.enq(fn_divide_step(ff_stage35.first()[169:114],ff_stage35.first()[113:56],ff_stage35.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage35.first()[169:114],ff_stage35.first()[113:56],ff_stage35.first()[55:0]);
	ff_stage35.deq();
endrule:rl_ff_stage_36

rule rl_ff_stage_37(!wr_flush);
	ff_stage37.enq(fn_divide_step(ff_stage36.first()[169:114],ff_stage36.first()[113:56],ff_stage36.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage36.first()[169:114],ff_stage36.first()[113:56],ff_stage36.first()[55:0]);
	ff_stage36.deq();
endrule:rl_ff_stage_37

rule rl_ff_stage_38(!wr_flush);
	ff_stage38.enq(fn_divide_step(ff_stage37.first()[169:114],ff_stage37.first()[113:56],ff_stage37.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage37.first()[169:114],ff_stage37.first()[113:56],ff_stage37.first()[55:0]);
	ff_stage37.deq();
endrule:rl_ff_stage_38

rule rl_ff_stage_39(!wr_flush);
	ff_stage39.enq(fn_divide_step(ff_stage38.first()[169:114],ff_stage38.first()[113:56],ff_stage38.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage38.first()[169:114],ff_stage38.first()[113:56],ff_stage38.first()[55:0]);
	ff_stage38.deq();
endrule:rl_ff_stage_39

rule rl_ff_stage_40(!wr_flush);
	ff_stage40.enq(fn_divide_step(ff_stage39.first()[169:114],ff_stage39.first()[113:56],ff_stage39.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage39.first()[169:114],ff_stage39.first()[113:56],ff_stage39.first()[55:0]);
	ff_stage39.deq();
endrule:rl_ff_stage_40

rule rl_ff_stage_41(!wr_flush);
	ff_stage41.enq(fn_divide_step(ff_stage40.first()[169:114],ff_stage40.first()[113:56],ff_stage40.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage40.first()[169:114],ff_stage40.first()[113:56],ff_stage40.first()[55:0]);
	ff_stage40.deq();
endrule:rl_ff_stage_41

rule rl_ff_stage_42(!wr_flush);
	ff_stage42.enq(fn_divide_step(ff_stage41.first()[169:114],ff_stage41.first()[113:56],ff_stage41.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage41.first()[169:114],ff_stage41.first()[113:56],ff_stage41.first()[55:0]);
	ff_stage41.deq();
endrule:rl_ff_stage_42

rule rl_ff_stage_43(!wr_flush);
	ff_stage43.enq(fn_divide_step(ff_stage42.first()[169:114],ff_stage42.first()[113:56],ff_stage42.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage42.first()[169:114],ff_stage42.first()[113:56],ff_stage42.first()[55:0]);
	ff_stage42.deq();
endrule:rl_ff_stage_43

rule rl_ff_stage_44(!wr_flush);
	ff_stage44.enq(fn_divide_step(ff_stage43.first()[169:114],ff_stage43.first()[113:56],ff_stage43.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage43.first()[169:114],ff_stage43.first()[113:56],ff_stage43.first()[55:0]);
	ff_stage43.deq();
endrule:rl_ff_stage_44

rule rl_ff_stage_45(!wr_flush);
	ff_stage45.enq(fn_divide_step(ff_stage44.first()[169:114],ff_stage44.first()[113:56],ff_stage44.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage44.first()[169:114],ff_stage44.first()[113:56],ff_stage44.first()[55:0]);
	ff_stage44.deq();
endrule:rl_ff_stage_45

rule rl_ff_stage_46(!wr_flush);
	ff_stage46.enq(fn_divide_step(ff_stage45.first()[169:114],ff_stage45.first()[113:56],ff_stage45.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage45.first()[169:114],ff_stage45.first()[113:56],ff_stage45.first()[55:0]);
	ff_stage45.deq();
endrule:rl_ff_stage_46

rule rl_ff_stage_47(!wr_flush);
	ff_stage47.enq(fn_divide_step(ff_stage46.first()[169:114],ff_stage46.first()[113:56],ff_stage46.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage46.first()[169:114],ff_stage46.first()[113:56],ff_stage46.first()[55:0]);
	ff_stage46.deq();
endrule:rl_ff_stage_47

rule rl_ff_stage_48(!wr_flush);
	ff_stage48.enq(fn_divide_step(ff_stage47.first()[169:114],ff_stage47.first()[113:56],ff_stage47.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage47.first()[169:114],ff_stage47.first()[113:56],ff_stage47.first()[55:0]);
	ff_stage47.deq();
endrule:rl_ff_stage_48

rule rl_ff_stage_49(!wr_flush);
	ff_stage49.enq(fn_divide_step(ff_stage48.first()[169:114],ff_stage48.first()[113:56],ff_stage48.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage48.first()[169:114],ff_stage48.first()[113:56],ff_stage48.first()[55:0]);
	ff_stage48.deq();
endrule:rl_ff_stage_49

rule rl_ff_stage_50(!wr_flush);
	ff_stage50.enq(fn_divide_step(ff_stage49.first()[169:114],ff_stage49.first()[113:56],ff_stage49.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage49.first()[169:114],ff_stage49.first()[113:56],ff_stage49.first()[55:0]);
	ff_stage49.deq();
endrule:rl_ff_stage_50

rule rl_ff_stage_51(!wr_flush);
	ff_stage51.enq(fn_divide_step(ff_stage50.first()[169:114],ff_stage50.first()[113:56],ff_stage50.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage50.first()[169:114],ff_stage50.first()[113:56],ff_stage50.first()[55:0]);
	ff_stage50.deq();
endrule:rl_ff_stage_51

rule rl_ff_stage_52(!wr_flush);
	ff_stage52.enq(fn_divide_step(ff_stage51.first()[169:114],ff_stage51.first()[113:56],ff_stage51.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage51.first()[169:114],ff_stage51.first()[113:56],ff_stage51.first()[55:0]);
	ff_stage51.deq();
endrule:rl_ff_stage_52

rule rl_ff_stage_53(!wr_flush);
	ff_stage53.enq(fn_divide_step(ff_stage52.first()[169:114],ff_stage52.first()[113:56],ff_stage52.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage52.first()[169:114],ff_stage52.first()[113:56],ff_stage52.first()[55:0]);
	ff_stage52.deq();
endrule:rl_ff_stage_53

rule rl_ff_stage_54(!wr_flush);
	ff_stage54.enq(fn_divide_step(ff_stage53.first()[169:114],ff_stage53.first()[113:56],ff_stage53.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage53.first()[169:114],ff_stage53.first()[113:56],ff_stage53.first()[55:0]);
	ff_stage53.deq();
endrule:rl_ff_stage_54

rule rl_ff_stage_55(!wr_flush);
	ff_stage55.enq(fn_divide_step(ff_stage54.first()[169:114],ff_stage54.first()[113:56],ff_stage54.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage54.first()[169:114],ff_stage54.first()[113:56],ff_stage54.first()[55:0]);
	ff_stage54.deq();
endrule:rl_ff_stage_55

rule rl_ff_stage_56(!wr_flush);
	ff_stage56.enq(fn_divide_step(ff_stage55.first()[169:114],ff_stage55.first()[113:56],ff_stage55.first()[55:0], False));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage55.first()[169:114],ff_stage55.first()[113:56],ff_stage55.first()[55:0]);
	ff_stage55.deq();
endrule:rl_ff_stage_56

rule rl_ff_stage_57(!wr_flush);
	ff_stage57.enq(fn_divide_step(ff_stage56.first()[169:114],ff_stage56.first()[113:56],ff_stage56.first()[55:0], True));
	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage56.first()[169:114],ff_stage56.first()[113:56],ff_stage56.first()[55:0]);
	ff_stage56.deq();
endrule:rl_ff_stage_57



// rule rl_ff_stage_58(!wr_flush);
// 	ff_stage58.enq(fn_divide_step(ff_stage57.first()[169:114],ff_stage57.first()[113:56],ff_stage57.first()[55:0], True));
// 	$display("divisor = %b, remainder = %b, dividend = %b", ff_stage57.first()[169:114],ff_stage57.first()[113:56],ff_stage57.first()[55:0]);
// 	ff_stage57.deq();
// endrule:rl_ff_stage_58


method Action _inputs(Bit#(56) _denominator, Bit#(56) _numerator);
	ff_stage1.enq(fn_divide_step(_denominator, {2'b0,_numerator}, 56'b0, False));
endmethod

method Bit#(170) output_();
	return ff_stage57.first();
endmethod

method Action _remove_last_entry();
	ff_stage57.deq();
endmethod

// when a wr_flush is initiated in the processor this method will also be called.
// it sets the flsuh wire to True, hence no rule other flush_all_fifos will
// fire and hence clear all the buffer and intermediate register/ wires ...
method Action _set_flush(Bool _flush);
    wr_flush<=_flush;
endmethod

endmodule:mkinteger_divider_for_dpfdiv


		/* 	TEST BENCH 	*/

//(*synthesize*)
module mkTb(Empty);

Reg#(Bit#(32)) rg_clock <-mkReg(0);

Ifc_integer_divider_for_dpfdiv instance_divider <-mkinteger_divider_for_dpfdiv();

rule rl_count_clock ;
	rg_clock<=rg_clock+1;
	$display("Clock=%d",rg_clock);
	if(rg_clock=='d60)
		$finish(0);
endrule:rl_count_clock

rule rl_input1(rg_clock==1);
	//instance_divider._inputs({6'b100011,'d0},{3'b111,'d0});
	instance_divider._inputs({3'b111, 53'b0}, {6'b100011, 50'b0});
	$display("%b %b", {3'b111, 53'b0}, {6'b100011, 50'b0});
endrule:rl_input1

rule rl_check;
	$display("Quotient=%b Remainder=%b",instance_divider.output_[55:0],instance_divider.output_[113:56]);
	instance_divider._remove_last_entry();
endrule
endmodule
endpackage:integer_divider_for_dpfdiv
