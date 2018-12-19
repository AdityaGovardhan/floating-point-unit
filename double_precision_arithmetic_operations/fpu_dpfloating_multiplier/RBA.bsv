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
Last updated on : 17th June, 2016

These are set of functions used along with the integer_multiplier to perform addition of partial products
*/
package RBA;
import Vector::*;

(*noinline*)
// Adding two one bit redundant binary numbers
function Bit#(4) rba(bit ap, bit am, bit bp, bit bm, bit betap, bit hp);
	bit dp,dm,alpha,beta,h,k,l,x,y;
	Bit#(4) ans=0;
	x=ap|am;
	y=bp|bm;
	k=ap|bp;
	h=am|bm;
	
	l=x^y;
	beta=(x&y&k)|((~x)&y&(~hp))|(x&(~y)&(~hp));
	alpha=x^y^hp;
	dm=alpha&(~betap);
	dp=(~alpha)&betap;
	ans={beta,h,dp,dm};
	return ans;

endfunction

(*noinline*)
// 128 bit rbAdder. Adds two RB numbers 
function Bit#(256)  rbAdder64(Bit#(128) ap, Bit#(128) am, Bit#(128) bp, Bit#(128) bm);
	Bit#(128) dp=0,dm=0;
	bit beta=0,h=0;
	Bit#(256) ans=0;
	Bit#(4) res=0;
	for(int i=0;i< 128;i=i+1)
	begin
		if(ap[i]==1 && am[i]==1)
		begin
			ap[i]=0;
			am[i]=0;
		end
		
		if(bp[i]==1 && bm[i]==1)
		begin
			bp[i]=0;
			bm[i]=0;
		end
		res=rba(ap[i],am[i],bp[i],bm[i],beta,h);		
		beta=res[3];
		h=res[2];
		dp[i]=res[1];
		dm[i]=res[0];
	end
	ans={dp,dm}; // Result of addition of two 128 bit redundant binary numbers.

	return ans;
endfunction

(*noinline*) 
function  Bit#(256) wallace_rba(Vector#(16,Bit#(128)) inp) ;
	
	//Step1. Adding 16 partial products
		Bit#(256) res1; //Temp varialbes
		Bit#(128) s1_1p,s1_1m; //Temp variables for stage 1
		res1=rbAdder64(inp[0],inp[1],inp[2],inp[3]);
		s1_1m=res1[127:0];
		s1_1p=res1[255:128];
	
		Bit#(256) res2; //Temp varialbes
		Bit#(128) s1_2m,s1_2p; //Temp variables for stage 1
		res2=rbAdder64(inp[4],inp[5],inp[6],inp[7]);
		s1_2m=res2[127:0];
		s1_2p=res2[255:128];
	
		Bit#(256) res3; //Temp varialbes
		Bit#(128) s1_3m,s1_3p; //Temp variables for stage 1
		res3=rbAdder64(inp[8],inp[9],inp[10],inp[11]);
		s1_3m=res3[127:0];
		s1_3p=res3[255:128];
		
		Bit#(256) res4; //Temp varialbes
		Bit#(128) s1_4m,s1_4p; //Temp variables for stage 1
		res4=rbAdder64(inp[12],inp[13],inp[14],inp[15]);
		s1_4m=res4[127:0];
		s1_4p=res4[255:128];

	//Step2. Adding the result from step 1

		Bit#(128) s2_1p,s2_1m; //Temp variable stage 2
		res1=rbAdder64(s1_1p,s1_1m,s1_2p,s1_2m);
		s2_1m=res1[127:0];
		s2_1p=res1[255:128];

		Bit#(128) s2_2p,s2_2m; //Temp variable stage 2
		res2=rbAdder64(s1_3p,s1_3m,s1_4p,s1_4m);
		s2_2m=res2[127:0];
		s2_2p=res2[255:128];
		
	//Step 3 Adding the result from step 2
		Bit#(128) s3_p, s3_m; // Temp varible for stage 3. Store final rb number after 3 steps of addition
		res1 =  rbAdder64(s2_1p,s2_1m,s2_2p,s2_2m);
		s3_m = res1[127:0];
		s3_p = res1[255:128];

	return {s3_p,s3_m};

endfunction

//Stage 4
//Wallace tree structure with 2 levels 
(*noinline*) 
function Bit#(256) wallace_rba_final(Bit#(256) in1, Bit#(256) in2);

	Bit#(128) s1_1p,s1_1m; //Temp variables
	Bit#(128) s2_p, s2_m; //Temp variable for final result
	Bit#(256) res1,res2;
	//Step1. Adding the result of additon of two sets of 16 partial products
	res1=rbAdder64(in1[255:128], in1[127:0], in2[255:128],in2[127:0]);
	s1_1m=res1[127:0];
	s1_1p=res1[255:128];
	
	//Step2. Adding the result from step 2 with a rb number, which is to be added due to logic in redundant binary number.
	Bit#(67) zero_=0;
	Bit#(61) inter = 'b1000100010001000100010001000100010001000100010001000100010001;
	
	res2=rbAdder64(s1_1p,s1_1m,'d0,{zero_,inter});
	s2_m=res2[127:0];
	s2_p=res2[255:128];

	return {s2_p, s2_m}; 

endfunction

endpackage