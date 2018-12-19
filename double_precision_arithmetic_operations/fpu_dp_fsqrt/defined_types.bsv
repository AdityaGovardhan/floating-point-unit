/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Author Names : Neel Gala, Arjun Menon
Email ID : neelgala@gmail.com, c.arjunmenon@gmail.com

Description : 

This files contains all the types and structures that are used in any of the modules.
*/
package defined_types;

`include "defined_parameters.bsv"

typedef enum {
    None,Instruction_misaligned, Load_misaligned, Store_misaligned, Instruction_buserr, Load_buserr, Store_buserr, Illegal_instruction, System_class, Breakpoint,
		Invalid, Overflow, Underflow, Divide_by_Zero,Inexact
} Exception deriving (Eq, Bits, FShow);

typedef enum{
	Taken, Notaken
}Actual_jump deriving (Eq,Bits,FShow); // actual branch condition used in the branch execution unit.

// enum defining the prediction of the branch predictor for the current PC.
typedef enum{
	Predicted_taken,Predicted_notaken
}Prediction_type deriving (Eq,Bits,FShow); // output from the branch prediction unit.

// A typedef defining , if the prediction by the branch predictor was correct or wrong. 
typedef union tagged{
	Bit#(`Addr_width) Mispredicted;
	Bit#(`Addr_width) Correct_prediction;
}Prediction_result deriving (Eq,Bits); // result of prediuction from the branch execution unit.

typedef struct{
	Bit#(32) prog_counter_;
    Prediction_type prediction_;
} Predictor_output deriving(Bits, Eq);    // the program counter from the branch prediction unit.

typedef struct{
	Bit#(32) data_forward;
	Bit#(TLog#(`Reg_width)) rd_forward;
	Bool valid;
	Register_type rd_type;
//  Bit#(2); // 0-INTEGER 1-FLOATING 2-PRIVELEGED
}Operand_forwading_type deriving (Bits,Eq);	// the data structure for operand forwarding from any stage

typedef struct{
	Bit#(`Reg_width) rs1;
	Bit#(`Reg_width) rs2;
	Bit#(`Reg_width) rs3;
} Output_for_operand_fetch deriving (Bits,Eq); // output from the register file to the decode stage

typedef enum {
	ALU,FPU,MEM,BRANCH,NOP,AMO,ILLEGAL,PRIVELEGED
}Instruction_type deriving(Bits, Eq,FShow); // the type of the decoded instruction.

// to distuingish between integer and floating point RF
typedef enum {IntegerRF, FloatingRF} Register_type deriving(Bits,Eq,FShow);

// the output data structure of the decoder. 
typedef struct {
	Instruction_type inst_type;  //to hold the istruction type
	Bit#(5) rs1;    //address of 1st source operand
	Bit#(5) rs2;    //address of 2nd source operand
	Bit#(5) rs3;    //address of 2nd source operand
	Bit#(5) rd;     //address of destination register
	Register_type rd_type;
	Register_type rs1_type;
	Register_type rs2_type;
	Bit#(32) instruction; // full 32bit instruction
  Bool auipc;
  Bool is_priv; // is the instruction a privilege instruction
  Bit#(3) priv_funct;
  Bit#(12) priv_addr;
  Bit#(1) priv_immediate;
} Decoded_info_type deriving(Bits, Eq);


// the data stucture for the pipeline FIFO between fetch and decode.
typedef struct{
	Bit#(32) program_counter;
	Bit#(32) instruction;
	Prediction_type prediction;
    Exception exception;
}IF_ID_type deriving (Bits,Eq);


typedef struct{
	Bit#(`Reg_width) rs1;
	Bit#(`Reg_width) rs2;
	Bit#(`Reg_width) rs3;
	Bit#(`Addr_width) program_counter;
	Bit#(32) instruction;
	Register_type rd_type;
	Instruction_type inst_type;
	Prediction_type prediction;
    Exception exception;
}ID_IE_type deriving (Bits,Eq);

typedef struct{
	Bool bypass; // if this is true then the current instruction is not a MEMORY operation and will bypass that stage
	Bit#(32) exec_result;
	Bit#(5) destination_register;
	Register_type rd_type;
	Bit#(32) memory_data;
	Bit#(1) signextend;
	Bit#(2) word_size;
	Memory_type mem_type;
	Atomic_funct amofunct;
	Bit#(`Addr_width) program_counter;
    Exception exception;
}IE_IMEM_type deriving (Bits,Eq);

typedef struct{
	Bit#(32) destination_value;
	Bit#(5) destination_address;
	Bit#(`Addr_width) program_counter;
  Exception exception;
	Register_type rd_type;
}IMEM_IWB_type deriving(Bits,Eq);

typedef enum {AMO_LOAD, AMO_STORE}Atomic_mode deriving(Bits, Eq,FShow); // atomic mode currently under execution

typedef struct{
	Bit#(32) aluresult;
	Bit#(5) destination;
} Alu_output deriving(Bits,Eq); // output struct from the alu.

typedef struct{
	Bit#(32) branchresult;
	Bit#(5) destination;
	Training_data training_data;
	Prediction_result pred_result;
} Branch_output deriving(Bits,Eq); // output from the branch execution unit.

typedef enum {LOAD, ATOMIC, STORE}Memory_type deriving (Bits,Eq,FShow); // type of memory from the execution unit.

typedef struct{
	Bit#(5) destination; // the destinatio address in the GPR
	Bit#(32) memory_address; // address to the memory
	Bit#(32) memory_data; // data to be written in the memory
	Bit#(2) word_size; // size of the data transfer
	Bit#(1) signextend; // whether the loaded value has to be signextended
	Atomic_funct amofunct; // type of atomic operation
	Memory_type mem_type; // STORE or AMO or LOAD
} Memory_output deriving(Bits,Eq); // output from the memory

typedef struct {
	Bit#(32) pc;
	Bit#(32) branch_address;
	Actual_jump actual;} Training_data deriving (Bits, Eq);

typedef enum {SWAP,ADD,XOR,AND,OR,MINU,MAXU,MIN,MAX} Atomic_funct deriving(Bits,Eq,FShow);

typedef struct{
		Bit#(32) fsr;						// the file status register containing all the flags and control bits
		Bit#(64) final_result;					// the final result for the operation
		Exception exception; 					// indicates if any exception is generated.
	}Floating_output deriving(Bits,Eq);				// data structure of the output FIFO.

endpackage
