`define Token_size 32 // the size of the token for each instruction.
`define Reg_width 32 // the register data width of the processor.
`define RegFileSize 32 // describes the size of ht register file in the processor.
`define Addr_width 32 // the address width
`ifdef simulate
  `define Addr_space 20	//since we are leaving off the lower 2 bits of address(byte addressable memory), we have to 
`else
  `define Addr_space 13
`endif
//`define divider32 //  define whether the divider is 64 bit or 32 bit.
`define RV32
//'define RV64
`ifdef RV32
	`define Multiplier32
	`define Multiplier16
	`define divider32
`endif
`ifdef RV64
	`define Multiplier64
	`define Multiplier32
	`define Multiplier16
`endif

// Branch_predictor_paramters
`define size_of_table 16        // indicates the number of entries of the Branch target Buffer.
`define bits_used_for_accessing 4 // Log2(`size_of_table)

////////////////////// parameters for Cache ///////////////////////////////////

`define Ways 4              // total number of ways in a n-way set-associative cache.
`define Word_size 4         // number of Bytes in a word
`define Block_size 4        // number of words in a block
`define Num_of_sets 512    // total number of sets
`define Num_of_tag_bits 19  // number of bits used to represent the tag.
`define Num_of_offset_bits 4 // number of bits used to represent the offset.
// number of BRAMs = number of Ways
// Depth of each BRAM = (Size_of_cache/(Ways*Word_size*Block_size)) entries
// Width of each Data BRAM = Block_size*Word_size*8 bits
// Width of each TAG BRAM = `Addr_width- Log(Size_of_cache/(Ways*Word_size*Block_size)) - Log(Block_size*Word_size)
///////////////////////////////////////////////////////////////////////////////////

`define Start_address
`define End_address

/////////////////////////// Register Mapping for Machine Mode Regs /////////////////
`define MCPUID   'hF00 // CPU Description
`define MIMPID   'hF01 // Vecndor ID and Version Number
`define MHARTID  'hF10 // Hardware thread ID
`define MSTATUS  'h300 // Machine Status Register
`define MTVEC    'h301 // Machine trap-handler base address
`define MTDELEG  'h302 // machine trap delegation regsiter
`define MIE      'h304 // machine interrupt-enable register
`define MTIMECMP 'h321 // machine wall-clock timer compare value
`define MTIME    'h701 // machine wall-clock time
`define MTIMEH   'h741 // Upper 32 bits of mtime, RV32I only
`define MSCRATCH 'h340 // scracth register for machine trap handlers
`define MEPC     'h341 // machine exception program counter
`define MCAUSE   'h342 // machine trap cause
`define MBADDR   'h343 // machine bad address
`define MIP      'h344 // machine interrupt pending
`define MBASE    'h380 // base register
`define MBOUND   'h381 // bound register
`define MIBASE   'h382 // instruction base register
`define MIBOUND  'h383 // intruction bound register
`define MDBASE   'h384 // data base register
`define MDBOUND  'h385 // data bound register
`define UARTTX   'h77f // Uart Transmit register. Write-only
`define UARTRX   'h77e // Uart Receive register. Read-only TODO: Make this read-only address
////////////////////////////////////////////////////////////////////////////////////

