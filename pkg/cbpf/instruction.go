package cbpf

const BPFMaxIns = 4096

type OpCode uint8

// instruction class
const (
	BPFLd OpCode = iota
	BPFLdx
	BPFSt
	BPFStx
	BPFAlu
	BPFJmp
	BPFRet  // BPFJmp32
	BPFMisc // BPFAlu64
)

// load/store mode
const (
	BPFImm OpCode = 0x00
	BPFAbs OpCode = 0x20
	BPFInd OpCode = 0x40
	BPFMem OpCode = 0x60
	BPFLen OpCode = 0x80
)

// size
const (
	BPFW OpCode = 0x00
	BPFH OpCode = 0x08
	BPFB OpCode = 0x10
)

// jump
const (
	BPFJa   OpCode = 0x00
	BPFJeq  OpCode = 0x10
	BPFJgt  OpCode = 0x20
	BPFJset OpCode = 0x40
)

// alu
const (
	// dst += src
	BPFAdd OpCode = 0x00
	// dst -= src
	BPFSub OpCode = 0x10
	// dst *= src
	BPFMul OpCode = 0x20
	// dst /= src
	BPFDiv OpCode = 0x30
	// dst |= src
	BPFOr OpCode = 0x40
	// dst &= src
	BPFAnd OpCode = 0x50
	// dst <<= src
	BPFLsh OpCode = 0x60
	// dst >>= src
	BPFRsh OpCode = 0x70
	// dst ~= src
	BPFNeg OpCode = 0x80
	// dst %= src
	BPFMod OpCode = 0x90
	// dst ^= src
	BPFXor OpCode = 0xa0
)

// source operand
const (
	// use 32-bit imm
	BPFK OpCode = 0x00
	// use src reg
	BPFX OpCode = 0x80
)

func (op OpCode) Class() OpCode {
	return op & 0x7
}

type BPFInstruction struct {
	// opcode
	OpCode OpCode
	// the upper 8 bits of the opcode are unused
	_padOpCode uint8

	// jump true
	Jt uint8
	// jump false
	Jf uint8

	K uint32
}

const (
	LdrStr  = "ldr"
	LdhStr  = "ldh"
	LdStr   = "ld"
	LdiStr  = "ldi"
	LdxStr  = "ldx"
	LdxiStr = "ldxi"
	StStr   = "st"
	StxStr  = "stx"
	JmpStr  = "jmp"
	JeqStr  = "jeq"
	JneqStr = "jneq"
	JltStr  = "jlt"
	JleStr  = "jle"
	JgtStr  = "jgt"
	JsetStr = "jset"
	AddStr  = "add"
	SubStr  = "sub"
	MulStr  = "mul"
	DivStr  = "div"
	ModStr  = "mod"
	NegStr  = "neg"
	AndStr  = "and"
	OrStr   = "or"
	XorStr  = "xor"
	LshStr  = "lsh"
	RshStr  = "rsh"
	RetStr  = "ret"
	TaxStr  = "tax"
	TxaStr  = "txa"
)
