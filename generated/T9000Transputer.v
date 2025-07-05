// Generator : SpinalHDL v1.12.2    git head : f25edbcee624ef41548345cfb91c42060e33313f
// Component : T9000Transputer
// Git hash  : b948a2fbad2daff59189098e1675e03fea3ed5e4

`timescale 1ns/1ps

module T9000Transputer (
  output wire          systemBus_cmd_valid,
  input  wire          systemBus_cmd_ready,
  output wire          systemBus_cmd_payload_last,
  output wire [3:0]    systemBus_cmd_payload_fragment_source,
  output wire [0:0]    systemBus_cmd_payload_fragment_opcode,
  output wire [31:0]   systemBus_cmd_payload_fragment_address,
  output wire [3:0]    systemBus_cmd_payload_fragment_length,
  output wire [127:0]  systemBus_cmd_payload_fragment_data,
  output wire [15:0]   systemBus_cmd_payload_fragment_mask,
  input  wire          systemBus_rsp_valid,
  output wire          systemBus_rsp_ready,
  input  wire          systemBus_rsp_payload_last,
  input  wire [3:0]    systemBus_rsp_payload_fragment_source,
  input  wire [0:0]    systemBus_rsp_payload_fragment_opcode,
  input  wire [127:0]  systemBus_rsp_payload_fragment_data,
  input  wire          clk,
  input  wire          reset
);
  localparam AluOp_ADD = 5'd0;
  localparam AluOp_SUB = 5'd1;
  localparam AluOp_MUL = 5'd2;
  localparam AluOp_DIV = 5'd3;
  localparam AluOp_REM_1 = 5'd4;
  localparam AluOp_AND_1 = 5'd5;
  localparam AluOp_OR_1 = 5'd6;
  localparam AluOp_XOR_1 = 5'd7;
  localparam AluOp_NOT_1 = 5'd8;
  localparam AluOp_SHL = 5'd9;
  localparam AluOp_SHR = 5'd10;
  localparam AluOp_GT = 5'd11;
  localparam AluOp_GTU = 5'd12;
  localparam AluOp_DIFF = 5'd13;
  localparam AluOp_SUM = 5'd14;
  localparam AluOp_PROD = 5'd15;
  localparam AluOp_REV = 5'd16;
  localparam AluOp_DUP = 5'd17;
  localparam AluOp_FMUL = 5'd18;
  localparam SecondaryOpcode_REV = 9'd0;
  localparam SecondaryOpcode_LB = 9'd1;
  localparam SecondaryOpcode_BSUB = 9'd2;
  localparam SecondaryOpcode_ENDP = 9'd3;
  localparam SecondaryOpcode_DIFF = 9'd4;
  localparam SecondaryOpcode_ADD = 9'd5;
  localparam SecondaryOpcode_GCALL = 9'd6;
  localparam SecondaryOpcode_IN_1 = 9'd7;
  localparam SecondaryOpcode_PROD = 9'd8;
  localparam SecondaryOpcode_GT = 9'd9;
  localparam SecondaryOpcode_WSUB = 9'd10;
  localparam SecondaryOpcode_OUT_1 = 9'd11;
  localparam SecondaryOpcode_SUB = 9'd12;
  localparam SecondaryOpcode_STARTP = 9'd13;
  localparam SecondaryOpcode_OUTBYTE = 9'd14;
  localparam SecondaryOpcode_OUTWORD = 9'd15;
  localparam SecondaryOpcode_RESETCH = 9'd18;
  localparam SecondaryOpcode_CSUB = 9'd19;
  localparam SecondaryOpcode_STOPP = 9'd21;
  localparam SecondaryOpcode_LADD = 9'd22;
  localparam SecondaryOpcode_STLB = 9'd23;
  localparam SecondaryOpcode_STHF = 9'd24;
  localparam SecondaryOpcode_NORM = 9'd25;
  localparam SecondaryOpcode_LDIV = 9'd26;
  localparam SecondaryOpcode_LDPI = 9'd27;
  localparam SecondaryOpcode_STLF = 9'd28;
  localparam SecondaryOpcode_XDBLE = 9'd29;
  localparam SecondaryOpcode_LDPRI = 9'd30;
  localparam SecondaryOpcode_REM_1 = 9'd31;
  localparam SecondaryOpcode_RET = 9'd32;
  localparam SecondaryOpcode_LEND = 9'd33;
  localparam SecondaryOpcode_LDTIMER = 9'd34;
  localparam SecondaryOpcode_TESTERR = 9'd41;
  localparam SecondaryOpcode_TIN = 9'd43;
  localparam SecondaryOpcode_DIV = 9'd44;
  localparam SecondaryOpcode_DIST_1 = 9'd46;
  localparam SecondaryOpcode_DISC = 9'd47;
  localparam SecondaryOpcode_DISS = 9'd48;
  localparam SecondaryOpcode_LMUL = 9'd49;
  localparam SecondaryOpcode_NOT_1 = 9'd50;
  localparam SecondaryOpcode_XOR_1 = 9'd51;
  localparam SecondaryOpcode_BCNT = 9'd52;
  localparam SecondaryOpcode_LSHR = 9'd53;
  localparam SecondaryOpcode_LSHL = 9'd54;
  localparam SecondaryOpcode_LSUM = 9'd55;
  localparam SecondaryOpcode_LSUB = 9'd56;
  localparam SecondaryOpcode_RUNP = 9'd57;
  localparam SecondaryOpcode_XWORD = 9'd58;
  localparam SecondaryOpcode_SB = 9'd59;
  localparam SecondaryOpcode_GAJW = 9'd60;
  localparam SecondaryOpcode_SAVEL = 9'd61;
  localparam SecondaryOpcode_SAVEH = 9'd62;
  localparam SecondaryOpcode_WCNT = 9'd63;
  localparam SecondaryOpcode_SHR = 9'd64;
  localparam SecondaryOpcode_SHL = 9'd65;
  localparam SecondaryOpcode_MINT = 9'd66;
  localparam SecondaryOpcode_ALT = 9'd67;
  localparam SecondaryOpcode_ALTWT = 9'd68;
  localparam SecondaryOpcode_ALTEND = 9'd69;
  localparam SecondaryOpcode_AND_1 = 9'd70;
  localparam SecondaryOpcode_ENBT = 9'd71;
  localparam SecondaryOpcode_ENBC = 9'd72;
  localparam SecondaryOpcode_ENBS = 9'd73;
  localparam SecondaryOpcode_MOVE = 9'd74;
  localparam SecondaryOpcode_OR_1 = 9'd75;
  localparam SecondaryOpcode_CSNGL = 9'd76;
  localparam SecondaryOpcode_CCNT = 9'd77;
  localparam SecondaryOpcode_TALT = 9'd78;
  localparam SecondaryOpcode_LDIFF = 9'd79;
  localparam SecondaryOpcode_STHB = 9'd80;
  localparam SecondaryOpcode_TALTWT = 9'd81;
  localparam SecondaryOpcode_SUM = 9'd82;
  localparam SecondaryOpcode_MUL = 9'd83;
  localparam SecondaryOpcode_STTIMER = 9'd84;
  localparam SecondaryOpcode_STOPERR = 9'd85;
  localparam SecondaryOpcode_CWORD = 9'd86;
  localparam SecondaryOpcode_CLRHALTERR = 9'd87;
  localparam SecondaryOpcode_SETHALTERR = 9'd88;
  localparam SecondaryOpcode_TESTHALTERR = 9'd89;
  localparam SecondaryOpcode_TESTPRANAL = 9'd42;
  localparam SecondaryOpcode_DUP = 9'd90;
  localparam SecondaryOpcode_MOVE2DINIT = 9'd91;
  localparam SecondaryOpcode_MOVE2DALL = 9'd92;
  localparam SecondaryOpcode_MOVE2DNONZERO = 9'd93;
  localparam SecondaryOpcode_MOVE2DZERO = 9'd94;
  localparam SecondaryOpcode_GTU = 9'd95;
  localparam SecondaryOpcode_FMUL = 9'd114;
  localparam SecondaryOpcode_CRCWORD = 9'd116;
  localparam SecondaryOpcode_CRCBYTE = 9'd117;
  localparam SecondaryOpcode_BITCNT = 9'd118;
  localparam SecondaryOpcode_BITREVWORD = 9'd119;
  localparam SecondaryOpcode_BITREVNBITS = 9'd120;
  localparam SecondaryOpcode_POP = 9'd121;
  localparam SecondaryOpcode_TIMERDISABLEH = 9'd122;
  localparam SecondaryOpcode_TIMERDISABLEL = 9'd123;
  localparam SecondaryOpcode_TIMERENABLEH = 9'd124;
  localparam SecondaryOpcode_TIMERENABLEL = 9'd125;
  localparam SecondaryOpcode_LDMEMSTARTVAL = 9'd126;
  localparam SecondaryOpcode_WSUBDB = 9'd129;
  localparam SecondaryOpcode_FPLDNLDBI = 9'd130;
  localparam SecondaryOpcode_FPSTNLDB = 9'd132;
  localparam SecondaryOpcode_FPLDNLSNI = 9'd134;
  localparam SecondaryOpcode_FPADD = 9'd135;
  localparam SecondaryOpcode_FPSTNLSN = 9'd136;
  localparam SecondaryOpcode_FPSUB = 9'd137;
  localparam SecondaryOpcode_FPLDNLDB = 9'd138;
  localparam SecondaryOpcode_FPMUL = 9'd139;
  localparam SecondaryOpcode_FPDIV = 9'd140;
  localparam SecondaryOpcode_FPRANGE = 9'd141;
  localparam SecondaryOpcode_FPLDNLSN = 9'd142;
  localparam SecondaryOpcode_FPNAN = 9'd145;
  localparam SecondaryOpcode_FPORDERED = 9'd146;
  localparam SecondaryOpcode_FPNOTFINITE = 9'd147;
  localparam SecondaryOpcode_FPGT = 9'd148;
  localparam SecondaryOpcode_FPEQ = 9'd149;
  localparam SecondaryOpcode_FPI32TOR32 = 9'd150;
  localparam SecondaryOpcode_FPGE = 9'd151;
  localparam SecondaryOpcode_FPI32TOR64 = 9'd152;
  localparam SecondaryOpcode_FPB32TOR64 = 9'd154;
  localparam SecondaryOpcode_FPLG = 9'd155;
  localparam SecondaryOpcode_FPRTOI32 = 9'd157;
  localparam SecondaryOpcode_FPSTNLI32 = 9'd158;
  localparam SecondaryOpcode_FPLDZEROSN = 9'd159;
  localparam SecondaryOpcode_FPLDZERODB = 9'd160;
  localparam SecondaryOpcode_FPINT = 9'd161;
  localparam SecondaryOpcode_FPDUP = 9'd163;
  localparam SecondaryOpcode_FPREV = 9'd164;
  localparam SecondaryOpcode_FPLDNLADDDB = 9'd166;
  localparam SecondaryOpcode_FPLDNLMULDB = 9'd168;
  localparam SecondaryOpcode_FPLDNLADDSN = 9'd170;
  localparam SecondaryOpcode_FPLDNLMULSN = 9'd172;
  localparam SecondaryOpcode_LDFLAGS = 9'd182;
  localparam SecondaryOpcode_STFLAGS = 9'd183;
  localparam SecondaryOpcode_XBWORD = 9'd184;
  localparam SecondaryOpcode_LBX = 9'd185;
  localparam SecondaryOpcode_CB = 9'd186;
  localparam SecondaryOpcode_CBU = 9'd187;
  localparam SecondaryOpcode_INSPHDR = 9'd188;
  localparam SecondaryOpcode_READBFR = 9'd189;
  localparam SecondaryOpcode_LDCONF = 9'd190;
  localparam SecondaryOpcode_STCONF = 9'd191;
  localparam SecondaryOpcode_LDCNT = 9'd192;
  localparam SecondaryOpcode_SSUB = 9'd193;
  localparam SecondaryOpcode_LDTH = 9'd194;
  localparam SecondaryOpcode_LDCHSTATUS = 9'd195;
  localparam SecondaryOpcode_INTDIS = 9'd196;
  localparam SecondaryOpcode_INTENB = 9'd197;
  localparam SecondaryOpcode_CIR = 9'd199;
  localparam SecondaryOpcode_SS = 9'd200;
  localparam SecondaryOpcode_CHANTYPE = 9'd201;
  localparam SecondaryOpcode_CIRU = 9'd204;
  localparam SecondaryOpcode_FPREM = 9'd207;
  localparam SecondaryOpcode_FPRN = 9'd208;
  localparam SecondaryOpcode_FPDIVBY2 = 9'd209;
  localparam SecondaryOpcode_FPMULBY2 = 9'd210;
  localparam SecondaryOpcode_FPSQRT = 9'd211;
  localparam SecondaryOpcode_FPRP = 9'd212;
  localparam SecondaryOpcode_FPRM = 9'd213;
  localparam SecondaryOpcode_FPRZ = 9'd214;
  localparam SecondaryOpcode_FPR32TOR64 = 9'd215;
  localparam SecondaryOpcode_FPR64TOR32 = 9'd216;
  localparam SecondaryOpcode_FPEXPDEC32 = 9'd217;
  localparam SecondaryOpcode_FPEXPINC32 = 9'd218;
  localparam SecondaryOpcode_FPABS = 9'd219;
  localparam SecondaryOpcode_FPCHKI32 = 9'd220;
  localparam SecondaryOpcode_FPCHKI64 = 9'd221;
  localparam SecondaryOpcode_DEVLB = 9'd240;
  localparam SecondaryOpcode_DEVSB = 9'd241;
  localparam SecondaryOpcode_DEVLS = 9'd242;
  localparam SecondaryOpcode_DEVSS = 9'd243;
  localparam SecondaryOpcode_DEVLW = 9'd244;
  localparam SecondaryOpcode_DEVSW = 9'd245;
  localparam SecondaryOpcode_XSWORD = 9'd248;
  localparam SecondaryOpcode_LSX = 9'd249;
  localparam SecondaryOpcode_CS = 9'd250;
  localparam SecondaryOpcode_CSU = 9'd251;
  localparam SecondaryOpcode_FPSTALL = 9'd257;
  localparam SecondaryOpcode_FPLDALL = 9'd258;
  localparam SecondaryOpcode_STSHADOW = 9'd259;
  localparam SecondaryOpcode_LDSHADOW = 9'd260;
  localparam SecondaryOpcode_TRET = 9'd261;
  localparam SecondaryOpcode_GOPROT = 9'd262;
  localparam SecondaryOpcode_SELTH = 9'd263;
  localparam SecondaryOpcode_SYSCALL = 9'd264;
  localparam SecondaryOpcode_WAIT_1 = 9'd267;
  localparam SecondaryOpcode_SIGNAL_1 = 9'd268;
  localparam SecondaryOpcode_TIMESLICE = 9'd269;
  localparam SecondaryOpcode_INSERTQUEUE = 9'd270;
  localparam SecondaryOpcode_SWAPTIMER = 9'd271;
  localparam SecondaryOpcode_SWAPQUEUE = 9'd272;
  localparam SecondaryOpcode_STOPCH = 9'd274;
  localparam SecondaryOpcode_VOUT = 9'd275;
  localparam SecondaryOpcode_VIN = 9'd276;
  localparam SecondaryOpcode_SWAPBFR = 9'd279;
  localparam SecondaryOpcode_SETHDR = 9'd280;
  localparam SecondaryOpcode_SETCHMODE = 9'd281;
  localparam SecondaryOpcode_INITVLCB = 9'd282;
  localparam SecondaryOpcode_WRITEHDR = 9'd283;
  localparam SecondaryOpcode_READHDR = 9'd284;
  localparam SecondaryOpcode_DISG = 9'd285;
  localparam SecondaryOpcode_ENBG = 9'd286;
  localparam SecondaryOpcode_GRANT = 9'd287;
  localparam SecondaryOpcode_STMOVE2DINIT = 9'd288;
  localparam SecondaryOpcode_CAUSEERROR = 9'd289;
  localparam SecondaryOpcode_UNMKRC = 9'd291;
  localparam SecondaryOpcode_MKRC = 9'd292;
  localparam SecondaryOpcode_IRDSQ = 9'd293;
  localparam SecondaryOpcode_ERDSQ = 9'd294;
  localparam SecondaryOpcode_STRESPTR = 9'd295;
  localparam SecondaryOpcode_LDRESPTR = 9'd296;
  localparam SecondaryOpcode_DEVMOVE = 9'd300;
  localparam SecondaryOpcode_ICL = 9'd301;
  localparam SecondaryOpcode_FDCL = 9'd302;
  localparam SecondaryOpcode_ICA = 9'd303;
  localparam SecondaryOpcode_FDCA = 9'd304;
  localparam SecondaryOpcode_NOP = 9'd320;
  localparam SecondaryOpcode_LDPRODID = 9'd388;
  localparam PrimaryOpcode_J = 4'd0;
  localparam PrimaryOpcode_LDLP = 4'd1;
  localparam PrimaryOpcode_PFIX = 4'd2;
  localparam PrimaryOpcode_LDNL = 4'd3;
  localparam PrimaryOpcode_LDC = 4'd4;
  localparam PrimaryOpcode_LDNLP = 4'd5;
  localparam PrimaryOpcode_NFIX = 4'd6;
  localparam PrimaryOpcode_LDL = 4'd7;
  localparam PrimaryOpcode_ADC = 4'd8;
  localparam PrimaryOpcode_CALL = 4'd9;
  localparam PrimaryOpcode_CJ = 4'd10;
  localparam PrimaryOpcode_AJW = 4'd11;
  localparam PrimaryOpcode_EQC = 4'd12;
  localparam PrimaryOpcode_STL = 4'd13;
  localparam PrimaryOpcode_STNL = 4'd14;
  localparam PrimaryOpcode_OPR = 4'd15;
  localparam RegName_Areg = 6'd0;
  localparam RegName_Breg = 6'd1;
  localparam RegName_Creg = 6'd2;
  localparam RegName_WdescReg = 6'd3;
  localparam RegName_IptrReg = 6'd4;
  localparam RegName_StatusReg = 6'd5;
  localparam RegName_ThReg = 6'd6;
  localparam RegName_FPstatusReg = 6'd7;
  localparam RegName_FPAreg = 6'd8;
  localparam RegName_FPBreg = 6'd9;
  localparam RegName_FPCreg = 6'd10;
  localparam RegName_BMreg0 = 6'd11;
  localparam RegName_BMreg1 = 6'd12;
  localparam RegName_BMreg2 = 6'd13;
  localparam RegName_WIReg = 6'd14;
  localparam RegName_WuReg = 6'd15;
  localparam RegName_Ereg = 6'd16;
  localparam RegName_Xreg = 6'd17;
  localparam RegName_EptrReg = 6'd18;
  localparam RegName_RegionReg0 = 6'd19;
  localparam RegName_RegionReg1 = 6'd20;
  localparam RegName_RegionReg2 = 6'd21;
  localparam RegName_RegionReg3 = 6'd22;
  localparam RegName_PstateReg = 6'd23;
  localparam RegName_WdescStubReg = 6'd24;
  localparam RegName_FptrReg0 = 6'd25;
  localparam RegName_FptrReg1 = 6'd26;
  localparam RegName_BptrReg0 = 6'd27;
  localparam RegName_BptrReg1 = 6'd28;
  localparam RegName_ClockReg0 = 6'd29;
  localparam RegName_ClockReg1 = 6'd30;
  localparam RegName_TptrReg0 = 6'd31;
  localparam RegName_TptrReg1 = 6'd32;
  localparam RegName_TnextReg0 = 6'd33;
  localparam RegName_TnextReg1 = 6'd34;
  localparam LongArithOp_LADD = 5'd0;
  localparam LongArithOp_LSUB = 5'd1;
  localparam LongArithOp_LMUL = 5'd2;
  localparam LongArithOp_LDIV = 5'd3;
  localparam LongArithOp_LSHL = 5'd4;
  localparam LongArithOp_LSHR = 5'd5;
  localparam LongArithOp_LSUM = 5'd6;
  localparam LongArithOp_LDIFF = 5'd7;
  localparam LongArithOp_MINT = 5'd8;
  localparam LongArithOp_BINT = 5'd9;
  localparam LongArithOp_XSWORD = 5'd10;
  localparam LongArithOp_RESCHEDULE = 5'd11;
  localparam LongArithOp_SLMUL = 5'd12;
  localparam LongArithOp_SULMUL = 5'd13;
  localparam LongArithOp_XDBLE = 5'd14;
  localparam LongArithOp_XWORD = 5'd15;
  localparam LongArithOp_NORMALISE = 5'd16;
  localparam LongArithOp_PROD = 5'd17;
  localparam ControlFlowOp_RET = 5'd0;
  localparam ControlFlowOp_LDPI = 5'd1;
  localparam ControlFlowOp_GAJW = 5'd2;
  localparam ControlFlowOp_GCALL = 5'd3;
  localparam ControlFlowOp_LEND = 5'd4;
  localparam ControlFlowOp_ENDP = 5'd5;
  localparam ControlFlowOp_DISS = 5'd6;
  localparam ControlFlowOp_STHF = 5'd7;
  localparam ControlFlowOp_STLF = 5'd8;
  localparam ControlFlowOp_STHB = 5'd9;
  localparam ControlFlowOp_STLB = 5'd10;
  localparam ControlFlowOp_SAVEL = 5'd11;
  localparam ControlFlowOp_SAVEH = 5'd12;
  localparam ControlFlowOp_WCNT = 5'd13;
  localparam ControlFlowOp_SHR = 5'd14;
  localparam ControlFlowOp_SHL = 5'd15;
  localparam ControlFlowOp_NORM = 5'd16;
  localparam ControlFlowOp_LDIV = 5'd17;
  localparam ControlFlowOp_LDIVSTEP = 5'd18;
  localparam ControlFlowOp_UNPACK_SNS = 5'd19;
  localparam ControlFlowOp_POSTNORMSN = 5'd20;
  localparam ControlFlowOp_ROUNDSN = 5'd21;
  localparam ControlFlowOp_LDINF = 5'd22;
  localparam BlockMoveOp_MOVE = 3'd0;
  localparam BlockMoveOp_MOVE2DINIT = 3'd1;
  localparam BlockMoveOp_MOVE2DALL = 3'd2;
  localparam BlockMoveOp_MOVE2DNONZERO = 3'd3;
  localparam BlockMoveOp_MOVE2DZERO = 3'd4;
  localparam IndexingOp_BSUB = 5'd0;
  localparam IndexingOp_WSUB = 5'd1;
  localparam IndexingOp_LB = 5'd2;
  localparam IndexingOp_SB = 5'd3;
  localparam IndexingOp_LSX = 5'd4;
  localparam IndexingOp_SS = 5'd5;
  localparam IndexingOp_LDL = 5'd6;
  localparam IndexingOp_STL = 5'd7;
  localparam IndexingOp_LDNL = 5'd8;
  localparam IndexingOp_STNL = 5'd9;
  localparam IndexingOp_LDLP = 5'd10;
  localparam IndexingOp_LDNLP = 5'd11;
  localparam IndexingOp_LDPI = 5'd12;
  localparam IndexingOp_GAJW = 5'd13;
  localparam IndexingOp_EQC = 5'd14;
  localparam IndexingOp_STLB = 5'd15;
  localparam IndexingOp_LDLB = 5'd16;
  localparam IndexingOp_STNLB = 5'd17;
  localparam IndexingOp_LDNLB = 5'd18;
  localparam RangeCheckOp_CIR = 4'd0;
  localparam RangeCheckOp_CB = 4'd1;
  localparam RangeCheckOp_CS = 4'd2;
  localparam RangeCheckOp_CWORD = 4'd3;
  localparam RangeCheckOp_XSWORD = 4'd4;
  localparam RangeCheckOp_CCNT1 = 4'd5;
  localparam RangeCheckOp_CJ = 4'd6;
  localparam RangeCheckOp_CALL = 4'd7;
  localparam RangeCheckOp_CSNGL = 4'd8;
  localparam RangeCheckOp_CDBL = 4'd9;
  localparam ChannelType_PHYSICAL = 2'd0;
  localparam ChannelType_VIRTUAL_1 = 2'd1;
  localparam ChannelType_RESOURCE = 2'd2;
  localparam ChannelType_INVALID = 2'd3;
  localparam ChannelOp_CHANTYPE = 4'd0;
  localparam ChannelOp_INITVLCB = 4'd1;
  localparam ChannelOp_SETCHMODE = 4'd2;
  localparam ChannelOp_SETHDR = 4'd3;
  localparam ChannelOp_WRITEHDR = 4'd4;
  localparam ChannelOp_READHDR = 4'd5;
  localparam ChannelOp_SWAPBFR = 4'd6;
  localparam ChannelOp_UNMKRC = 4'd7;
  localparam ChannelOp_MKRC = 4'd8;
  localparam InterruptOp_INTDIS = 3'd0;
  localparam InterruptOp_INTENB = 3'd1;
  localparam InterruptOp_LDTRAPPED = 3'd2;
  localparam InterruptOp_STTRAPPED = 3'd3;
  localparam InterruptOp_LDSHADOW = 3'd4;
  localparam InterruptOp_STSHADOW = 3'd5;
  localparam InterruptOp_RESTART = 3'd6;
  localparam InterruptOp_CAUSEERROR = 3'd7;
  localparam ResourceOp_GRANT = 4'd0;
  localparam ResourceOp_ENBG = 4'd1;
  localparam ResourceOp_DISG = 4'd2;
  localparam ResourceOp_MKRC = 4'd3;
  localparam ResourceOp_UNMKRC = 4'd4;
  localparam ResourceOp_IRDSQ = 4'd5;
  localparam ResourceOp_ERDSQ = 4'd6;
  localparam ResourceOp_STRESPTR = 4'd7;
  localparam ResourceOp_LDRESPTR = 4'd8;
  localparam SystemOp_TESTPRANAL = 3'd0;
  localparam SystemOp_LDCONF = 3'd1;
  localparam SystemOp_STCONF = 3'd2;
  localparam SystemOp_SYSREQ = 3'd3;
  localparam SystemOp_DEVMOVE = 3'd4;
  localparam SystemOp_SETTIMESLICE = 3'd5;
  localparam SystemOp_LDMEMSTARTVAL = 3'd6;
  localparam ProcessState_RUNNING = 2'd0;
  localparam ProcessState_READY = 2'd1;
  localparam ProcessState_WAITING = 2'd2;
  localparam ProcessState_TERMINATED = 2'd3;

  reg                 fpuAdder_1_io_cmd_valid;
  reg        [63:0]   fpuAdder_1_io_cmd_payload_a;
  reg        [63:0]   fpuAdder_1_io_cmd_payload_b;
  reg                 fpuAdder_1_io_cmd_payload_sub;
  reg        [1:0]    fpuAdder_1_io_cmd_payload_rounding;
  wire                streamFifo_2_io_push_valid;
  reg                 streamFifo_2_io_pop_ready;
  wire                streamFifo_3_io_push_valid;
  reg                 streamFifo_3_io_pop_ready;
  wire                bmbUpSizerBridge_1_io_input_cmd_ready;
  wire                bmbUpSizerBridge_1_io_input_rsp_valid;
  wire                bmbUpSizerBridge_1_io_input_rsp_payload_last;
  wire       [0:0]    bmbUpSizerBridge_1_io_input_rsp_payload_fragment_source;
  wire       [0:0]    bmbUpSizerBridge_1_io_input_rsp_payload_fragment_opcode;
  wire       [63:0]   bmbUpSizerBridge_1_io_input_rsp_payload_fragment_data;
  wire                bmbUpSizerBridge_1_io_output_cmd_valid;
  wire                bmbUpSizerBridge_1_io_output_cmd_payload_last;
  wire       [0:0]    bmbUpSizerBridge_1_io_output_cmd_payload_fragment_source;
  wire       [0:0]    bmbUpSizerBridge_1_io_output_cmd_payload_fragment_opcode;
  wire       [31:0]   bmbUpSizerBridge_1_io_output_cmd_payload_fragment_address;
  wire       [2:0]    bmbUpSizerBridge_1_io_output_cmd_payload_fragment_length;
  wire       [127:0]  bmbUpSizerBridge_1_io_output_cmd_payload_fragment_data;
  wire       [15:0]   bmbUpSizerBridge_1_io_output_cmd_payload_fragment_mask;
  wire       [1:0]    bmbUpSizerBridge_1_io_output_cmd_payload_fragment_context;
  wire                bmbUpSizerBridge_1_io_output_rsp_ready;
  wire                fpuAdder_1_io_cmd_ready;
  wire                fpuAdder_1_io_rsp_valid;
  wire       [63:0]   fpuAdder_1_io_rsp_payload;
  wire                streamFifo_2_io_push_ready;
  wire                streamFifo_2_io_pop_valid;
  wire       [31:0]   streamFifo_2_io_pop_payload;
  wire       [4:0]    streamFifo_2_io_occupancy;
  wire       [4:0]    streamFifo_2_io_availability;
  wire                streamFifo_3_io_push_ready;
  wire                streamFifo_3_io_pop_valid;
  wire       [31:0]   streamFifo_3_io_pop_payload;
  wire       [4:0]    streamFifo_3_io_occupancy;
  wire       [4:0]    streamFifo_3_io_availability;
  wire       [3:0]    _zz_8;
  wire       [8:0]    _zz_9;
  wire       [3:0]    _zz_10;
  wire       [8:0]    _zz_11;
  wire       [3:0]    _zz_12;
  wire       [8:0]    _zz_13;
  wire       [3:0]    _zz_14;
  wire       [8:0]    _zz_15;
  wire       [3:0]    _zz_16;
  wire       [8:0]    _zz_17;
  wire       [3:0]    _zz_18;
  wire       [8:0]    _zz_19;
  wire       [3:0]    _zz_20;
  wire       [8:0]    _zz_21;
  wire       [3:0]    _zz_22;
  wire       [8:0]    _zz_23;
  wire       [3:0]    _zz_24;
  wire       [8:0]    _zz_25;
  wire       [3:0]    _zz_26;
  wire       [8:0]    _zz_27;
  wire       [3:0]    _zz_when;
  wire       [8:0]    _zz_when_1;
  wire       [3:0]    _zz_when_2;
  wire       [8:0]    _zz_when_3;
  wire       [3:0]    _zz_when_4;
  wire       [8:0]    _zz_when_5;
  wire       [3:0]    _zz_when_6;
  wire       [8:0]    _zz_when_7;
  wire       [3:0]    _zz_when_8;
  wire       [8:0]    _zz_when_9;
  wire       [3:0]    _zz_when_10;
  wire       [8:0]    _zz_when_11;
  wire       [3:0]    _zz_when_12;
  wire       [8:0]    _zz_when_13;
  wire       [3:0]    _zz_when_14;
  wire       [8:0]    _zz_when_15;
  wire       [3:0]    _zz_when_16;
  wire       [8:0]    _zz_when_17;
  wire       [3:0]    _zz_when_18;
  wire       [8:0]    _zz_when_19;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_15;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_15_1;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_15_2;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_17;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_17_1;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_17_2;
  wire       [3:0]    _zz_28;
  wire       [8:0]    _zz_29;
  wire       [3:0]    _zz_30;
  wire       [8:0]    _zz_31;
  wire       [3:0]    _zz_32;
  wire       [8:0]    _zz_33;
  wire       [3:0]    _zz_34;
  wire       [8:0]    _zz_35;
  wire       [3:0]    _zz_36;
  wire       [8:0]    _zz_37;
  wire       [3:0]    _zz_38;
  wire       [8:0]    _zz_39;
  wire       [3:0]    _zz_40;
  wire       [8:0]    _zz_41;
  wire       [3:0]    _zz_42;
  wire       [8:0]    _zz_43;
  wire       [3:0]    _zz_44;
  wire       [8:0]    _zz_45;
  wire       [3:0]    _zz_when_LongArithPlugin_l142_3;
  wire       [8:0]    _zz_when_LongArithPlugin_l142_4;
  wire       [3:0]    _zz_when_LongArithPlugin_l142_5;
  wire       [8:0]    _zz_when_LongArithPlugin_l142_6;
  wire       [3:0]    _zz_when_LongArithPlugin_l142_7;
  wire       [8:0]    _zz_when_LongArithPlugin_l142_8;
  wire       [3:0]    _zz_when_LongArithPlugin_l142_9;
  wire       [8:0]    _zz_when_LongArithPlugin_l142_10;
  wire       [3:0]    _zz_when_LongArithPlugin_l142_11;
  wire       [8:0]    _zz_when_LongArithPlugin_l142_12;
  wire       [3:0]    _zz_when_LongArithPlugin_l142_13;
  wire       [8:0]    _zz_when_LongArithPlugin_l142_14;
  wire       [3:0]    _zz_when_LongArithPlugin_l142_15;
  wire       [8:0]    _zz_when_LongArithPlugin_l142_16;
  wire       [3:0]    _zz_when_LongArithPlugin_l142_17;
  wire       [8:0]    _zz_when_LongArithPlugin_l142_18;
  wire       [3:0]    _zz_when_LongArithPlugin_l142_19;
  wire       [8:0]    _zz_when_LongArithPlugin_l142_20;
  wire       [32:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_32;
  wire       [64:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35;
  wire       [32:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35_1;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35_2;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35_3;
  wire       [64:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35_4;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35_5;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_36;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_37;
  wire       [63:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_38;
  wire       [63:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_38_1;
  wire       [63:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_42;
  wire       [63:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_42_1;
  wire       [63:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_43;
  wire       [63:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_43_1;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_50;
  wire       [0:0]    _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_50_1;
  wire       [63:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_53;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_53_1;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_57;
  wire       [63:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_57_1;
  wire       [63:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_57_2;
  wire       [3:0]    _zz_when_ControlFlowPlugin_l115_3;
  wire       [8:0]    _zz_when_ControlFlowPlugin_l115_4;
  wire       [3:0]    _zz_when_ControlFlowPlugin_l115_5;
  wire       [8:0]    _zz_when_ControlFlowPlugin_l115_6;
  wire       [3:0]    _zz_when_ControlFlowPlugin_l115_7;
  wire       [8:0]    _zz_when_ControlFlowPlugin_l115_8;
  wire       [3:0]    _zz_when_ControlFlowPlugin_l115_9;
  wire       [8:0]    _zz_when_ControlFlowPlugin_l115_10;
  wire       [3:0]    _zz_when_ControlFlowPlugin_l115_11;
  wire       [8:0]    _zz_when_ControlFlowPlugin_l115_12;
  wire       [3:0]    _zz_when_ControlFlowPlugin_l115_13;
  wire       [8:0]    _zz_when_ControlFlowPlugin_l115_14;
  wire       [3:0]    _zz_when_ControlFlowPlugin_l115_15;
  wire       [8:0]    _zz_when_ControlFlowPlugin_l115_16;
  wire       [3:0]    _zz_46;
  wire       [8:0]    _zz_47;
  wire       [3:0]    _zz_48;
  wire       [8:0]    _zz_49;
  wire       [3:0]    _zz_50;
  wire       [8:0]    _zz_51;
  wire       [3:0]    _zz_52;
  wire       [8:0]    _zz_53;
  wire       [3:0]    _zz_54;
  wire       [8:0]    _zz_55;
  wire       [3:0]    _zz_56;
  wire       [8:0]    _zz_57;
  wire       [3:0]    _zz_58;
  wire       [8:0]    _zz_59;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_64;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_67;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_69;
  wire       [63:0]   _zz__zz_when_BlockMovePlugin_l165;
  wire                _zz_when_IndexingPlugin_l135_5;
  wire                _zz_when_IndexingPlugin_l135_6;
  wire       [3:0]    _zz_when_IndexingPlugin_l135_7;
  wire       [3:0]    _zz_when_IndexingPlugin_l135_8;
  wire                _zz_when_IndexingPlugin_l135_9;
  wire       [3:0]    _zz_when_IndexingPlugin_l135_10;
  wire       [8:0]    _zz_when_IndexingPlugin_l135_11;
  wire                _zz_when_IndexingPlugin_l135_12;
  wire       [3:0]    _zz_when_IndexingPlugin_l135_13;
  wire       [8:0]    _zz_when_IndexingPlugin_l135_14;
  wire       [3:0]    _zz_when_IndexingPlugin_l135_15;
  wire       [8:0]    _zz_when_IndexingPlugin_l135_16;
  wire       [3:0]    _zz_when_IndexingPlugin_l135_17;
  wire       [8:0]    _zz_when_IndexingPlugin_l135_18;
  wire       [3:0]    _zz_when_IndexingPlugin_l135_19;
  wire       [8:0]    _zz_when_IndexingPlugin_l135_20;
  wire       [3:0]    _zz_when_IndexingPlugin_l135_21;
  wire       [8:0]    _zz_when_IndexingPlugin_l135_22;
  wire       [3:0]    _zz_60;
  wire       [8:0]    _zz_61;
  wire       [3:0]    _zz_62;
  wire       [8:0]    _zz_63;
  wire       [3:0]    _zz_64;
  wire       [8:0]    _zz_65;
  wire       [3:0]    _zz_66;
  wire       [8:0]    _zz_67;
  wire       [3:0]    _zz_68;
  wire       [8:0]    _zz_69;
  wire       [3:0]    _zz_70;
  wire       [8:0]    _zz_71;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_86;
  wire       [29:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77;
  wire       [29:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_1;
  wire       [29:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_2;
  wire       [29:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_3;
  wire       [29:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_4;
  wire       [29:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_5;
  wire       [33:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_6;
  wire       [33:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_7;
  wire       [33:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_8;
  wire                _zz_when_RangeCheckPlugin_l124_5;
  wire                _zz_when_RangeCheckPlugin_l124_6;
  wire       [3:0]    _zz_when_RangeCheckPlugin_l124_7;
  wire       [3:0]    _zz_when_RangeCheckPlugin_l124_8;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_103;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_106;
  wire       [3:0]    _zz_when_GeneralPlugin_l69_2;
  wire       [8:0]    _zz_when_GeneralPlugin_l69_3;
  wire       [3:0]    _zz_when_GeneralPlugin_l69_4;
  wire       [8:0]    _zz_when_GeneralPlugin_l69_5;
  wire       [3:0]    _zz_when_GeneralPlugin_l69_6;
  wire       [8:0]    _zz_when_GeneralPlugin_l69_7;
  wire       [3:0]    _zz_when_GeneralPlugin_l69_8;
  wire       [8:0]    _zz_when_GeneralPlugin_l69_9;
  wire       [3:0]    _zz_when_GeneralPlugin_l69_10;
  wire       [8:0]    _zz_when_GeneralPlugin_l69_11;
  wire       [3:0]    _zz_72;
  wire       [8:0]    _zz_73;
  wire       [3:0]    _zz_74;
  wire       [8:0]    _zz_75;
  wire       [3:0]    _zz_76;
  wire       [8:0]    _zz_77;
  wire       [3:0]    _zz_78;
  wire       [8:0]    _zz_79;
  wire       [3:0]    _zz_80;
  wire       [8:0]    _zz_81;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_121;
  wire       [0:0]    _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_121_1;
  reg                 _zz_when_ChannelPlugin_l129_17;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_145;
  wire       [1:0]    _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_145_1;
  reg        [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_152;
  wire       [7:0]    _zz_82;
  wire       [8:0]    _zz_83;
  wire       [7:0]    _zz_84;
  wire       [8:0]    _zz_85;
  wire       [7:0]    _zz_86;
  wire       [8:0]    _zz_87;
  wire       [7:0]    _zz_88;
  wire       [8:0]    _zz_89;
  wire       [7:0]    _zz_90;
  wire       [8:0]    _zz_91;
  wire       [7:0]    _zz_92;
  wire       [8:0]    _zz_93;
  wire       [7:0]    _zz_94;
  wire       [8:0]    _zz_95;
  wire       [7:0]    _zz_96;
  wire       [8:0]    _zz_97;
  wire       [7:0]    _zz_98;
  wire       [8:0]    _zz_99;
  wire       [7:0]    _zz_100;
  wire       [8:0]    _zz_101;
  wire       [7:0]    _zz_102;
  wire       [8:0]    _zz_103;
  wire       [7:0]    _zz_104;
  wire       [8:0]    _zz_105;
  wire       [7:0]    _zz_106;
  wire       [8:0]    _zz_107;
  wire       [7:0]    _zz_108;
  wire       [8:0]    _zz_109;
  wire       [7:0]    _zz_110;
  wire       [8:0]    _zz_111;
  wire       [7:0]    _zz_fpu_fpPipe_ctrl_0_up_valid_2;
  wire       [8:0]    _zz_fpu_fpPipe_ctrl_0_up_valid_3;
  wire       [7:0]    _zz_fpu_fpPipe_ctrl_0_up_valid_4;
  wire       [8:0]    _zz_fpu_fpPipe_ctrl_0_up_valid_5;
  wire       [7:0]    _zz_fpu_fpPipe_ctrl_0_up_valid_6;
  wire       [8:0]    _zz_fpu_fpPipe_ctrl_0_up_valid_7;
  wire       [7:0]    _zz_fpu_fpPipe_ctrl_0_up_valid_8;
  wire       [8:0]    _zz_fpu_fpPipe_ctrl_0_up_valid_9;
  wire                _zz_when_20;
  wire       [7:0]    _zz_112;
  wire       [8:0]    _zz_113;
  wire       [7:0]    _zz_114;
  wire       [8:0]    _zz_115;
  wire       [7:0]    _zz_116;
  wire       [8:0]    _zz_117;
  wire       [7:0]    _zz_118;
  wire       [8:0]    _zz_119;
  wire       [7:0]    _zz_120;
  wire       [8:0]    _zz_121;
  wire       [7:0]    _zz_122;
  wire       [8:0]    _zz_123;
  wire       [7:0]    _zz_124;
  wire       [8:0]    _zz_125;
  wire       [7:0]    _zz_126;
  wire       [8:0]    _zz_127;
  wire       [7:0]    _zz_128;
  wire       [8:0]    _zz_129;
  wire       [7:0]    _zz_130;
  wire       [8:0]    _zz_131;
  wire       [7:0]    _zz_132;
  wire       [8:0]    _zz_133;
  wire       [7:0]    _zz_134;
  wire       [8:0]    _zz_135;
  wire       [7:0]    _zz_136;
  wire       [8:0]    _zz_137;
  wire       [7:0]    _zz_138;
  wire       [8:0]    _zz_139;
  wire       [7:0]    _zz_140;
  wire       [8:0]    _zz_141;
  wire       [7:0]    _zz_142;
  wire       [8:0]    _zz_143;
  wire       [7:0]    _zz_144;
  wire       [8:0]    _zz_145;
  wire       [7:0]    _zz_146;
  wire       [8:0]    _zz_147;
  wire       [7:0]    _zz_148;
  wire       [8:0]    _zz_149;
  wire       [7:0]    _zz_150;
  wire       [8:0]    _zz_151;
  wire       [7:0]    _zz_152;
  wire       [8:0]    _zz_153;
  wire       [7:0]    _zz_154;
  wire       [8:0]    _zz_155;
  wire       [7:0]    _zz_156;
  wire       [8:0]    _zz_157;
  wire       [7:0]    _zz_158;
  wire       [8:0]    _zz_159;
  wire       [7:0]    _zz_160;
  wire       [8:0]    _zz_161;
  wire       [7:0]    _zz_162;
  wire       [8:0]    _zz_163;
  wire       [7:0]    _zz_164;
  wire       [8:0]    _zz_165;
  wire       [7:0]    _zz_166;
  wire       [8:0]    _zz_167;
  wire       [7:0]    _zz_168;
  wire       [8:0]    _zz_169;
  wire       [7:0]    _zz_170;
  wire       [8:0]    _zz_171;
  wire       [7:0]    _zz_172;
  wire       [8:0]    _zz_173;
  wire       [7:0]    _zz_174;
  wire       [8:0]    _zz_175;
  wire       [10:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_229;
  wire       [9:0]    _zz_fpu_fpPipe_ctrl_0_down_RESULT_230;
  wire       [9:0]    _zz_fpu_fpPipe_ctrl_0_down_RESULT_231;
  wire       [9:0]    _zz_fpu_fpPipe_ctrl_0_down_RESULT_232;
  wire       [9:0]    _zz_fpu_fpPipe_ctrl_0_down_RESULT_233;
  wire       [7:0]    _zz_fpu_fpPipe_ctrl_0_down_RESULT_234;
  wire       [63:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_217;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_235;
  wire       [10:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_236;
  wire       [10:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_237;
  wire       [10:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_238;
  wire       [22:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_239;
  wire       [51:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_240;
  wire       [31:0]   _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_219;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_241;
  wire       [7:0]    _zz_fpu_fpPipe_ctrl_0_down_RESULT_242;
  wire       [22:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_243;
  wire       [51:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_244;
  wire       [51:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_245;
  wire       [0:0]    _zz_fpu_fpPipe_ctrl_0_down_RESULT_246;
  wire       [31:0]   _zz_io_push_payload;
  wire                _zz_io_push_valid;
  wire                _zz_io_push_valid_1;
  wire       [31:0]   _zz_io_push_payload_1;
  wire                _zz_io_push_valid_2;
  wire                _zz_io_push_valid_3;
  wire                fpu_fpPipe_ctrl_0_down_isValid;
  wire                fpu_fpPipe_ctrl_0_up_isReady;
  reg                 fpu_fpPipe_ctrl_0_down_valid;
  reg                 fpu_fpPipe_ctrl_0_up_ready;
  wire       [9:0]    fpu_fpPipe_ctrl_0_down_CYCLE_CNT;
  wire                fpu_fpPipe_ctrl_0_up_isFiring;
  wire                fpu_fpPipe_ctrl_0_down_isFiring;
  reg        [63:0]   fpu_fpPipe_ctrl_0_down_T805_STATE;
  reg        [9:0]    fpu_fpPipe_ctrl_0_down_MAX_CYCLES;
  reg        [55:0]   fpu_fpPipe_ctrl_0_down_RESULT_AFIX;
  reg        [63:0]   fpu_fpPipe_ctrl_0_down_RESULT;
  wire                fpu_fpPipe_ctrl_0_down_isReady;
  wire                fpu_fpPipe_ctrl_0_up_isValid;
  wire                execute_up_isValid;
  wire                fpu_fpPipe_ctrl_0_up_valid;
  wire       [7:0]    _zz_fpu_fpPipe_ctrl_0_up_valid;
  wire       [7:0]    _zz_1;
  wire       [63:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT;
  wire       [63:0]   _zz_io_cmd_payload_b;
  wire       [1:0]    _zz_switch_Misc_l245;
  wire                _zz_fpu_fpPipe_ctrl_0_up_valid_1;
  wire       [7:0]    _zz_when_SystemPlugin_l123;
  wire       [7:0]    _zz_when_ResourcePlugin_l114;
  wire       [7:0]    _zz_when_InterruptPlugin_l118;
  wire       [7:0]    _zz_when_ChannelPlugin_l111;
  wire       [7:0]    _zz_when_GeneralPlugin_l69;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_1;
  wire       [7:0]    _zz_when_RangeCheckPlugin_l124;
  wire       [7:0]    _zz_when_IndexingPlugin_l135;
  wire       [7:0]    _zz_when_BlockMovePlugin_l113;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_2;
  wire       [7:0]    _zz_when_ControlFlowPlugin_l115;
  wire       [7:0]    _zz_when_LongArithPlugin_l142;
  wire       [7:0]    _zz_2;
  wire                writeback_down_isReady;
  wire                writeback_down_isValid;
  wire                execute_down_isReady;
  wire                execute_down_isValid;
  wire                addressCache_down_isReady;
  wire                addressCache_down_isValid;
  wire                localDecode_down_isReady;
  wire                localDecode_down_isValid;
  wire                fetchGroup_down_isReady;
  wire                fetchGroup_down_isValid;
  wire                writeback_down_isFiring;
  wire                execute_down_isFiring;
  wire                addressCache_down_isFiring;
  wire                localDecode_down_isFiring;
  wire                fetchGroup_down_isFiring;
  wire                _zz_io_input_cmd_valid;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
  wire                _zz_io_input_cmd_valid_1;
  wire       [63:0]   _zz_io_input_cmd_payload_fragment_data;
  wire       [7:0]    _zz_io_input_cmd_payload_fragment_mask;
  wire                _zz_io_input_rsp_ready;
  reg        [31:0]   _zz_io_input_cmd_payload_fragment_address;
  reg                 _zz_io_input_cmd_valid_2;
  reg        [4:0]    switch_ArithmeticPlugin_l160;
  wire       [3:0]    _zz_3;
  wire       [5:0]    switch_Misc_l245;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_9;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_10;
  wire       [5:0]    switch_Misc_l245_1;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_11;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_12;
  wire       [5:0]    switch_Misc_l245_2;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_13;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_14;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_15;
  wire                when_RegStackPlugin_l144;
  wire                when_RegStackPlugin_l146;
  wire                when_RegStackPlugin_l148;
  wire                when_RegStackPlugin_l150;
  wire                when_RegStackPlugin_l152;
  wire                when_RegStackPlugin_l154;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_16;
  wire                when_RegStackPlugin_l144_1;
  wire                when_RegStackPlugin_l146_1;
  wire                when_RegStackPlugin_l148_1;
  wire                when_RegStackPlugin_l150_1;
  wire                when_RegStackPlugin_l152_1;
  wire                when_RegStackPlugin_l154_1;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_17;
  wire                when_RegStackPlugin_l144_2;
  wire                when_RegStackPlugin_l146_2;
  wire                when_RegStackPlugin_l148_2;
  wire                when_RegStackPlugin_l150_2;
  wire                when_RegStackPlugin_l152_2;
  wire                when_RegStackPlugin_l154_2;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_18;
  wire                when_RegStackPlugin_l144_3;
  wire                when_RegStackPlugin_l146_3;
  wire                when_RegStackPlugin_l148_3;
  wire                when_RegStackPlugin_l150_3;
  wire                when_RegStackPlugin_l152_3;
  wire                when_RegStackPlugin_l154_3;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_19;
  wire                when_RegStackPlugin_l144_4;
  wire                when_RegStackPlugin_l146_4;
  wire                when_RegStackPlugin_l148_4;
  wire                when_RegStackPlugin_l150_4;
  wire                when_RegStackPlugin_l152_4;
  wire                when_RegStackPlugin_l154_4;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_20;
  wire                when_RegStackPlugin_l144_5;
  wire                when_RegStackPlugin_l146_5;
  wire                when_RegStackPlugin_l148_5;
  wire                when_RegStackPlugin_l150_5;
  wire                when_RegStackPlugin_l152_5;
  wire                when_RegStackPlugin_l154_5;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_21;
  wire                when_RegStackPlugin_l144_6;
  wire                when_RegStackPlugin_l146_6;
  wire                when_RegStackPlugin_l148_6;
  wire                when_RegStackPlugin_l150_6;
  wire                when_RegStackPlugin_l152_6;
  wire                when_RegStackPlugin_l154_6;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_22;
  wire                when_RegStackPlugin_l144_7;
  wire                when_RegStackPlugin_l146_7;
  wire                when_RegStackPlugin_l148_7;
  wire                when_RegStackPlugin_l150_7;
  wire                when_RegStackPlugin_l152_7;
  wire                when_RegStackPlugin_l154_7;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_23;
  wire                when_RegStackPlugin_l144_8;
  wire                when_RegStackPlugin_l146_8;
  wire                when_RegStackPlugin_l148_8;
  wire                when_RegStackPlugin_l150_8;
  wire                when_RegStackPlugin_l152_8;
  wire                when_RegStackPlugin_l154_8;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_24;
  wire                when_RegStackPlugin_l144_9;
  wire                when_RegStackPlugin_l146_9;
  wire                when_RegStackPlugin_l148_9;
  wire                when_RegStackPlugin_l150_9;
  wire                when_RegStackPlugin_l152_9;
  wire                when_RegStackPlugin_l154_9;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_25;
  wire                when_RegStackPlugin_l144_10;
  wire                when_RegStackPlugin_l146_10;
  wire                when_RegStackPlugin_l148_10;
  wire                when_RegStackPlugin_l150_10;
  wire                when_RegStackPlugin_l152_10;
  wire                when_RegStackPlugin_l154_10;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_26;
  wire                when_RegStackPlugin_l144_11;
  wire                when_RegStackPlugin_l146_11;
  wire                when_RegStackPlugin_l148_11;
  wire                when_RegStackPlugin_l150_11;
  wire                when_RegStackPlugin_l152_11;
  wire                when_RegStackPlugin_l154_11;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_27;
  wire                when_RegStackPlugin_l144_12;
  wire                when_RegStackPlugin_l146_12;
  wire                when_RegStackPlugin_l148_12;
  wire                when_RegStackPlugin_l150_12;
  wire                when_RegStackPlugin_l152_12;
  wire                when_RegStackPlugin_l154_12;
  reg                 when_LongArithPlugin_l312;
  reg        [3:0]    _zz_when_LongArithPlugin_l312;
  reg        [4:0]    switch_LongArithPlugin_l132;
  reg        [3:0]    _zz_when_LongArithPlugin_l203;
  wire                _zz_when_LongArithPlugin_l142_1;
  wire       [3:0]    _zz_when_LongArithPlugin_l142_2;
  wire                when_LongArithPlugin_l142;
  wire       [5:0]    switch_Misc_l245_3;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_28;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_29;
  wire       [5:0]    switch_Misc_l245_4;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_30;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_31;
  wire       [5:0]    switch_Misc_l245_5;
  reg        [31:0]   _zz_when_LongArithPlugin_l220;
  wire       [31:0]   _zz_when_LongArithPlugin_l220_1;
  wire       [64:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_32;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_33;
  wire                when_RegStackPlugin_l144_13;
  wire                when_RegStackPlugin_l146_13;
  wire                when_RegStackPlugin_l148_13;
  wire                when_RegStackPlugin_l150_13;
  wire                when_RegStackPlugin_l152_13;
  wire                when_RegStackPlugin_l154_13;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_34;
  wire                when_RegStackPlugin_l144_14;
  wire                when_RegStackPlugin_l146_14;
  wire                when_RegStackPlugin_l148_14;
  wire                when_RegStackPlugin_l150_14;
  wire                when_RegStackPlugin_l152_14;
  wire                when_RegStackPlugin_l154_14;
  wire       [64:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_35;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_36;
  wire                when_RegStackPlugin_l144_15;
  wire                when_RegStackPlugin_l146_15;
  wire                when_RegStackPlugin_l148_15;
  wire                when_RegStackPlugin_l150_15;
  wire                when_RegStackPlugin_l152_15;
  wire                when_RegStackPlugin_l154_15;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_37;
  wire                when_RegStackPlugin_l144_16;
  wire                when_RegStackPlugin_l146_16;
  wire                when_RegStackPlugin_l148_16;
  wire                when_RegStackPlugin_l150_16;
  wire                when_RegStackPlugin_l152_16;
  wire                when_RegStackPlugin_l154_16;
  wire                when_LongArithPlugin_l203;
  wire       [127:0]  _zz_fpu_fpPipe_ctrl_0_down_RESULT_38;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_39;
  wire                when_RegStackPlugin_l144_17;
  wire                when_RegStackPlugin_l146_17;
  wire                when_RegStackPlugin_l148_17;
  wire                when_RegStackPlugin_l150_17;
  wire                when_RegStackPlugin_l152_17;
  wire                when_RegStackPlugin_l154_17;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_40;
  wire                when_RegStackPlugin_l144_18;
  wire                when_RegStackPlugin_l146_18;
  wire                when_RegStackPlugin_l148_18;
  wire                when_RegStackPlugin_l150_18;
  wire                when_RegStackPlugin_l152_18;
  wire                when_RegStackPlugin_l154_18;
  wire                when_LongArithPlugin_l220;
  wire       [63:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_41;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_42;
  wire                when_RegStackPlugin_l144_19;
  wire                when_RegStackPlugin_l146_19;
  wire                when_RegStackPlugin_l148_19;
  wire                when_RegStackPlugin_l150_19;
  wire                when_RegStackPlugin_l152_19;
  wire                when_RegStackPlugin_l154_19;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_43;
  wire                when_RegStackPlugin_l144_20;
  wire                when_RegStackPlugin_l146_20;
  wire                when_RegStackPlugin_l148_20;
  wire                when_RegStackPlugin_l150_20;
  wire                when_RegStackPlugin_l152_20;
  wire                when_RegStackPlugin_l154_20;
  wire       [63:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_44;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_45;
  wire                when_RegStackPlugin_l144_21;
  wire                when_RegStackPlugin_l146_21;
  wire                when_RegStackPlugin_l148_21;
  wire                when_RegStackPlugin_l150_21;
  wire                when_RegStackPlugin_l152_21;
  wire                when_RegStackPlugin_l154_21;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_46;
  wire                when_RegStackPlugin_l144_22;
  wire                when_RegStackPlugin_l146_22;
  wire                when_RegStackPlugin_l148_22;
  wire                when_RegStackPlugin_l150_22;
  wire                when_RegStackPlugin_l152_22;
  wire                when_RegStackPlugin_l154_22;
  wire       [63:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_47;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_48;
  wire                when_RegStackPlugin_l144_23;
  wire                when_RegStackPlugin_l146_23;
  wire                when_RegStackPlugin_l148_23;
  wire                when_RegStackPlugin_l150_23;
  wire                when_RegStackPlugin_l152_23;
  wire                when_RegStackPlugin_l154_23;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_49;
  wire                when_RegStackPlugin_l144_24;
  wire                when_RegStackPlugin_l146_24;
  wire                when_RegStackPlugin_l148_24;
  wire                when_RegStackPlugin_l150_24;
  wire                when_RegStackPlugin_l152_24;
  wire                when_RegStackPlugin_l154_24;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_50;
  wire                when_RegStackPlugin_l144_25;
  wire                when_RegStackPlugin_l146_25;
  wire                when_RegStackPlugin_l148_25;
  wire                when_RegStackPlugin_l150_25;
  wire                when_RegStackPlugin_l152_25;
  wire                when_RegStackPlugin_l154_25;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_51;
  wire                when_RegStackPlugin_l144_26;
  wire                when_RegStackPlugin_l146_26;
  wire                when_RegStackPlugin_l148_26;
  wire                when_RegStackPlugin_l150_26;
  wire                when_RegStackPlugin_l152_26;
  wire                when_RegStackPlugin_l154_26;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_52;
  wire                when_RegStackPlugin_l144_27;
  wire                when_RegStackPlugin_l146_27;
  wire                when_RegStackPlugin_l148_27;
  wire                when_RegStackPlugin_l150_27;
  wire                when_RegStackPlugin_l152_27;
  wire                when_RegStackPlugin_l154_27;
  wire       [63:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_53;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_54;
  wire                when_RegStackPlugin_l144_28;
  wire                when_RegStackPlugin_l146_28;
  wire                when_RegStackPlugin_l148_28;
  wire                when_RegStackPlugin_l150_28;
  wire                when_RegStackPlugin_l152_28;
  wire                when_RegStackPlugin_l154_28;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_55;
  wire                when_RegStackPlugin_l144_29;
  wire                when_RegStackPlugin_l146_29;
  wire                when_RegStackPlugin_l148_29;
  wire                when_RegStackPlugin_l150_29;
  wire                when_RegStackPlugin_l152_29;
  wire                when_RegStackPlugin_l154_29;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_56;
  wire                when_RegStackPlugin_l144_30;
  wire                when_RegStackPlugin_l146_30;
  wire                when_RegStackPlugin_l148_30;
  wire                when_RegStackPlugin_l150_30;
  wire                when_RegStackPlugin_l152_30;
  wire                when_RegStackPlugin_l154_30;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_57;
  wire                when_RegStackPlugin_l144_31;
  wire                when_RegStackPlugin_l146_31;
  wire                when_RegStackPlugin_l148_31;
  wire                when_RegStackPlugin_l150_31;
  wire                when_RegStackPlugin_l152_31;
  wire                when_RegStackPlugin_l154_31;
  wire                when_LongArithPlugin_l313;
  wire                when_ControlFlowPlugin_l115;
  reg        [4:0]    switch_ControlFlowPlugin_l132;
  wire                _zz_when_ControlFlowPlugin_l115_1;
  wire       [3:0]    _zz_when_ControlFlowPlugin_l115_2;
  wire       [5:0]    switch_Misc_l245_6;
  reg        [31:0]   _zz_when_ControlFlowPlugin_l171;
  wire       [31:0]   _zz_when_ControlFlowPlugin_l171_1;
  wire       [5:0]    switch_Misc_l245_7;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_58;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_59;
  wire       [5:0]    switch_Misc_l245_8;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_60;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_61;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_62;
  wire                when_RegStackPlugin_l144_32;
  wire                when_RegStackPlugin_l146_32;
  wire                when_RegStackPlugin_l148_32;
  wire                when_RegStackPlugin_l150_32;
  wire                when_RegStackPlugin_l152_32;
  wire                when_RegStackPlugin_l154_32;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_63;
  wire                when_RegStackPlugin_l144_33;
  wire                when_RegStackPlugin_l146_33;
  wire                when_RegStackPlugin_l148_33;
  wire                when_RegStackPlugin_l150_33;
  wire                when_RegStackPlugin_l152_33;
  wire                when_RegStackPlugin_l154_33;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_64;
  wire                when_RegStackPlugin_l144_34;
  wire                when_RegStackPlugin_l146_34;
  wire                when_RegStackPlugin_l148_34;
  wire                when_RegStackPlugin_l150_34;
  wire                when_RegStackPlugin_l152_34;
  wire                when_RegStackPlugin_l154_34;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_65;
  wire                when_RegStackPlugin_l144_35;
  wire                when_RegStackPlugin_l146_35;
  wire                when_RegStackPlugin_l148_35;
  wire                when_RegStackPlugin_l150_35;
  wire                when_RegStackPlugin_l152_35;
  wire                when_RegStackPlugin_l154_35;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_66;
  wire                when_RegStackPlugin_l144_36;
  wire                when_RegStackPlugin_l146_36;
  wire                when_RegStackPlugin_l148_36;
  wire                when_RegStackPlugin_l150_36;
  wire                when_RegStackPlugin_l152_36;
  wire                when_RegStackPlugin_l154_36;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_67;
  wire                when_RegStackPlugin_l144_37;
  wire                when_RegStackPlugin_l146_37;
  wire                when_RegStackPlugin_l148_37;
  wire                when_RegStackPlugin_l150_37;
  wire                when_RegStackPlugin_l152_37;
  wire                when_RegStackPlugin_l154_37;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_68;
  wire                when_RegStackPlugin_l144_38;
  wire                when_RegStackPlugin_l146_38;
  wire                when_RegStackPlugin_l148_38;
  wire                when_RegStackPlugin_l150_38;
  wire                when_RegStackPlugin_l152_38;
  wire                when_RegStackPlugin_l154_38;
  wire                when_ControlFlowPlugin_l171;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_69;
  wire                when_RegStackPlugin_l144_39;
  wire                when_RegStackPlugin_l146_39;
  wire                when_RegStackPlugin_l148_39;
  wire                when_RegStackPlugin_l150_39;
  wire                when_RegStackPlugin_l152_39;
  wire                when_RegStackPlugin_l154_39;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_70;
  wire                when_RegStackPlugin_l144_40;
  wire                when_RegStackPlugin_l146_40;
  wire                when_RegStackPlugin_l148_40;
  wire                when_RegStackPlugin_l150_40;
  wire                when_RegStackPlugin_l152_40;
  wire                when_RegStackPlugin_l154_40;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_71;
  wire                when_RegStackPlugin_l144_41;
  wire                when_RegStackPlugin_l146_41;
  wire                when_RegStackPlugin_l148_41;
  wire                when_RegStackPlugin_l150_41;
  wire                when_RegStackPlugin_l152_41;
  wire                when_RegStackPlugin_l154_41;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_72;
  wire                when_RegStackPlugin_l144_42;
  wire                when_RegStackPlugin_l146_42;
  wire                when_RegStackPlugin_l148_42;
  wire                when_RegStackPlugin_l150_42;
  wire                when_RegStackPlugin_l152_42;
  wire                when_RegStackPlugin_l154_42;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_73;
  wire                when_RegStackPlugin_l144_43;
  wire                when_RegStackPlugin_l146_43;
  wire                when_RegStackPlugin_l148_43;
  wire                when_RegStackPlugin_l150_43;
  wire                when_RegStackPlugin_l152_43;
  wire                when_RegStackPlugin_l154_43;
  reg        [2:0]    switch_BlockMovePlugin_l118;
  reg                 when_BlockMovePlugin_l165;
  reg        [2:0]    _zz_4;
  reg        [31:0]   _zz_when_BlockMovePlugin_l165;
  reg        [31:0]   _zz_when_BlockMovePlugin_l165_1;
  reg        [31:0]   _zz_when_BlockMovePlugin_l165_2;
  wire                _zz_when_BlockMovePlugin_l113_1;
  wire       [3:0]    _zz_when_BlockMovePlugin_l113_2;
  wire                when_BlockMovePlugin_l113;
  wire       [5:0]    switch_Misc_l245_9;
  reg        [31:0]   _zz_when_BlockMovePlugin_l165_3;
  wire       [5:0]    switch_Misc_l245_10;
  reg        [31:0]   _zz_when_BlockMovePlugin_l165_4;
  wire       [5:0]    switch_Misc_l245_11;
  reg        [31:0]   _zz_when_BlockMovePlugin_l165_5;
  wire       [31:0]   _zz_when_BlockMovePlugin_l165_6;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_74;
  wire                when_RegStackPlugin_l144_44;
  wire                when_RegStackPlugin_l146_44;
  wire                when_RegStackPlugin_l148_44;
  wire                when_RegStackPlugin_l150_44;
  wire                when_RegStackPlugin_l152_44;
  wire                when_RegStackPlugin_l154_44;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_75;
  wire                when_RegStackPlugin_l144_45;
  wire                when_RegStackPlugin_l146_45;
  wire                when_RegStackPlugin_l148_45;
  wire                when_RegStackPlugin_l150_45;
  wire                when_RegStackPlugin_l152_45;
  wire                when_RegStackPlugin_l154_45;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_76;
  wire                when_RegStackPlugin_l144_46;
  wire                when_RegStackPlugin_l146_46;
  wire                when_RegStackPlugin_l148_46;
  wire                when_RegStackPlugin_l150_46;
  wire                when_RegStackPlugin_l152_46;
  wire                when_RegStackPlugin_l154_46;
  wire                when_BlockMovePlugin_l171;
  wire                when_BlockMovePlugin_l187;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_77;
  wire                when_IndexingPlugin_l135;
  reg        [4:0]    switch_IndexingPlugin_l155;
  wire                _zz_when_IndexingPlugin_l135_1;
  wire                _zz_when_IndexingPlugin_l135_2;
  wire       [3:0]    _zz_when_IndexingPlugin_l135_3;
  wire       [3:0]    _zz_when_IndexingPlugin_l135_4;
  wire       [3:0]    _zz_fpu_fpPipe_ctrl_0_down_RESULT_78;
  wire       [5:0]    switch_Misc_l245_12;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_79;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_80;
  wire       [5:0]    switch_Misc_l245_13;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_81;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_82;
  wire       [5:0]    switch_Misc_l245_14;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_83;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_84;
  wire       [5:0]    switch_Misc_l245_15;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_85;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_86;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_87;
  wire                when_RegStackPlugin_l144_47;
  wire                when_RegStackPlugin_l146_47;
  wire                when_RegStackPlugin_l148_47;
  wire                when_RegStackPlugin_l150_47;
  wire                when_RegStackPlugin_l152_47;
  wire                when_RegStackPlugin_l154_47;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_88;
  wire                when_RegStackPlugin_l144_48;
  wire                when_RegStackPlugin_l146_48;
  wire                when_RegStackPlugin_l148_48;
  wire                when_RegStackPlugin_l150_48;
  wire                when_RegStackPlugin_l152_48;
  wire                when_RegStackPlugin_l154_48;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_89;
  wire                when_RegStackPlugin_l144_49;
  wire                when_RegStackPlugin_l146_49;
  wire                when_RegStackPlugin_l148_49;
  wire                when_RegStackPlugin_l150_49;
  wire                when_RegStackPlugin_l152_49;
  wire                when_RegStackPlugin_l154_49;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_90;
  wire                when_RegStackPlugin_l144_50;
  wire                when_RegStackPlugin_l146_50;
  wire                when_RegStackPlugin_l148_50;
  wire                when_RegStackPlugin_l150_50;
  wire                when_RegStackPlugin_l152_50;
  wire                when_RegStackPlugin_l154_50;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_91;
  wire                when_RegStackPlugin_l144_51;
  wire                when_RegStackPlugin_l146_51;
  wire                when_RegStackPlugin_l148_51;
  wire                when_RegStackPlugin_l150_51;
  wire                when_RegStackPlugin_l152_51;
  wire                when_RegStackPlugin_l154_51;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_92;
  wire                when_RegStackPlugin_l144_52;
  wire                when_RegStackPlugin_l146_52;
  wire                when_RegStackPlugin_l148_52;
  wire                when_RegStackPlugin_l150_52;
  wire                when_RegStackPlugin_l152_52;
  wire                when_RegStackPlugin_l154_52;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_93;
  wire                when_RegStackPlugin_l144_53;
  wire                when_RegStackPlugin_l146_53;
  wire                when_RegStackPlugin_l148_53;
  wire                when_RegStackPlugin_l150_53;
  wire                when_RegStackPlugin_l152_53;
  wire                when_RegStackPlugin_l154_53;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_94;
  wire                when_RegStackPlugin_l144_54;
  wire                when_RegStackPlugin_l146_54;
  wire                when_RegStackPlugin_l148_54;
  wire                when_RegStackPlugin_l150_54;
  wire                when_RegStackPlugin_l152_54;
  wire                when_RegStackPlugin_l154_54;
  wire                when_RangeCheckPlugin_l124;
  reg        [3:0]    switch_RangeCheckPlugin_l137;
  wire                _zz_when_RangeCheckPlugin_l124_1;
  wire                _zz_when_RangeCheckPlugin_l124_2;
  wire       [3:0]    _zz_when_RangeCheckPlugin_l124_3;
  wire       [3:0]    _zz_when_RangeCheckPlugin_l124_4;
  wire       [5:0]    switch_Misc_l245_16;
  reg        [31:0]   _zz_when_RangeCheckPlugin_l191;
  wire       [31:0]   _zz_when_RangeCheckPlugin_l191_1;
  wire       [5:0]    switch_Misc_l245_17;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_95;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_96;
  wire       [5:0]    switch_Misc_l245_18;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_97;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_98;
  wire                when_RegStackPlugin_l144_55;
  wire                when_RegStackPlugin_l146_55;
  wire                when_RegStackPlugin_l148_55;
  wire                when_RegStackPlugin_l150_55;
  wire                when_RegStackPlugin_l152_55;
  wire                when_RegStackPlugin_l154_55;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_99;
  wire                when_RegStackPlugin_l144_56;
  wire                when_RegStackPlugin_l146_56;
  wire                when_RegStackPlugin_l148_56;
  wire                when_RegStackPlugin_l150_56;
  wire                when_RegStackPlugin_l152_56;
  wire                when_RegStackPlugin_l154_56;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_100;
  wire                when_RegStackPlugin_l144_57;
  wire                when_RegStackPlugin_l146_57;
  wire                when_RegStackPlugin_l148_57;
  wire                when_RegStackPlugin_l150_57;
  wire                when_RegStackPlugin_l152_57;
  wire                when_RegStackPlugin_l154_57;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_101;
  wire                when_RegStackPlugin_l144_58;
  wire                when_RegStackPlugin_l146_58;
  wire                when_RegStackPlugin_l148_58;
  wire                when_RegStackPlugin_l150_58;
  wire                when_RegStackPlugin_l152_58;
  wire                when_RegStackPlugin_l154_58;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_102;
  wire                when_RegStackPlugin_l144_59;
  wire                when_RegStackPlugin_l146_59;
  wire                when_RegStackPlugin_l148_59;
  wire                when_RegStackPlugin_l150_59;
  wire                when_RegStackPlugin_l152_59;
  wire                when_RegStackPlugin_l154_59;
  wire                when_RangeCheckPlugin_l191;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_103;
  wire                when_RegStackPlugin_l144_60;
  wire                when_RegStackPlugin_l146_60;
  wire                when_RegStackPlugin_l148_60;
  wire                when_RegStackPlugin_l150_60;
  wire                when_RegStackPlugin_l152_60;
  wire                when_RegStackPlugin_l154_60;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_104;
  wire                when_RegStackPlugin_l144_61;
  wire                when_RegStackPlugin_l146_61;
  wire                when_RegStackPlugin_l148_61;
  wire                when_RegStackPlugin_l150_61;
  wire                when_RegStackPlugin_l152_61;
  wire                when_RegStackPlugin_l154_61;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_105;
  wire                when_RegStackPlugin_l144_62;
  wire                when_RegStackPlugin_l146_62;
  wire                when_RegStackPlugin_l148_62;
  wire                when_RegStackPlugin_l150_62;
  wire                when_RegStackPlugin_l152_62;
  wire                when_RegStackPlugin_l154_62;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_106;
  wire                when_RegStackPlugin_l144_63;
  wire                when_RegStackPlugin_l146_63;
  wire                when_RegStackPlugin_l148_63;
  wire                when_RegStackPlugin_l150_63;
  wire                when_RegStackPlugin_l152_63;
  wire                when_RegStackPlugin_l154_63;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_107;
  wire                when_RegStackPlugin_l144_64;
  wire                when_RegStackPlugin_l146_64;
  wire                when_RegStackPlugin_l148_64;
  wire                when_RegStackPlugin_l150_64;
  wire                when_RegStackPlugin_l152_64;
  wire                when_RegStackPlugin_l154_64;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_108;
  wire                when_RegStackPlugin_l144_65;
  wire                when_RegStackPlugin_l146_65;
  wire                when_RegStackPlugin_l148_65;
  wire                when_RegStackPlugin_l150_65;
  wire                when_RegStackPlugin_l152_65;
  wire                when_RegStackPlugin_l154_65;
  wire                when_GeneralPlugin_l69;
  wire       [3:0]    _zz_when_GeneralPlugin_l69_1;
  wire       [5:0]    switch_Misc_l245_19;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_109;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_110;
  wire       [5:0]    switch_Misc_l245_20;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_111;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_112;
  wire       [5:0]    switch_Misc_l245_21;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_113;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_114;
  wire                when_RegStackPlugin_l144_66;
  wire                when_RegStackPlugin_l146_66;
  wire                when_RegStackPlugin_l148_66;
  wire                when_RegStackPlugin_l150_66;
  wire                when_RegStackPlugin_l152_66;
  wire                when_RegStackPlugin_l154_66;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_115;
  wire                when_RegStackPlugin_l144_67;
  wire                when_RegStackPlugin_l146_67;
  wire                when_RegStackPlugin_l148_67;
  wire                when_RegStackPlugin_l150_67;
  wire                when_RegStackPlugin_l152_67;
  wire                when_RegStackPlugin_l154_67;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_116;
  wire                when_RegStackPlugin_l144_68;
  wire                when_RegStackPlugin_l146_68;
  wire                when_RegStackPlugin_l148_68;
  wire                when_RegStackPlugin_l150_68;
  wire                when_RegStackPlugin_l152_68;
  wire                when_RegStackPlugin_l154_68;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_117;
  wire                when_RegStackPlugin_l144_69;
  wire                when_RegStackPlugin_l146_69;
  wire                when_RegStackPlugin_l148_69;
  wire                when_RegStackPlugin_l150_69;
  wire                when_RegStackPlugin_l152_69;
  wire                when_RegStackPlugin_l154_69;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_118;
  wire                when_RegStackPlugin_l144_70;
  wire                when_RegStackPlugin_l146_70;
  wire                when_RegStackPlugin_l148_70;
  wire                when_RegStackPlugin_l150_70;
  wire                when_RegStackPlugin_l152_70;
  wire                when_RegStackPlugin_l154_70;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_119;
  wire                when_RegStackPlugin_l144_71;
  wire                when_RegStackPlugin_l146_71;
  wire                when_RegStackPlugin_l148_71;
  wire                when_RegStackPlugin_l150_71;
  wire                when_RegStackPlugin_l152_71;
  wire                when_RegStackPlugin_l154_71;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_120;
  wire                when_RegStackPlugin_l144_72;
  wire                when_RegStackPlugin_l146_72;
  wire                when_RegStackPlugin_l148_72;
  wire                when_RegStackPlugin_l150_72;
  wire                when_RegStackPlugin_l152_72;
  wire                when_RegStackPlugin_l154_72;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_121;
  wire                when_RegStackPlugin_l144_73;
  wire                when_RegStackPlugin_l146_73;
  wire                when_RegStackPlugin_l148_73;
  wire                when_RegStackPlugin_l150_73;
  wire                when_RegStackPlugin_l152_73;
  wire                when_RegStackPlugin_l154_73;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_122;
  wire                when_RegStackPlugin_l144_74;
  wire                when_RegStackPlugin_l146_74;
  wire                when_RegStackPlugin_l148_74;
  wire                when_RegStackPlugin_l150_74;
  wire                when_RegStackPlugin_l152_74;
  wire                when_RegStackPlugin_l154_74;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_123;
  wire                when_RegStackPlugin_l144_75;
  wire                when_RegStackPlugin_l146_75;
  wire                when_RegStackPlugin_l148_75;
  wire                when_RegStackPlugin_l150_75;
  wire                when_RegStackPlugin_l152_75;
  wire                when_RegStackPlugin_l154_75;
  reg        [1:0]    _zz_fpu_fpPipe_ctrl_0_down_RESULT_124;
  wire                when_ChannelPlugin_l111;
  reg        [3:0]    switch_ChannelPlugin_l121;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_125;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_126;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_127;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_128;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_129;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_130;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_131;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_132;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_133;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_134;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_135;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_136;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_137;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_138;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_139;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_140;
  reg                 _zz_when_ChannelPlugin_l129;
  reg                 _zz_when_ChannelPlugin_l129_1;
  reg                 _zz_when_ChannelPlugin_l129_2;
  reg                 _zz_when_ChannelPlugin_l129_3;
  reg                 _zz_when_ChannelPlugin_l129_4;
  reg                 _zz_when_ChannelPlugin_l129_5;
  reg                 _zz_when_ChannelPlugin_l129_6;
  reg                 _zz_when_ChannelPlugin_l129_7;
  reg                 _zz_when_ChannelPlugin_l129_8;
  reg                 _zz_when_ChannelPlugin_l129_9;
  reg                 _zz_when_ChannelPlugin_l129_10;
  reg                 _zz_when_ChannelPlugin_l129_11;
  reg                 _zz_when_ChannelPlugin_l129_12;
  reg                 _zz_when_ChannelPlugin_l129_13;
  reg                 _zz_when_ChannelPlugin_l129_14;
  reg                 _zz_when_ChannelPlugin_l129_15;
  reg        [15:0]   _zz_when_ChannelPlugin_l127;
  wire                _zz_when_ChannelPlugin_l111_1;
  wire       [3:0]    _zz_when_ChannelPlugin_l111_2;
  wire                when_ChannelPlugin_l102;
  wire       [5:0]    switch_Misc_l245_22;
  reg        [31:0]   _zz_when_ChannelPlugin_l127_1;
  wire       [31:0]   _zz_when_ChannelPlugin_l127_2;
  wire       [5:0]    switch_Misc_l245_23;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_141;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_142;
  wire       [5:0]    switch_Misc_l245_24;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_143;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_144;
  wire       [3:0]    _zz_when_ChannelPlugin_l127_3;
  wire                when_ChannelPlugin_l127;
  wire       [15:0]   _zz_5;
  wire                when_ChannelPlugin_l129;
  wire                when_ChannelPlugin_l131;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_145;
  wire                when_RegStackPlugin_l144_76;
  wire                when_RegStackPlugin_l146_76;
  wire                when_RegStackPlugin_l148_76;
  wire                when_RegStackPlugin_l150_76;
  wire                when_RegStackPlugin_l152_76;
  wire                when_RegStackPlugin_l154_76;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_146;
  wire                when_RegStackPlugin_l144_77;
  wire                when_RegStackPlugin_l146_77;
  wire                when_RegStackPlugin_l148_77;
  wire                when_RegStackPlugin_l150_77;
  wire                when_RegStackPlugin_l152_77;
  wire                when_RegStackPlugin_l154_77;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_147;
  wire                when_RegStackPlugin_l144_78;
  wire                when_RegStackPlugin_l146_78;
  wire                when_RegStackPlugin_l148_78;
  wire                when_RegStackPlugin_l150_78;
  wire                when_RegStackPlugin_l152_78;
  wire                when_RegStackPlugin_l154_78;
  wire                when_ChannelPlugin_l154;
  wire                _zz_when_ChannelPlugin_l129_16;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_148;
  wire                when_RegStackPlugin_l144_79;
  wire                when_RegStackPlugin_l146_79;
  wire                when_RegStackPlugin_l148_79;
  wire                when_RegStackPlugin_l150_79;
  wire                when_RegStackPlugin_l152_79;
  wire                when_RegStackPlugin_l154_79;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_149;
  wire                when_RegStackPlugin_l144_80;
  wire                when_RegStackPlugin_l146_80;
  wire                when_RegStackPlugin_l148_80;
  wire                when_RegStackPlugin_l150_80;
  wire                when_RegStackPlugin_l152_80;
  wire                when_RegStackPlugin_l154_80;
  wire                when_ChannelPlugin_l167;
  wire       [15:0]   _zz_6;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_150;
  wire                when_RegStackPlugin_l144_81;
  wire                when_RegStackPlugin_l146_81;
  wire                when_RegStackPlugin_l148_81;
  wire                when_RegStackPlugin_l150_81;
  wire                when_RegStackPlugin_l152_81;
  wire                when_RegStackPlugin_l154_81;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_151;
  wire                when_RegStackPlugin_l144_82;
  wire                when_RegStackPlugin_l146_82;
  wire                when_RegStackPlugin_l148_82;
  wire                when_RegStackPlugin_l150_82;
  wire                when_RegStackPlugin_l152_82;
  wire                when_RegStackPlugin_l154_82;
  wire                when_ChannelPlugin_l177;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_152;
  wire                when_RegStackPlugin_l144_83;
  wire                when_RegStackPlugin_l146_83;
  wire                when_RegStackPlugin_l148_83;
  wire                when_RegStackPlugin_l150_83;
  wire                when_RegStackPlugin_l152_83;
  wire                when_RegStackPlugin_l154_83;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_153;
  wire                when_RegStackPlugin_l144_84;
  wire                when_RegStackPlugin_l146_84;
  wire                when_RegStackPlugin_l148_84;
  wire                when_RegStackPlugin_l150_84;
  wire                when_RegStackPlugin_l152_84;
  wire                when_RegStackPlugin_l154_84;
  wire                when_ChannelPlugin_l190;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_154;
  wire                when_RegStackPlugin_l144_85;
  wire                when_RegStackPlugin_l146_85;
  wire                when_RegStackPlugin_l148_85;
  wire                when_RegStackPlugin_l150_85;
  wire                when_RegStackPlugin_l152_85;
  wire                when_RegStackPlugin_l154_85;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_155;
  wire                when_RegStackPlugin_l144_86;
  wire                when_RegStackPlugin_l146_86;
  wire                when_RegStackPlugin_l148_86;
  wire                when_RegStackPlugin_l150_86;
  wire                when_RegStackPlugin_l152_86;
  wire                when_RegStackPlugin_l154_86;
  wire                when_ChannelPlugin_l200;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_156;
  wire                when_RegStackPlugin_l144_87;
  wire                when_RegStackPlugin_l146_87;
  wire                when_RegStackPlugin_l148_87;
  wire                when_RegStackPlugin_l150_87;
  wire                when_RegStackPlugin_l152_87;
  wire                when_RegStackPlugin_l154_87;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_157;
  wire                when_RegStackPlugin_l144_88;
  wire                when_RegStackPlugin_l146_88;
  wire                when_RegStackPlugin_l148_88;
  wire                when_RegStackPlugin_l150_88;
  wire                when_RegStackPlugin_l152_88;
  wire                when_RegStackPlugin_l154_88;
  wire                when_ChannelPlugin_l222;
  wire                when_InterruptPlugin_l118;
  reg        [2:0]    switch_InterruptPlugin_l123;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_158;
  wire                _zz_when_InterruptPlugin_l118_1;
  wire       [3:0]    _zz_when_InterruptPlugin_l118_2;
  wire       [5:0]    switch_Misc_l245_25;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_159;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_160;
  wire       [5:0]    switch_Misc_l245_26;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_161;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_162;
  wire       [5:0]    switch_Misc_l245_27;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_163;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_164;
  wire                when_RegStackPlugin_l144_89;
  wire                when_RegStackPlugin_l146_89;
  wire                when_RegStackPlugin_l148_89;
  wire                when_RegStackPlugin_l150_89;
  wire                when_RegStackPlugin_l152_89;
  wire                when_RegStackPlugin_l154_89;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_165;
  wire                when_RegStackPlugin_l144_90;
  wire                when_RegStackPlugin_l146_90;
  wire                when_RegStackPlugin_l148_90;
  wire                when_RegStackPlugin_l150_90;
  wire                when_RegStackPlugin_l152_90;
  wire                when_RegStackPlugin_l154_90;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_166;
  wire                when_RegStackPlugin_l144_91;
  wire                when_RegStackPlugin_l146_91;
  wire                when_RegStackPlugin_l148_91;
  wire                when_RegStackPlugin_l150_91;
  wire                when_RegStackPlugin_l152_91;
  wire                when_RegStackPlugin_l154_91;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_167;
  wire                when_RegStackPlugin_l144_92;
  wire                when_RegStackPlugin_l146_92;
  wire                when_RegStackPlugin_l148_92;
  wire                when_RegStackPlugin_l150_92;
  wire                when_RegStackPlugin_l152_92;
  wire                when_RegStackPlugin_l154_92;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_168;
  wire                when_RegStackPlugin_l144_93;
  wire                when_RegStackPlugin_l146_93;
  wire                when_RegStackPlugin_l148_93;
  wire                when_RegStackPlugin_l150_93;
  wire                when_RegStackPlugin_l152_93;
  wire                when_RegStackPlugin_l154_93;
  wire                when_ResourcePlugin_l114;
  reg        [3:0]    switch_ResourcePlugin_l119;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_169;
  wire                _zz_when_ResourcePlugin_l114_1;
  wire       [3:0]    _zz_when_ResourcePlugin_l114_2;
  wire       [5:0]    switch_Misc_l245_28;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_170;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_171;
  wire       [5:0]    switch_Misc_l245_29;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_172;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_173;
  wire       [5:0]    switch_Misc_l245_30;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_174;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_175;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_176;
  wire                when_RegStackPlugin_l144_94;
  wire                when_RegStackPlugin_l146_94;
  wire                when_RegStackPlugin_l148_94;
  wire                when_RegStackPlugin_l150_94;
  wire                when_RegStackPlugin_l152_94;
  wire                when_RegStackPlugin_l154_94;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_177;
  wire                when_RegStackPlugin_l144_95;
  wire                when_RegStackPlugin_l146_95;
  wire                when_RegStackPlugin_l148_95;
  wire                when_RegStackPlugin_l150_95;
  wire                when_RegStackPlugin_l152_95;
  wire                when_RegStackPlugin_l154_95;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_178;
  wire                when_RegStackPlugin_l144_96;
  wire                when_RegStackPlugin_l146_96;
  wire                when_RegStackPlugin_l148_96;
  wire                when_RegStackPlugin_l150_96;
  wire                when_RegStackPlugin_l152_96;
  wire                when_RegStackPlugin_l154_96;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_179;
  wire                when_RegStackPlugin_l144_97;
  wire                when_RegStackPlugin_l146_97;
  wire                when_RegStackPlugin_l148_97;
  wire                when_RegStackPlugin_l150_97;
  wire                when_RegStackPlugin_l152_97;
  wire                when_RegStackPlugin_l154_97;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_180;
  wire                when_RegStackPlugin_l144_98;
  wire                when_RegStackPlugin_l146_98;
  wire                when_RegStackPlugin_l148_98;
  wire                when_RegStackPlugin_l150_98;
  wire                when_RegStackPlugin_l152_98;
  wire                when_RegStackPlugin_l154_98;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_181;
  wire                when_RegStackPlugin_l144_99;
  wire                when_RegStackPlugin_l146_99;
  wire                when_RegStackPlugin_l148_99;
  wire                when_RegStackPlugin_l150_99;
  wire                when_RegStackPlugin_l152_99;
  wire                when_RegStackPlugin_l154_99;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_182;
  wire                when_RegStackPlugin_l144_100;
  wire                when_RegStackPlugin_l146_100;
  wire                when_RegStackPlugin_l148_100;
  wire                when_RegStackPlugin_l150_100;
  wire                when_RegStackPlugin_l152_100;
  wire                when_RegStackPlugin_l154_100;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_183;
  wire                when_RegStackPlugin_l144_101;
  wire                when_RegStackPlugin_l146_101;
  wire                when_RegStackPlugin_l148_101;
  wire                when_RegStackPlugin_l150_101;
  wire                when_RegStackPlugin_l152_101;
  wire                when_RegStackPlugin_l154_101;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_184;
  wire                when_RegStackPlugin_l144_102;
  wire                when_RegStackPlugin_l146_102;
  wire                when_RegStackPlugin_l148_102;
  wire                when_RegStackPlugin_l150_102;
  wire                when_RegStackPlugin_l152_102;
  wire                when_RegStackPlugin_l154_102;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_185;
  wire                when_RegStackPlugin_l144_103;
  wire                when_RegStackPlugin_l146_103;
  wire                when_RegStackPlugin_l148_103;
  wire                when_RegStackPlugin_l150_103;
  wire                when_RegStackPlugin_l152_103;
  wire                when_RegStackPlugin_l154_103;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_186;
  wire                when_RegStackPlugin_l144_104;
  wire                when_RegStackPlugin_l146_104;
  wire                when_RegStackPlugin_l148_104;
  wire                when_RegStackPlugin_l150_104;
  wire                when_RegStackPlugin_l152_104;
  wire                when_RegStackPlugin_l154_104;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_187;
  wire                when_RegStackPlugin_l144_105;
  wire                when_RegStackPlugin_l146_105;
  wire                when_RegStackPlugin_l148_105;
  wire                when_RegStackPlugin_l150_105;
  wire                when_RegStackPlugin_l152_105;
  wire                when_RegStackPlugin_l154_105;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_188;
  wire                when_RegStackPlugin_l144_106;
  wire                when_RegStackPlugin_l146_106;
  wire                when_RegStackPlugin_l148_106;
  wire                when_RegStackPlugin_l150_106;
  wire                when_RegStackPlugin_l152_106;
  wire                when_RegStackPlugin_l154_106;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_189;
  wire                when_SystemPlugin_l123;
  reg        [2:0]    switch_SystemPlugin_l128;
  reg        [7:0]    _zz_fpu_fpPipe_ctrl_0_down_RESULT_190;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_191;
  reg        [15:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_192;
  reg        [15:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_193;
  reg        [15:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_194;
  wire                _zz_when_SystemPlugin_l123_1;
  wire       [3:0]    _zz_when_SystemPlugin_l123_2;
  wire       [5:0]    switch_Misc_l245_31;
  reg        [31:0]   _zz_switch_SystemPlugin_l145;
  wire       [31:0]   _zz_switch_SystemPlugin_l145_1;
  wire       [5:0]    switch_Misc_l245_32;
  reg        [31:0]   _zz_switch_SystemPlugin_l163;
  wire       [31:0]   _zz_switch_SystemPlugin_l163_1;
  wire       [5:0]    switch_Misc_l245_33;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_195;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_196;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_197;
  wire                when_RegStackPlugin_l144_107;
  wire                when_RegStackPlugin_l146_107;
  wire                when_RegStackPlugin_l148_107;
  wire                when_RegStackPlugin_l150_107;
  wire                when_RegStackPlugin_l152_107;
  wire                when_RegStackPlugin_l154_107;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_198;
  wire                when_RegStackPlugin_l144_108;
  wire                when_RegStackPlugin_l146_108;
  wire                when_RegStackPlugin_l148_108;
  wire                when_RegStackPlugin_l150_108;
  wire                when_RegStackPlugin_l152_108;
  wire                when_RegStackPlugin_l154_108;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_199;
  wire                when_RegStackPlugin_l144_109;
  wire                when_RegStackPlugin_l146_109;
  wire                when_RegStackPlugin_l148_109;
  wire                when_RegStackPlugin_l150_109;
  wire                when_RegStackPlugin_l152_109;
  wire                when_RegStackPlugin_l154_109;
  wire       [3:0]    switch_SystemPlugin_l145;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_200;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_201;
  wire                when_RegStackPlugin_l144_110;
  wire                when_RegStackPlugin_l146_110;
  wire                when_RegStackPlugin_l148_110;
  wire                when_RegStackPlugin_l150_110;
  wire                when_RegStackPlugin_l152_110;
  wire                when_RegStackPlugin_l154_110;
  wire       [3:0]    switch_SystemPlugin_l163;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_202;
  wire                when_RegStackPlugin_l144_111;
  wire                when_RegStackPlugin_l146_111;
  wire                when_RegStackPlugin_l148_111;
  wire                when_RegStackPlugin_l150_111;
  wire                when_RegStackPlugin_l152_111;
  wire                when_RegStackPlugin_l154_111;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_203;
  wire                when_RegStackPlugin_l144_112;
  wire                when_RegStackPlugin_l146_112;
  wire                when_RegStackPlugin_l148_112;
  wire                when_RegStackPlugin_l150_112;
  wire                when_RegStackPlugin_l152_112;
  wire                when_RegStackPlugin_l154_112;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_204;
  wire                when_RegStackPlugin_l144_113;
  wire                when_RegStackPlugin_l146_113;
  wire                when_RegStackPlugin_l148_113;
  wire                when_RegStackPlugin_l150_113;
  wire                when_RegStackPlugin_l152_113;
  wire                when_RegStackPlugin_l154_113;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_205;
  wire                when_RegStackPlugin_l144_114;
  wire                when_RegStackPlugin_l146_114;
  wire                when_RegStackPlugin_l148_114;
  wire                when_RegStackPlugin_l150_114;
  wire                when_RegStackPlugin_l152_114;
  wire                when_RegStackPlugin_l154_114;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_206;
  wire                when_RegStackPlugin_l144_115;
  wire                when_RegStackPlugin_l146_115;
  wire                when_RegStackPlugin_l148_115;
  wire                when_RegStackPlugin_l150_115;
  wire                when_RegStackPlugin_l152_115;
  wire                when_RegStackPlugin_l154_115;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_207;
  wire                when_RegStackPlugin_l144_116;
  wire                when_RegStackPlugin_l146_116;
  wire                when_RegStackPlugin_l148_116;
  wire                when_RegStackPlugin_l150_116;
  wire                when_RegStackPlugin_l152_116;
  wire                when_RegStackPlugin_l154_116;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_208;
  wire                when_RegStackPlugin_l144_117;
  wire                when_RegStackPlugin_l146_117;
  wire                when_RegStackPlugin_l148_117;
  wire                when_RegStackPlugin_l150_117;
  wire                when_RegStackPlugin_l152_117;
  wire                when_RegStackPlugin_l154_117;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_209;
  wire                when_RegStackPlugin_l144_118;
  wire                when_RegStackPlugin_l146_118;
  wire                when_RegStackPlugin_l148_118;
  wire                when_RegStackPlugin_l150_118;
  wire                when_RegStackPlugin_l152_118;
  wire                when_RegStackPlugin_l154_118;
  wire       [3:0]    switch_SystemPlugin_l217;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_210;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_211;
  wire                when_RegStackPlugin_l144_119;
  wire                when_RegStackPlugin_l146_119;
  wire                when_RegStackPlugin_l148_119;
  wire                when_RegStackPlugin_l150_119;
  wire                when_RegStackPlugin_l152_119;
  wire                when_RegStackPlugin_l154_119;
  reg        [63:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_212;
  reg        [63:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_213;
  reg        [63:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_214;
  reg        [1:0]    _zz_switch_Misc_l245_1;
  reg        [9:0]    _zz_fpu_fpPipe_ctrl_0_down_CYCLE_CNT;
  wire       [55:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_AFIX;
  wire       [7:0]    _zz_7;
  reg        [9:0]    _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES;
  wire                fpu_fpPipe_ctrl_0_haltRequest_FpuPlugin_l222;
  wire       [63:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_215;
  wire       [63:0]   _zz_io_cmd_payload_b_1;
  wire       [1:0]    switch_Misc_l245_34;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_216;
  wire       [63:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_217;
  wire       [5:0]    switch_Misc_l245_35;
  reg        [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_218;
  wire       [31:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_219;
  wire                _zz_fpu_fpPipe_ctrl_0_down_RESULT_220;
  wire       [51:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_221;
  wire                _zz_fpu_fpPipe_ctrl_0_down_RESULT_222;
  wire                _zz_fpu_fpPipe_ctrl_0_down_RESULT_223;
  wire                _zz_fpu_fpPipe_ctrl_0_down_RESULT_224;
  reg                 _zz_fpu_fpPipe_ctrl_0_down_RESULT_225;
  wire                _zz_fpu_fpPipe_ctrl_0_down_RESULT_226;
  wire       [10:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_227;
  wire                when_FpuPlugin_l421;
  wire       [10:0]   _zz_fpu_fpPipe_ctrl_0_down_RESULT_228;
  wire                when_FpuPlugin_l434;
  wire                when_FpuPlugin_l492;
  wire                when_FpuPlugin_l512;
  wire                when_CtrlLink_l150;
  reg        [31:0]   _zz_when_SchedulerPlugin_l170;
  reg        [31:0]   _zz_when_SchedulerPlugin_l170_1;
  reg                 _zz_when_SchedulerPlugin_l131;
  reg                 _zz_when_SchedulerPlugin_l131_1;
  reg                 _zz_when_SchedulerPlugin_l131_2;
  reg        [1:0]    _zz_when_SchedulerPlugin_l168;
  wire                io_push_fire;
  wire                io_push_fire_1;
  wire                when_SchedulerPlugin_l131;
  wire                when_SchedulerPlugin_l163;
  wire                when_SchedulerPlugin_l168;
  wire                when_SchedulerPlugin_l170;
  `ifndef SYNTHESIS
  reg [39:0] switch_ArithmeticPlugin_l160_string;
  reg [95:0] switch_Misc_l245_string;
  reg [95:0] switch_Misc_l245_1_string;
  reg [95:0] switch_Misc_l245_2_string;
  reg [79:0] switch_LongArithPlugin_l132_string;
  reg [95:0] switch_Misc_l245_3_string;
  reg [95:0] switch_Misc_l245_4_string;
  reg [95:0] switch_Misc_l245_5_string;
  reg [79:0] switch_ControlFlowPlugin_l132_string;
  reg [95:0] switch_Misc_l245_6_string;
  reg [95:0] switch_Misc_l245_7_string;
  reg [95:0] switch_Misc_l245_8_string;
  reg [103:0] switch_BlockMovePlugin_l118_string;
  reg [95:0] switch_Misc_l245_9_string;
  reg [95:0] switch_Misc_l245_10_string;
  reg [95:0] switch_Misc_l245_11_string;
  reg [39:0] switch_IndexingPlugin_l155_string;
  reg [95:0] switch_Misc_l245_12_string;
  reg [95:0] switch_Misc_l245_13_string;
  reg [95:0] switch_Misc_l245_14_string;
  reg [95:0] switch_Misc_l245_15_string;
  reg [47:0] switch_RangeCheckPlugin_l137_string;
  reg [95:0] switch_Misc_l245_16_string;
  reg [95:0] switch_Misc_l245_17_string;
  reg [95:0] switch_Misc_l245_18_string;
  reg [95:0] switch_Misc_l245_19_string;
  reg [95:0] switch_Misc_l245_20_string;
  reg [95:0] switch_Misc_l245_21_string;
  reg [71:0] _zz_fpu_fpPipe_ctrl_0_down_RESULT_124_string;
  reg [71:0] switch_ChannelPlugin_l121_string;
  reg [95:0] switch_Misc_l245_22_string;
  reg [95:0] switch_Misc_l245_23_string;
  reg [95:0] switch_Misc_l245_24_string;
  reg [79:0] switch_InterruptPlugin_l123_string;
  reg [95:0] switch_Misc_l245_25_string;
  reg [95:0] switch_Misc_l245_26_string;
  reg [95:0] switch_Misc_l245_27_string;
  reg [63:0] switch_ResourcePlugin_l119_string;
  reg [95:0] switch_Misc_l245_28_string;
  reg [95:0] switch_Misc_l245_29_string;
  reg [95:0] switch_Misc_l245_30_string;
  reg [103:0] switch_SystemPlugin_l128_string;
  reg [95:0] switch_Misc_l245_31_string;
  reg [95:0] switch_Misc_l245_32_string;
  reg [95:0] switch_Misc_l245_33_string;
  reg [95:0] switch_Misc_l245_35_string;
  reg [79:0] _zz_when_SchedulerPlugin_l168_string;
  `endif


  assign _zz_when_20 = 1'b0;
  assign _zz_9 = SecondaryOpcode_ADD;
  assign _zz_8 = _zz_9[3:0];
  assign _zz_11 = SecondaryOpcode_SUB;
  assign _zz_10 = _zz_11[3:0];
  assign _zz_13 = SecondaryOpcode_AND_1;
  assign _zz_12 = _zz_13[3:0];
  assign _zz_15 = SecondaryOpcode_OR_1;
  assign _zz_14 = _zz_15[3:0];
  assign _zz_17 = SecondaryOpcode_XOR_1;
  assign _zz_16 = _zz_17[3:0];
  assign _zz_19 = SecondaryOpcode_NOT_1;
  assign _zz_18 = _zz_19[3:0];
  assign _zz_21 = SecondaryOpcode_SHL;
  assign _zz_20 = _zz_21[3:0];
  assign _zz_23 = SecondaryOpcode_SHR;
  assign _zz_22 = _zz_23[3:0];
  assign _zz_25 = SecondaryOpcode_GT;
  assign _zz_24 = _zz_25[3:0];
  assign _zz_27 = SecondaryOpcode_REV;
  assign _zz_26 = _zz_27[3:0];
  assign _zz_when_1 = SecondaryOpcode_ADD;
  assign _zz_when = _zz_when_1[3:0];
  assign _zz_when_3 = SecondaryOpcode_SUB;
  assign _zz_when_2 = _zz_when_3[3:0];
  assign _zz_when_5 = SecondaryOpcode_AND_1;
  assign _zz_when_4 = _zz_when_5[3:0];
  assign _zz_when_7 = SecondaryOpcode_OR_1;
  assign _zz_when_6 = _zz_when_7[3:0];
  assign _zz_when_9 = SecondaryOpcode_XOR_1;
  assign _zz_when_8 = _zz_when_9[3:0];
  assign _zz_when_11 = SecondaryOpcode_NOT_1;
  assign _zz_when_10 = _zz_when_11[3:0];
  assign _zz_when_13 = SecondaryOpcode_SHL;
  assign _zz_when_12 = _zz_when_13[3:0];
  assign _zz_when_15 = SecondaryOpcode_SHR;
  assign _zz_when_14 = _zz_when_15[3:0];
  assign _zz_when_17 = SecondaryOpcode_GT;
  assign _zz_when_16 = _zz_when_17[3:0];
  assign _zz_when_19 = SecondaryOpcode_REV;
  assign _zz_when_18 = _zz_when_19[3:0];
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_15 = ($signed(_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_15_1) + $signed(_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_15_2));
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_15_1 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_12;
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_15_2 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_10;
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_17 = ($signed(_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_17_1) - $signed(_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_17_2));
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_17_1 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_12;
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_17_2 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_10;
  assign _zz_29 = SecondaryOpcode_LADD;
  assign _zz_28 = _zz_29[3:0];
  assign _zz_31 = SecondaryOpcode_LSUB;
  assign _zz_30 = _zz_31[3:0];
  assign _zz_33 = SecondaryOpcode_LMUL;
  assign _zz_32 = _zz_33[3:0];
  assign _zz_35 = SecondaryOpcode_LDIV;
  assign _zz_34 = _zz_35[3:0];
  assign _zz_37 = SecondaryOpcode_LSHL;
  assign _zz_36 = _zz_37[3:0];
  assign _zz_39 = SecondaryOpcode_LSHR;
  assign _zz_38 = _zz_39[3:0];
  assign _zz_41 = SecondaryOpcode_MINT;
  assign _zz_40 = _zz_41[3:0];
  assign _zz_43 = SecondaryOpcode_XSWORD;
  assign _zz_42 = _zz_43[3:0];
  assign _zz_45 = SecondaryOpcode_PROD;
  assign _zz_44 = _zz_45[3:0];
  assign _zz_when_LongArithPlugin_l142_4 = SecondaryOpcode_LADD;
  assign _zz_when_LongArithPlugin_l142_3 = _zz_when_LongArithPlugin_l142_4[3:0];
  assign _zz_when_LongArithPlugin_l142_6 = SecondaryOpcode_LSUB;
  assign _zz_when_LongArithPlugin_l142_5 = _zz_when_LongArithPlugin_l142_6[3:0];
  assign _zz_when_LongArithPlugin_l142_8 = SecondaryOpcode_LMUL;
  assign _zz_when_LongArithPlugin_l142_7 = _zz_when_LongArithPlugin_l142_8[3:0];
  assign _zz_when_LongArithPlugin_l142_10 = SecondaryOpcode_LDIV;
  assign _zz_when_LongArithPlugin_l142_9 = _zz_when_LongArithPlugin_l142_10[3:0];
  assign _zz_when_LongArithPlugin_l142_12 = SecondaryOpcode_LSHL;
  assign _zz_when_LongArithPlugin_l142_11 = _zz_when_LongArithPlugin_l142_12[3:0];
  assign _zz_when_LongArithPlugin_l142_14 = SecondaryOpcode_LSHR;
  assign _zz_when_LongArithPlugin_l142_13 = _zz_when_LongArithPlugin_l142_14[3:0];
  assign _zz_when_LongArithPlugin_l142_16 = SecondaryOpcode_MINT;
  assign _zz_when_LongArithPlugin_l142_15 = _zz_when_LongArithPlugin_l142_16[3:0];
  assign _zz_when_LongArithPlugin_l142_18 = SecondaryOpcode_XSWORD;
  assign _zz_when_LongArithPlugin_l142_17 = _zz_when_LongArithPlugin_l142_18[3:0];
  assign _zz_when_LongArithPlugin_l142_20 = SecondaryOpcode_PROD;
  assign _zz_when_LongArithPlugin_l142_19 = _zz_when_LongArithPlugin_l142_20[3:0];
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_32 = {1'd0, _zz_fpu_fpPipe_ctrl_0_down_RESULT_31};
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35 = {_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35_1,_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35_3};
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35_2 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_31;
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35_1 = {{1{_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35_2[31]}}, _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35_2};
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35_3 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_29;
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35_4 = {33'h0,_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35_5};
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35_5 = _zz_when_LongArithPlugin_l220_1;
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_36 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_35[31 : 0];
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_37 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_35[63 : 32];
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_38 = {32'd0, _zz_fpu_fpPipe_ctrl_0_down_RESULT_29};
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_38_1 = {32'd0, _zz_when_LongArithPlugin_l220_1};
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_42 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_41 / _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_42_1);
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_42_1 = {32'd0, _zz_when_LongArithPlugin_l220_1};
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_43 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_41 % _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_43_1);
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_43_1 = {32'd0, _zz_when_LongArithPlugin_l220_1};
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_50 = ({31'd0,_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_50_1} <<< 5'd31);
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_50_1 = (1'b0 - 1'b1);
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_53_1 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_29;
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_53 = {{32{_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_53_1[31]}}, _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_53_1};
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_57_1 = _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_57_2;
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_57 = _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_57_1[31:0];
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_57_2 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_29 * _zz_when_LongArithPlugin_l220_1);
  assign _zz_when_ControlFlowPlugin_l115_4 = SecondaryOpcode_RET;
  assign _zz_when_ControlFlowPlugin_l115_3 = _zz_when_ControlFlowPlugin_l115_4[3:0];
  assign _zz_when_ControlFlowPlugin_l115_6 = SecondaryOpcode_LDPI;
  assign _zz_when_ControlFlowPlugin_l115_5 = _zz_when_ControlFlowPlugin_l115_6[3:0];
  assign _zz_when_ControlFlowPlugin_l115_8 = SecondaryOpcode_GAJW;
  assign _zz_when_ControlFlowPlugin_l115_7 = _zz_when_ControlFlowPlugin_l115_8[3:0];
  assign _zz_when_ControlFlowPlugin_l115_10 = SecondaryOpcode_GCALL;
  assign _zz_when_ControlFlowPlugin_l115_9 = _zz_when_ControlFlowPlugin_l115_10[3:0];
  assign _zz_when_ControlFlowPlugin_l115_12 = SecondaryOpcode_LEND;
  assign _zz_when_ControlFlowPlugin_l115_11 = _zz_when_ControlFlowPlugin_l115_12[3:0];
  assign _zz_when_ControlFlowPlugin_l115_14 = SecondaryOpcode_ENDP;
  assign _zz_when_ControlFlowPlugin_l115_13 = _zz_when_ControlFlowPlugin_l115_14[3:0];
  assign _zz_when_ControlFlowPlugin_l115_16 = SecondaryOpcode_DISS;
  assign _zz_when_ControlFlowPlugin_l115_15 = _zz_when_ControlFlowPlugin_l115_16[3:0];
  assign _zz_47 = SecondaryOpcode_RET;
  assign _zz_46 = _zz_47[3:0];
  assign _zz_49 = SecondaryOpcode_LDPI;
  assign _zz_48 = _zz_49[3:0];
  assign _zz_51 = SecondaryOpcode_GAJW;
  assign _zz_50 = _zz_51[3:0];
  assign _zz_53 = SecondaryOpcode_GCALL;
  assign _zz_52 = _zz_53[3:0];
  assign _zz_55 = SecondaryOpcode_LEND;
  assign _zz_54 = _zz_55[3:0];
  assign _zz_57 = SecondaryOpcode_ENDP;
  assign _zz_56 = _zz_57[3:0];
  assign _zz_59 = SecondaryOpcode_DISS;
  assign _zz_58 = _zz_59[3:0];
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_64 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_2 + _zz_when_ControlFlowPlugin_l171_1);
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_67 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_2 + 32'h00000001);
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_69 = (_zz_when_ControlFlowPlugin_l171_1 - 32'h00000001);
  assign _zz__zz_when_BlockMovePlugin_l165 = (_zz_when_BlockMovePlugin_l165_1 * _zz_when_BlockMovePlugin_l165_2);
  assign _zz_when_IndexingPlugin_l135_11 = SecondaryOpcode_BSUB;
  assign _zz_when_IndexingPlugin_l135_10 = _zz_when_IndexingPlugin_l135_11[3:0];
  assign _zz_when_IndexingPlugin_l135_14 = SecondaryOpcode_WSUB;
  assign _zz_when_IndexingPlugin_l135_13 = _zz_when_IndexingPlugin_l135_14[3:0];
  assign _zz_when_IndexingPlugin_l135_16 = SecondaryOpcode_LB;
  assign _zz_when_IndexingPlugin_l135_15 = _zz_when_IndexingPlugin_l135_16[3:0];
  assign _zz_when_IndexingPlugin_l135_18 = SecondaryOpcode_SB;
  assign _zz_when_IndexingPlugin_l135_17 = _zz_when_IndexingPlugin_l135_18[3:0];
  assign _zz_when_IndexingPlugin_l135_20 = SecondaryOpcode_LSX;
  assign _zz_when_IndexingPlugin_l135_19 = _zz_when_IndexingPlugin_l135_20[3:0];
  assign _zz_when_IndexingPlugin_l135_22 = SecondaryOpcode_SS;
  assign _zz_when_IndexingPlugin_l135_21 = _zz_when_IndexingPlugin_l135_22[3:0];
  assign _zz_61 = SecondaryOpcode_BSUB;
  assign _zz_60 = _zz_61[3:0];
  assign _zz_63 = SecondaryOpcode_WSUB;
  assign _zz_62 = _zz_63[3:0];
  assign _zz_65 = SecondaryOpcode_LB;
  assign _zz_64 = _zz_65[3:0];
  assign _zz_67 = SecondaryOpcode_SB;
  assign _zz_66 = _zz_67[3:0];
  assign _zz_69 = SecondaryOpcode_LSX;
  assign _zz_68 = _zz_69[3:0];
  assign _zz_71 = SecondaryOpcode_SS;
  assign _zz_70 = _zz_71[3:0];
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_86 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_85 & 32'hffffffff);
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77 = {26'd0, _zz_fpu_fpPipe_ctrl_0_down_RESULT_78};
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_1 = {26'd0, _zz_fpu_fpPipe_ctrl_0_down_RESULT_78};
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_2 = {26'd0, _zz_fpu_fpPipe_ctrl_0_down_RESULT_78};
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_3 = {26'd0, _zz_fpu_fpPipe_ctrl_0_down_RESULT_78};
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_4 = {26'd0, _zz_fpu_fpPipe_ctrl_0_down_RESULT_78};
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_5 = {26'd0, _zz_fpu_fpPipe_ctrl_0_down_RESULT_78};
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_6 = (_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_7 + _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_8);
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_7 = {2'd0, _zz_fpu_fpPipe_ctrl_0_down_RESULT_82};
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_8 = ({2'd0,_zz_fpu_fpPipe_ctrl_0_down_RESULT_80} <<< 2'd2);
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_103 = (_zz_when_RangeCheckPlugin_l191_1 - 32'h00000001);
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_106 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_1 + 32'h00000001);
  assign _zz_when_GeneralPlugin_l69_3 = SecondaryOpcode_REV;
  assign _zz_when_GeneralPlugin_l69_2 = _zz_when_GeneralPlugin_l69_3[3:0];
  assign _zz_when_GeneralPlugin_l69_5 = SecondaryOpcode_DUP;
  assign _zz_when_GeneralPlugin_l69_4 = _zz_when_GeneralPlugin_l69_5[3:0];
  assign _zz_when_GeneralPlugin_l69_7 = SecondaryOpcode_POP;
  assign _zz_when_GeneralPlugin_l69_6 = _zz_when_GeneralPlugin_l69_7[3:0];
  assign _zz_when_GeneralPlugin_l69_9 = SecondaryOpcode_NOP;
  assign _zz_when_GeneralPlugin_l69_8 = _zz_when_GeneralPlugin_l69_9[3:0];
  assign _zz_when_GeneralPlugin_l69_11 = SecondaryOpcode_MINT;
  assign _zz_when_GeneralPlugin_l69_10 = _zz_when_GeneralPlugin_l69_11[3:0];
  assign _zz_73 = SecondaryOpcode_REV;
  assign _zz_72 = _zz_73[3:0];
  assign _zz_75 = SecondaryOpcode_DUP;
  assign _zz_74 = _zz_75[3:0];
  assign _zz_77 = SecondaryOpcode_POP;
  assign _zz_76 = _zz_77[3:0];
  assign _zz_79 = SecondaryOpcode_NOP;
  assign _zz_78 = _zz_79[3:0];
  assign _zz_81 = SecondaryOpcode_MINT;
  assign _zz_80 = _zz_81[3:0];
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_121 = ({31'd0,_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_121_1} <<< 5'd31);
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_121_1 = (1'b0 - 1'b1);
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_145_1 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_124;
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_145 = {30'd0, _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_145_1};
  assign _zz_83 = SecondaryOpcode_FPADD;
  assign _zz_82 = _zz_83[7:0];
  assign _zz_85 = SecondaryOpcode_FPSUB;
  assign _zz_84 = _zz_85[7:0];
  assign _zz_87 = SecondaryOpcode_FPMUL;
  assign _zz_86 = _zz_87[7:0];
  assign _zz_89 = SecondaryOpcode_FPDIV;
  assign _zz_88 = _zz_89[7:0];
  assign _zz_91 = SecondaryOpcode_FPSQRT;
  assign _zz_90 = _zz_91[7:0];
  assign _zz_93 = SecondaryOpcode_FPREM;
  assign _zz_92 = _zz_93[7:0];
  assign _zz_95 = SecondaryOpcode_FPRANGE;
  assign _zz_94 = _zz_95[7:0];
  assign _zz_97 = SecondaryOpcode_FPABS;
  assign _zz_96 = _zz_97[7:0];
  assign _zz_99 = SecondaryOpcode_FPINT;
  assign _zz_98 = _zz_99[7:0];
  assign _zz_101 = SecondaryOpcode_FPMULBY2;
  assign _zz_100 = _zz_101[7:0];
  assign _zz_103 = SecondaryOpcode_FPDIVBY2;
  assign _zz_102 = _zz_103[7:0];
  assign _zz_105 = SecondaryOpcode_FPRN;
  assign _zz_104 = _zz_105[7:0];
  assign _zz_107 = SecondaryOpcode_FPRP;
  assign _zz_106 = _zz_107[7:0];
  assign _zz_109 = SecondaryOpcode_FPRM;
  assign _zz_108 = _zz_109[7:0];
  assign _zz_111 = SecondaryOpcode_FPRZ;
  assign _zz_110 = _zz_111[7:0];
  assign _zz_fpu_fpPipe_ctrl_0_up_valid_3 = SecondaryOpcode_FPADD;
  assign _zz_fpu_fpPipe_ctrl_0_up_valid_2 = _zz_fpu_fpPipe_ctrl_0_up_valid_3[7:0];
  assign _zz_fpu_fpPipe_ctrl_0_up_valid_5 = SecondaryOpcode_FPSUB;
  assign _zz_fpu_fpPipe_ctrl_0_up_valid_4 = _zz_fpu_fpPipe_ctrl_0_up_valid_5[7:0];
  assign _zz_fpu_fpPipe_ctrl_0_up_valid_7 = SecondaryOpcode_FPMUL;
  assign _zz_fpu_fpPipe_ctrl_0_up_valid_6 = _zz_fpu_fpPipe_ctrl_0_up_valid_7[7:0];
  assign _zz_fpu_fpPipe_ctrl_0_up_valid_9 = SecondaryOpcode_FPDIV;
  assign _zz_fpu_fpPipe_ctrl_0_up_valid_8 = _zz_fpu_fpPipe_ctrl_0_up_valid_9[7:0];
  assign _zz_113 = SecondaryOpcode_FPREV;
  assign _zz_112 = _zz_113[7:0];
  assign _zz_115 = SecondaryOpcode_FPDUP;
  assign _zz_114 = _zz_115[7:0];
  assign _zz_117 = SecondaryOpcode_FPRN;
  assign _zz_116 = _zz_117[7:0];
  assign _zz_119 = SecondaryOpcode_FPRZ;
  assign _zz_118 = _zz_119[7:0];
  assign _zz_121 = SecondaryOpcode_FPRP;
  assign _zz_120 = _zz_121[7:0];
  assign _zz_123 = SecondaryOpcode_FPRM;
  assign _zz_122 = _zz_123[7:0];
  assign _zz_125 = SecondaryOpcode_FPGT;
  assign _zz_124 = _zz_125[7:0];
  assign _zz_127 = SecondaryOpcode_FPEQ;
  assign _zz_126 = _zz_127[7:0];
  assign _zz_129 = SecondaryOpcode_FPGE;
  assign _zz_128 = _zz_129[7:0];
  assign _zz_131 = SecondaryOpcode_FPLG;
  assign _zz_130 = _zz_131[7:0];
  assign _zz_133 = SecondaryOpcode_FPORDERED;
  assign _zz_132 = _zz_133[7:0];
  assign _zz_135 = SecondaryOpcode_FPNAN;
  assign _zz_134 = _zz_135[7:0];
  assign _zz_137 = SecondaryOpcode_FPNOTFINITE;
  assign _zz_136 = _zz_137[7:0];
  assign _zz_139 = SecondaryOpcode_FPR32TOR64;
  assign _zz_138 = _zz_139[7:0];
  assign _zz_141 = SecondaryOpcode_FPR64TOR32;
  assign _zz_140 = _zz_141[7:0];
  assign _zz_143 = SecondaryOpcode_FPRTOI32;
  assign _zz_142 = _zz_143[7:0];
  assign _zz_145 = SecondaryOpcode_FPI32TOR32;
  assign _zz_144 = _zz_145[7:0];
  assign _zz_147 = SecondaryOpcode_FPADD;
  assign _zz_146 = _zz_147[7:0];
  assign _zz_149 = SecondaryOpcode_FPSUB;
  assign _zz_148 = _zz_149[7:0];
  assign _zz_151 = SecondaryOpcode_FPMUL;
  assign _zz_150 = _zz_151[7:0];
  assign _zz_153 = SecondaryOpcode_FPDIV;
  assign _zz_152 = _zz_153[7:0];
  assign _zz_155 = SecondaryOpcode_FPSQRT;
  assign _zz_154 = _zz_155[7:0];
  assign _zz_157 = SecondaryOpcode_FPREM;
  assign _zz_156 = _zz_157[7:0];
  assign _zz_159 = SecondaryOpcode_FPRANGE;
  assign _zz_158 = _zz_159[7:0];
  assign _zz_161 = SecondaryOpcode_FPABS;
  assign _zz_160 = _zz_161[7:0];
  assign _zz_163 = SecondaryOpcode_FPINT;
  assign _zz_162 = _zz_163[7:0];
  assign _zz_165 = SecondaryOpcode_FPMULBY2;
  assign _zz_164 = _zz_165[7:0];
  assign _zz_167 = SecondaryOpcode_FPDIVBY2;
  assign _zz_166 = _zz_167[7:0];
  assign _zz_169 = SecondaryOpcode_FPRN;
  assign _zz_168 = _zz_169[7:0];
  assign _zz_171 = SecondaryOpcode_FPRP;
  assign _zz_170 = _zz_171[7:0];
  assign _zz_173 = SecondaryOpcode_FPRM;
  assign _zz_172 = _zz_173[7:0];
  assign _zz_175 = SecondaryOpcode_FPRZ;
  assign _zz_174 = _zz_175[7:0];
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_230 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_231;
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_229 = {1'd0, _zz_fpu_fpPipe_ctrl_0_down_RESULT_230};
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_231 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_232 - 10'h07f);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_232 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_233 + 10'h3ff);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_234 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_216[30 : 23];
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_233 = {2'd0, _zz_fpu_fpPipe_ctrl_0_down_RESULT_234};
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_217 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_212;
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_235 = {{_zz_fpu_fpPipe_ctrl_0_down_RESULT_217[63],_zz_fpu_fpPipe_ctrl_0_down_RESULT_236[7 : 0]},_zz_fpu_fpPipe_ctrl_0_down_RESULT_239[22 : 0]};
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_236 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_237;
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_237 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_238 + 11'h07f);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_238 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_217[62 : 52] - 11'h3ff);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_239 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_240[51 : 29];
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_240 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_217[51 : 0];
  assign _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_219 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_218 & 32'hffffffff);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_241 = {{_zz_fpu_fpPipe_ctrl_0_down_RESULT_219[31],_zz_fpu_fpPipe_ctrl_0_down_RESULT_242[7 : 0]},_zz_fpu_fpPipe_ctrl_0_down_RESULT_243[22 : 0]};
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_242 = 8'h7f;
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_243 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_219[31 : 9];
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_244 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_221 + _zz_fpu_fpPipe_ctrl_0_down_RESULT_245);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_246 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_225;
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_245 = {51'd0, _zz_fpu_fpPipe_ctrl_0_down_RESULT_246};
  assign _zz_when_IndexingPlugin_l135_5 = (_zz_when_IndexingPlugin_l135_4 == PrimaryOpcode_LDL);
  assign _zz_when_IndexingPlugin_l135_6 = (_zz_when_IndexingPlugin_l135_4 == PrimaryOpcode_STL);
  assign _zz_when_IndexingPlugin_l135_7 = PrimaryOpcode_LDNL;
  assign _zz_when_IndexingPlugin_l135_8 = PrimaryOpcode_STNL;
  assign _zz_when_IndexingPlugin_l135_9 = (_zz_when_IndexingPlugin_l135_3 == _zz_when_IndexingPlugin_l135_10);
  assign _zz_when_IndexingPlugin_l135_12 = (_zz_when_IndexingPlugin_l135_3 == _zz_when_IndexingPlugin_l135_13);
  assign _zz_when_RangeCheckPlugin_l124_5 = ((_zz_when_RangeCheckPlugin_l124_3 == 4'b0111) || (_zz_when_RangeCheckPlugin_l124_3 == 4'b1010));
  assign _zz_when_RangeCheckPlugin_l124_6 = (_zz_when_RangeCheckPlugin_l124_3 == 4'b1010);
  assign _zz_when_RangeCheckPlugin_l124_7 = 4'b0110;
  assign _zz_when_RangeCheckPlugin_l124_8 = 4'b1000;
  BmbUpSizerBridge bmbUpSizerBridge_1 (
    .io_input_cmd_valid                     (_zz_io_input_cmd_valid_1                                       ), //i
    .io_input_cmd_ready                     (bmbUpSizerBridge_1_io_input_cmd_ready                          ), //o
    .io_input_cmd_payload_last              (1'b1                                                           ), //i
    .io_input_cmd_payload_fragment_source   (1'b0                                                           ), //i
    .io_input_cmd_payload_fragment_opcode   (1'b0                                                           ), //i
    .io_input_cmd_payload_fragment_address  (_zz_io_input_cmd_payload_fragment_address[31:0]                ), //i
    .io_input_cmd_payload_fragment_length   (3'b111                                                         ), //i
    .io_input_cmd_payload_fragment_data     (_zz_io_input_cmd_payload_fragment_data[63:0]                   ), //i
    .io_input_cmd_payload_fragment_mask     (_zz_io_input_cmd_payload_fragment_mask[7:0]                    ), //i
    .io_input_rsp_valid                     (bmbUpSizerBridge_1_io_input_rsp_valid                          ), //o
    .io_input_rsp_ready                     (_zz_io_input_rsp_ready                                         ), //i
    .io_input_rsp_payload_last              (bmbUpSizerBridge_1_io_input_rsp_payload_last                   ), //o
    .io_input_rsp_payload_fragment_source   (bmbUpSizerBridge_1_io_input_rsp_payload_fragment_source        ), //o
    .io_input_rsp_payload_fragment_opcode   (bmbUpSizerBridge_1_io_input_rsp_payload_fragment_opcode        ), //o
    .io_input_rsp_payload_fragment_data     (bmbUpSizerBridge_1_io_input_rsp_payload_fragment_data[63:0]    ), //o
    .io_output_cmd_valid                    (bmbUpSizerBridge_1_io_output_cmd_valid                         ), //o
    .io_output_cmd_ready                    (1'b1                                                           ), //i
    .io_output_cmd_payload_last             (bmbUpSizerBridge_1_io_output_cmd_payload_last                  ), //o
    .io_output_cmd_payload_fragment_source  (bmbUpSizerBridge_1_io_output_cmd_payload_fragment_source       ), //o
    .io_output_cmd_payload_fragment_opcode  (bmbUpSizerBridge_1_io_output_cmd_payload_fragment_opcode       ), //o
    .io_output_cmd_payload_fragment_address (bmbUpSizerBridge_1_io_output_cmd_payload_fragment_address[31:0]), //o
    .io_output_cmd_payload_fragment_length  (bmbUpSizerBridge_1_io_output_cmd_payload_fragment_length[2:0]  ), //o
    .io_output_cmd_payload_fragment_data    (bmbUpSizerBridge_1_io_output_cmd_payload_fragment_data[127:0]  ), //o
    .io_output_cmd_payload_fragment_mask    (bmbUpSizerBridge_1_io_output_cmd_payload_fragment_mask[15:0]   ), //o
    .io_output_cmd_payload_fragment_context (bmbUpSizerBridge_1_io_output_cmd_payload_fragment_context[1:0] ), //o
    .io_output_rsp_valid                    (1'b0                                                           ), //i
    .io_output_rsp_ready                    (bmbUpSizerBridge_1_io_output_rsp_ready                         ), //o
    .io_output_rsp_payload_last             (1'b1                                                           ), //i
    .io_output_rsp_payload_fragment_source  (1'b0                                                           ), //i
    .io_output_rsp_payload_fragment_opcode  (                                                               ), //i
    .io_output_rsp_payload_fragment_data    (128'h0                                                         ), //i
    .io_output_rsp_payload_fragment_context (                                                               ), //i
    .clk                                    (clk                                                            ), //i
    .reset                                  (reset                                                          )  //i
  );
  FpuAdder fpuAdder_1 (
    .io_cmd_valid            (fpuAdder_1_io_cmd_valid                ), //i
    .io_cmd_ready            (fpuAdder_1_io_cmd_ready                ), //o
    .io_cmd_payload_a        (fpuAdder_1_io_cmd_payload_a[63:0]      ), //i
    .io_cmd_payload_b        (fpuAdder_1_io_cmd_payload_b[63:0]      ), //i
    .io_cmd_payload_sub      (fpuAdder_1_io_cmd_payload_sub          ), //i
    .io_cmd_payload_rounding (fpuAdder_1_io_cmd_payload_rounding[1:0]), //i
    .io_rsp_valid            (fpuAdder_1_io_rsp_valid                ), //o
    .io_rsp_ready            (                                       ), //i
    .io_rsp_payload          (fpuAdder_1_io_rsp_payload[63:0]        ), //o
    .clk                     (clk                                    ), //i
    .reset                   (reset                                  )  //i
  );
  StreamFifo streamFifo_2 (
    .io_push_valid   (streamFifo_2_io_push_valid       ), //i
    .io_push_ready   (streamFifo_2_io_push_ready       ), //o
    .io_push_payload (_zz_io_push_payload_1[31:0]      ), //i
    .io_pop_valid    (streamFifo_2_io_pop_valid        ), //o
    .io_pop_ready    (streamFifo_2_io_pop_ready        ), //i
    .io_pop_payload  (streamFifo_2_io_pop_payload[31:0]), //o
    .io_flush        (1'b0                             ), //i
    .io_occupancy    (streamFifo_2_io_occupancy[4:0]   ), //o
    .io_availability (streamFifo_2_io_availability[4:0]), //o
    .clk             (clk                              ), //i
    .reset           (reset                            )  //i
  );
  StreamFifo streamFifo_3 (
    .io_push_valid   (streamFifo_3_io_push_valid       ), //i
    .io_push_ready   (streamFifo_3_io_push_ready       ), //o
    .io_push_payload (_zz_io_push_payload[31:0]        ), //i
    .io_pop_valid    (streamFifo_3_io_pop_valid        ), //o
    .io_pop_ready    (streamFifo_3_io_pop_ready        ), //i
    .io_pop_payload  (streamFifo_3_io_pop_payload[31:0]), //o
    .io_flush        (1'b0                             ), //i
    .io_occupancy    (streamFifo_3_io_occupancy[4:0]   ), //o
    .io_availability (streamFifo_3_io_availability[4:0]), //o
    .clk             (clk                              ), //i
    .reset           (reset                            )  //i
  );
  always @(*) begin
    case(_zz_when_ChannelPlugin_l127_3)
      4'b0000 : begin
        _zz_when_ChannelPlugin_l129_17 = _zz_when_ChannelPlugin_l129;
        _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_152 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_125;
      end
      4'b0001 : begin
        _zz_when_ChannelPlugin_l129_17 = _zz_when_ChannelPlugin_l129_1;
        _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_152 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_126;
      end
      4'b0010 : begin
        _zz_when_ChannelPlugin_l129_17 = _zz_when_ChannelPlugin_l129_2;
        _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_152 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_127;
      end
      4'b0011 : begin
        _zz_when_ChannelPlugin_l129_17 = _zz_when_ChannelPlugin_l129_3;
        _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_152 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_128;
      end
      4'b0100 : begin
        _zz_when_ChannelPlugin_l129_17 = _zz_when_ChannelPlugin_l129_4;
        _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_152 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_129;
      end
      4'b0101 : begin
        _zz_when_ChannelPlugin_l129_17 = _zz_when_ChannelPlugin_l129_5;
        _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_152 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_130;
      end
      4'b0110 : begin
        _zz_when_ChannelPlugin_l129_17 = _zz_when_ChannelPlugin_l129_6;
        _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_152 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_131;
      end
      4'b0111 : begin
        _zz_when_ChannelPlugin_l129_17 = _zz_when_ChannelPlugin_l129_7;
        _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_152 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_132;
      end
      4'b1000 : begin
        _zz_when_ChannelPlugin_l129_17 = _zz_when_ChannelPlugin_l129_8;
        _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_152 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_133;
      end
      4'b1001 : begin
        _zz_when_ChannelPlugin_l129_17 = _zz_when_ChannelPlugin_l129_9;
        _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_152 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_134;
      end
      4'b1010 : begin
        _zz_when_ChannelPlugin_l129_17 = _zz_when_ChannelPlugin_l129_10;
        _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_152 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_135;
      end
      4'b1011 : begin
        _zz_when_ChannelPlugin_l129_17 = _zz_when_ChannelPlugin_l129_11;
        _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_152 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_136;
      end
      4'b1100 : begin
        _zz_when_ChannelPlugin_l129_17 = _zz_when_ChannelPlugin_l129_12;
        _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_152 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_137;
      end
      4'b1101 : begin
        _zz_when_ChannelPlugin_l129_17 = _zz_when_ChannelPlugin_l129_13;
        _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_152 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_138;
      end
      4'b1110 : begin
        _zz_when_ChannelPlugin_l129_17 = _zz_when_ChannelPlugin_l129_14;
        _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_152 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_139;
      end
      default : begin
        _zz_when_ChannelPlugin_l129_17 = _zz_when_ChannelPlugin_l129_15;
        _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_152 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_140;
      end
    endcase
  end

  `ifndef SYNTHESIS
  always @(*) begin
    case(switch_ArithmeticPlugin_l160)
      AluOp_ADD : switch_ArithmeticPlugin_l160_string = "ADD  ";
      AluOp_SUB : switch_ArithmeticPlugin_l160_string = "SUB  ";
      AluOp_MUL : switch_ArithmeticPlugin_l160_string = "MUL  ";
      AluOp_DIV : switch_ArithmeticPlugin_l160_string = "DIV  ";
      AluOp_REM_1 : switch_ArithmeticPlugin_l160_string = "REM_1";
      AluOp_AND_1 : switch_ArithmeticPlugin_l160_string = "AND_1";
      AluOp_OR_1 : switch_ArithmeticPlugin_l160_string = "OR_1 ";
      AluOp_XOR_1 : switch_ArithmeticPlugin_l160_string = "XOR_1";
      AluOp_NOT_1 : switch_ArithmeticPlugin_l160_string = "NOT_1";
      AluOp_SHL : switch_ArithmeticPlugin_l160_string = "SHL  ";
      AluOp_SHR : switch_ArithmeticPlugin_l160_string = "SHR  ";
      AluOp_GT : switch_ArithmeticPlugin_l160_string = "GT   ";
      AluOp_GTU : switch_ArithmeticPlugin_l160_string = "GTU  ";
      AluOp_DIFF : switch_ArithmeticPlugin_l160_string = "DIFF ";
      AluOp_SUM : switch_ArithmeticPlugin_l160_string = "SUM  ";
      AluOp_PROD : switch_ArithmeticPlugin_l160_string = "PROD ";
      AluOp_REV : switch_ArithmeticPlugin_l160_string = "REV  ";
      AluOp_DUP : switch_ArithmeticPlugin_l160_string = "DUP  ";
      AluOp_FMUL : switch_ArithmeticPlugin_l160_string = "FMUL ";
      default : switch_ArithmeticPlugin_l160_string = "?????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245)
      RegName_Areg : switch_Misc_l245_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_string = "TnextReg1   ";
      default : switch_Misc_l245_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_1)
      RegName_Areg : switch_Misc_l245_1_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_1_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_1_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_1_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_1_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_1_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_1_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_1_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_1_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_1_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_1_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_1_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_1_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_1_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_1_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_1_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_1_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_1_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_1_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_1_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_1_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_1_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_1_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_1_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_1_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_1_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_1_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_1_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_1_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_1_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_1_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_1_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_1_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_1_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_1_string = "TnextReg1   ";
      default : switch_Misc_l245_1_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_2)
      RegName_Areg : switch_Misc_l245_2_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_2_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_2_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_2_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_2_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_2_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_2_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_2_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_2_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_2_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_2_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_2_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_2_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_2_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_2_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_2_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_2_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_2_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_2_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_2_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_2_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_2_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_2_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_2_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_2_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_2_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_2_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_2_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_2_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_2_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_2_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_2_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_2_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_2_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_2_string = "TnextReg1   ";
      default : switch_Misc_l245_2_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_LongArithPlugin_l132)
      LongArithOp_LADD : switch_LongArithPlugin_l132_string = "LADD      ";
      LongArithOp_LSUB : switch_LongArithPlugin_l132_string = "LSUB      ";
      LongArithOp_LMUL : switch_LongArithPlugin_l132_string = "LMUL      ";
      LongArithOp_LDIV : switch_LongArithPlugin_l132_string = "LDIV      ";
      LongArithOp_LSHL : switch_LongArithPlugin_l132_string = "LSHL      ";
      LongArithOp_LSHR : switch_LongArithPlugin_l132_string = "LSHR      ";
      LongArithOp_LSUM : switch_LongArithPlugin_l132_string = "LSUM      ";
      LongArithOp_LDIFF : switch_LongArithPlugin_l132_string = "LDIFF     ";
      LongArithOp_MINT : switch_LongArithPlugin_l132_string = "MINT      ";
      LongArithOp_BINT : switch_LongArithPlugin_l132_string = "BINT      ";
      LongArithOp_XSWORD : switch_LongArithPlugin_l132_string = "XSWORD    ";
      LongArithOp_RESCHEDULE : switch_LongArithPlugin_l132_string = "RESCHEDULE";
      LongArithOp_SLMUL : switch_LongArithPlugin_l132_string = "SLMUL     ";
      LongArithOp_SULMUL : switch_LongArithPlugin_l132_string = "SULMUL    ";
      LongArithOp_XDBLE : switch_LongArithPlugin_l132_string = "XDBLE     ";
      LongArithOp_XWORD : switch_LongArithPlugin_l132_string = "XWORD     ";
      LongArithOp_NORMALISE : switch_LongArithPlugin_l132_string = "NORMALISE ";
      LongArithOp_PROD : switch_LongArithPlugin_l132_string = "PROD      ";
      default : switch_LongArithPlugin_l132_string = "??????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_3)
      RegName_Areg : switch_Misc_l245_3_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_3_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_3_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_3_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_3_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_3_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_3_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_3_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_3_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_3_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_3_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_3_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_3_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_3_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_3_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_3_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_3_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_3_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_3_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_3_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_3_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_3_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_3_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_3_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_3_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_3_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_3_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_3_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_3_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_3_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_3_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_3_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_3_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_3_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_3_string = "TnextReg1   ";
      default : switch_Misc_l245_3_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_4)
      RegName_Areg : switch_Misc_l245_4_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_4_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_4_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_4_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_4_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_4_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_4_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_4_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_4_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_4_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_4_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_4_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_4_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_4_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_4_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_4_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_4_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_4_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_4_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_4_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_4_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_4_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_4_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_4_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_4_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_4_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_4_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_4_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_4_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_4_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_4_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_4_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_4_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_4_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_4_string = "TnextReg1   ";
      default : switch_Misc_l245_4_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_5)
      RegName_Areg : switch_Misc_l245_5_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_5_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_5_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_5_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_5_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_5_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_5_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_5_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_5_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_5_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_5_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_5_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_5_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_5_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_5_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_5_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_5_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_5_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_5_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_5_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_5_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_5_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_5_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_5_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_5_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_5_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_5_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_5_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_5_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_5_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_5_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_5_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_5_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_5_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_5_string = "TnextReg1   ";
      default : switch_Misc_l245_5_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_ControlFlowPlugin_l132)
      ControlFlowOp_RET : switch_ControlFlowPlugin_l132_string = "RET       ";
      ControlFlowOp_LDPI : switch_ControlFlowPlugin_l132_string = "LDPI      ";
      ControlFlowOp_GAJW : switch_ControlFlowPlugin_l132_string = "GAJW      ";
      ControlFlowOp_GCALL : switch_ControlFlowPlugin_l132_string = "GCALL     ";
      ControlFlowOp_LEND : switch_ControlFlowPlugin_l132_string = "LEND      ";
      ControlFlowOp_ENDP : switch_ControlFlowPlugin_l132_string = "ENDP      ";
      ControlFlowOp_DISS : switch_ControlFlowPlugin_l132_string = "DISS      ";
      ControlFlowOp_STHF : switch_ControlFlowPlugin_l132_string = "STHF      ";
      ControlFlowOp_STLF : switch_ControlFlowPlugin_l132_string = "STLF      ";
      ControlFlowOp_STHB : switch_ControlFlowPlugin_l132_string = "STHB      ";
      ControlFlowOp_STLB : switch_ControlFlowPlugin_l132_string = "STLB      ";
      ControlFlowOp_SAVEL : switch_ControlFlowPlugin_l132_string = "SAVEL     ";
      ControlFlowOp_SAVEH : switch_ControlFlowPlugin_l132_string = "SAVEH     ";
      ControlFlowOp_WCNT : switch_ControlFlowPlugin_l132_string = "WCNT      ";
      ControlFlowOp_SHR : switch_ControlFlowPlugin_l132_string = "SHR       ";
      ControlFlowOp_SHL : switch_ControlFlowPlugin_l132_string = "SHL       ";
      ControlFlowOp_NORM : switch_ControlFlowPlugin_l132_string = "NORM      ";
      ControlFlowOp_LDIV : switch_ControlFlowPlugin_l132_string = "LDIV      ";
      ControlFlowOp_LDIVSTEP : switch_ControlFlowPlugin_l132_string = "LDIVSTEP  ";
      ControlFlowOp_UNPACK_SNS : switch_ControlFlowPlugin_l132_string = "UNPACK_SNS";
      ControlFlowOp_POSTNORMSN : switch_ControlFlowPlugin_l132_string = "POSTNORMSN";
      ControlFlowOp_ROUNDSN : switch_ControlFlowPlugin_l132_string = "ROUNDSN   ";
      ControlFlowOp_LDINF : switch_ControlFlowPlugin_l132_string = "LDINF     ";
      default : switch_ControlFlowPlugin_l132_string = "??????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_6)
      RegName_Areg : switch_Misc_l245_6_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_6_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_6_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_6_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_6_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_6_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_6_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_6_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_6_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_6_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_6_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_6_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_6_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_6_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_6_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_6_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_6_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_6_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_6_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_6_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_6_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_6_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_6_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_6_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_6_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_6_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_6_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_6_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_6_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_6_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_6_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_6_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_6_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_6_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_6_string = "TnextReg1   ";
      default : switch_Misc_l245_6_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_7)
      RegName_Areg : switch_Misc_l245_7_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_7_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_7_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_7_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_7_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_7_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_7_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_7_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_7_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_7_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_7_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_7_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_7_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_7_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_7_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_7_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_7_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_7_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_7_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_7_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_7_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_7_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_7_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_7_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_7_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_7_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_7_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_7_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_7_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_7_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_7_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_7_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_7_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_7_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_7_string = "TnextReg1   ";
      default : switch_Misc_l245_7_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_8)
      RegName_Areg : switch_Misc_l245_8_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_8_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_8_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_8_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_8_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_8_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_8_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_8_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_8_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_8_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_8_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_8_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_8_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_8_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_8_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_8_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_8_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_8_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_8_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_8_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_8_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_8_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_8_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_8_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_8_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_8_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_8_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_8_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_8_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_8_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_8_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_8_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_8_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_8_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_8_string = "TnextReg1   ";
      default : switch_Misc_l245_8_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_BlockMovePlugin_l118)
      BlockMoveOp_MOVE : switch_BlockMovePlugin_l118_string = "MOVE         ";
      BlockMoveOp_MOVE2DINIT : switch_BlockMovePlugin_l118_string = "MOVE2DINIT   ";
      BlockMoveOp_MOVE2DALL : switch_BlockMovePlugin_l118_string = "MOVE2DALL    ";
      BlockMoveOp_MOVE2DNONZERO : switch_BlockMovePlugin_l118_string = "MOVE2DNONZERO";
      BlockMoveOp_MOVE2DZERO : switch_BlockMovePlugin_l118_string = "MOVE2DZERO   ";
      default : switch_BlockMovePlugin_l118_string = "?????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_9)
      RegName_Areg : switch_Misc_l245_9_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_9_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_9_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_9_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_9_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_9_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_9_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_9_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_9_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_9_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_9_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_9_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_9_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_9_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_9_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_9_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_9_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_9_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_9_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_9_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_9_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_9_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_9_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_9_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_9_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_9_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_9_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_9_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_9_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_9_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_9_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_9_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_9_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_9_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_9_string = "TnextReg1   ";
      default : switch_Misc_l245_9_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_10)
      RegName_Areg : switch_Misc_l245_10_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_10_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_10_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_10_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_10_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_10_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_10_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_10_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_10_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_10_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_10_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_10_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_10_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_10_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_10_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_10_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_10_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_10_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_10_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_10_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_10_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_10_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_10_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_10_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_10_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_10_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_10_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_10_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_10_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_10_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_10_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_10_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_10_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_10_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_10_string = "TnextReg1   ";
      default : switch_Misc_l245_10_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_11)
      RegName_Areg : switch_Misc_l245_11_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_11_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_11_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_11_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_11_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_11_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_11_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_11_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_11_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_11_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_11_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_11_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_11_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_11_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_11_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_11_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_11_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_11_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_11_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_11_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_11_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_11_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_11_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_11_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_11_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_11_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_11_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_11_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_11_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_11_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_11_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_11_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_11_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_11_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_11_string = "TnextReg1   ";
      default : switch_Misc_l245_11_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_IndexingPlugin_l155)
      IndexingOp_BSUB : switch_IndexingPlugin_l155_string = "BSUB ";
      IndexingOp_WSUB : switch_IndexingPlugin_l155_string = "WSUB ";
      IndexingOp_LB : switch_IndexingPlugin_l155_string = "LB   ";
      IndexingOp_SB : switch_IndexingPlugin_l155_string = "SB   ";
      IndexingOp_LSX : switch_IndexingPlugin_l155_string = "LSX  ";
      IndexingOp_SS : switch_IndexingPlugin_l155_string = "SS   ";
      IndexingOp_LDL : switch_IndexingPlugin_l155_string = "LDL  ";
      IndexingOp_STL : switch_IndexingPlugin_l155_string = "STL  ";
      IndexingOp_LDNL : switch_IndexingPlugin_l155_string = "LDNL ";
      IndexingOp_STNL : switch_IndexingPlugin_l155_string = "STNL ";
      IndexingOp_LDLP : switch_IndexingPlugin_l155_string = "LDLP ";
      IndexingOp_LDNLP : switch_IndexingPlugin_l155_string = "LDNLP";
      IndexingOp_LDPI : switch_IndexingPlugin_l155_string = "LDPI ";
      IndexingOp_GAJW : switch_IndexingPlugin_l155_string = "GAJW ";
      IndexingOp_EQC : switch_IndexingPlugin_l155_string = "EQC  ";
      IndexingOp_STLB : switch_IndexingPlugin_l155_string = "STLB ";
      IndexingOp_LDLB : switch_IndexingPlugin_l155_string = "LDLB ";
      IndexingOp_STNLB : switch_IndexingPlugin_l155_string = "STNLB";
      IndexingOp_LDNLB : switch_IndexingPlugin_l155_string = "LDNLB";
      default : switch_IndexingPlugin_l155_string = "?????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_12)
      RegName_Areg : switch_Misc_l245_12_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_12_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_12_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_12_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_12_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_12_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_12_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_12_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_12_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_12_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_12_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_12_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_12_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_12_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_12_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_12_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_12_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_12_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_12_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_12_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_12_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_12_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_12_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_12_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_12_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_12_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_12_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_12_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_12_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_12_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_12_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_12_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_12_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_12_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_12_string = "TnextReg1   ";
      default : switch_Misc_l245_12_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_13)
      RegName_Areg : switch_Misc_l245_13_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_13_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_13_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_13_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_13_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_13_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_13_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_13_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_13_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_13_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_13_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_13_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_13_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_13_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_13_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_13_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_13_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_13_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_13_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_13_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_13_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_13_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_13_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_13_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_13_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_13_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_13_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_13_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_13_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_13_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_13_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_13_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_13_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_13_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_13_string = "TnextReg1   ";
      default : switch_Misc_l245_13_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_14)
      RegName_Areg : switch_Misc_l245_14_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_14_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_14_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_14_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_14_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_14_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_14_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_14_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_14_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_14_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_14_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_14_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_14_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_14_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_14_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_14_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_14_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_14_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_14_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_14_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_14_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_14_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_14_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_14_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_14_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_14_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_14_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_14_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_14_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_14_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_14_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_14_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_14_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_14_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_14_string = "TnextReg1   ";
      default : switch_Misc_l245_14_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_15)
      RegName_Areg : switch_Misc_l245_15_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_15_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_15_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_15_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_15_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_15_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_15_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_15_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_15_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_15_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_15_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_15_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_15_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_15_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_15_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_15_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_15_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_15_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_15_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_15_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_15_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_15_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_15_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_15_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_15_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_15_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_15_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_15_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_15_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_15_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_15_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_15_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_15_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_15_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_15_string = "TnextReg1   ";
      default : switch_Misc_l245_15_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_RangeCheckPlugin_l137)
      RangeCheckOp_CIR : switch_RangeCheckPlugin_l137_string = "CIR   ";
      RangeCheckOp_CB : switch_RangeCheckPlugin_l137_string = "CB    ";
      RangeCheckOp_CS : switch_RangeCheckPlugin_l137_string = "CS    ";
      RangeCheckOp_CWORD : switch_RangeCheckPlugin_l137_string = "CWORD ";
      RangeCheckOp_XSWORD : switch_RangeCheckPlugin_l137_string = "XSWORD";
      RangeCheckOp_CCNT1 : switch_RangeCheckPlugin_l137_string = "CCNT1 ";
      RangeCheckOp_CJ : switch_RangeCheckPlugin_l137_string = "CJ    ";
      RangeCheckOp_CALL : switch_RangeCheckPlugin_l137_string = "CALL  ";
      RangeCheckOp_CSNGL : switch_RangeCheckPlugin_l137_string = "CSNGL ";
      RangeCheckOp_CDBL : switch_RangeCheckPlugin_l137_string = "CDBL  ";
      default : switch_RangeCheckPlugin_l137_string = "??????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_16)
      RegName_Areg : switch_Misc_l245_16_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_16_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_16_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_16_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_16_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_16_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_16_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_16_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_16_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_16_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_16_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_16_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_16_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_16_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_16_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_16_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_16_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_16_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_16_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_16_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_16_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_16_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_16_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_16_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_16_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_16_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_16_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_16_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_16_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_16_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_16_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_16_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_16_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_16_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_16_string = "TnextReg1   ";
      default : switch_Misc_l245_16_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_17)
      RegName_Areg : switch_Misc_l245_17_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_17_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_17_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_17_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_17_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_17_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_17_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_17_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_17_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_17_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_17_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_17_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_17_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_17_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_17_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_17_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_17_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_17_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_17_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_17_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_17_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_17_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_17_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_17_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_17_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_17_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_17_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_17_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_17_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_17_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_17_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_17_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_17_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_17_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_17_string = "TnextReg1   ";
      default : switch_Misc_l245_17_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_18)
      RegName_Areg : switch_Misc_l245_18_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_18_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_18_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_18_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_18_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_18_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_18_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_18_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_18_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_18_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_18_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_18_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_18_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_18_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_18_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_18_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_18_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_18_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_18_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_18_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_18_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_18_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_18_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_18_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_18_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_18_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_18_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_18_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_18_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_18_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_18_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_18_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_18_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_18_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_18_string = "TnextReg1   ";
      default : switch_Misc_l245_18_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_19)
      RegName_Areg : switch_Misc_l245_19_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_19_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_19_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_19_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_19_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_19_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_19_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_19_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_19_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_19_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_19_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_19_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_19_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_19_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_19_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_19_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_19_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_19_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_19_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_19_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_19_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_19_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_19_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_19_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_19_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_19_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_19_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_19_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_19_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_19_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_19_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_19_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_19_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_19_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_19_string = "TnextReg1   ";
      default : switch_Misc_l245_19_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_20)
      RegName_Areg : switch_Misc_l245_20_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_20_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_20_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_20_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_20_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_20_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_20_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_20_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_20_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_20_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_20_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_20_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_20_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_20_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_20_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_20_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_20_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_20_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_20_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_20_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_20_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_20_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_20_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_20_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_20_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_20_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_20_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_20_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_20_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_20_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_20_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_20_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_20_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_20_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_20_string = "TnextReg1   ";
      default : switch_Misc_l245_20_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_21)
      RegName_Areg : switch_Misc_l245_21_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_21_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_21_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_21_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_21_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_21_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_21_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_21_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_21_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_21_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_21_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_21_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_21_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_21_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_21_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_21_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_21_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_21_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_21_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_21_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_21_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_21_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_21_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_21_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_21_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_21_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_21_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_21_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_21_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_21_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_21_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_21_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_21_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_21_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_21_string = "TnextReg1   ";
      default : switch_Misc_l245_21_string = "????????????";
    endcase
  end
  always @(*) begin
    case(_zz_fpu_fpPipe_ctrl_0_down_RESULT_124)
      ChannelType_PHYSICAL : _zz_fpu_fpPipe_ctrl_0_down_RESULT_124_string = "PHYSICAL ";
      ChannelType_VIRTUAL_1 : _zz_fpu_fpPipe_ctrl_0_down_RESULT_124_string = "VIRTUAL_1";
      ChannelType_RESOURCE : _zz_fpu_fpPipe_ctrl_0_down_RESULT_124_string = "RESOURCE ";
      ChannelType_INVALID : _zz_fpu_fpPipe_ctrl_0_down_RESULT_124_string = "INVALID  ";
      default : _zz_fpu_fpPipe_ctrl_0_down_RESULT_124_string = "?????????";
    endcase
  end
  always @(*) begin
    case(switch_ChannelPlugin_l121)
      ChannelOp_CHANTYPE : switch_ChannelPlugin_l121_string = "CHANTYPE ";
      ChannelOp_INITVLCB : switch_ChannelPlugin_l121_string = "INITVLCB ";
      ChannelOp_SETCHMODE : switch_ChannelPlugin_l121_string = "SETCHMODE";
      ChannelOp_SETHDR : switch_ChannelPlugin_l121_string = "SETHDR   ";
      ChannelOp_WRITEHDR : switch_ChannelPlugin_l121_string = "WRITEHDR ";
      ChannelOp_READHDR : switch_ChannelPlugin_l121_string = "READHDR  ";
      ChannelOp_SWAPBFR : switch_ChannelPlugin_l121_string = "SWAPBFR  ";
      ChannelOp_UNMKRC : switch_ChannelPlugin_l121_string = "UNMKRC   ";
      ChannelOp_MKRC : switch_ChannelPlugin_l121_string = "MKRC     ";
      default : switch_ChannelPlugin_l121_string = "?????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_22)
      RegName_Areg : switch_Misc_l245_22_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_22_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_22_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_22_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_22_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_22_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_22_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_22_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_22_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_22_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_22_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_22_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_22_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_22_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_22_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_22_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_22_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_22_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_22_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_22_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_22_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_22_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_22_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_22_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_22_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_22_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_22_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_22_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_22_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_22_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_22_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_22_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_22_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_22_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_22_string = "TnextReg1   ";
      default : switch_Misc_l245_22_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_23)
      RegName_Areg : switch_Misc_l245_23_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_23_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_23_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_23_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_23_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_23_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_23_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_23_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_23_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_23_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_23_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_23_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_23_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_23_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_23_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_23_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_23_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_23_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_23_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_23_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_23_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_23_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_23_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_23_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_23_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_23_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_23_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_23_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_23_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_23_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_23_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_23_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_23_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_23_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_23_string = "TnextReg1   ";
      default : switch_Misc_l245_23_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_24)
      RegName_Areg : switch_Misc_l245_24_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_24_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_24_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_24_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_24_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_24_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_24_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_24_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_24_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_24_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_24_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_24_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_24_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_24_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_24_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_24_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_24_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_24_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_24_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_24_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_24_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_24_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_24_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_24_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_24_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_24_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_24_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_24_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_24_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_24_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_24_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_24_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_24_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_24_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_24_string = "TnextReg1   ";
      default : switch_Misc_l245_24_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_InterruptPlugin_l123)
      InterruptOp_INTDIS : switch_InterruptPlugin_l123_string = "INTDIS    ";
      InterruptOp_INTENB : switch_InterruptPlugin_l123_string = "INTENB    ";
      InterruptOp_LDTRAPPED : switch_InterruptPlugin_l123_string = "LDTRAPPED ";
      InterruptOp_STTRAPPED : switch_InterruptPlugin_l123_string = "STTRAPPED ";
      InterruptOp_LDSHADOW : switch_InterruptPlugin_l123_string = "LDSHADOW  ";
      InterruptOp_STSHADOW : switch_InterruptPlugin_l123_string = "STSHADOW  ";
      InterruptOp_RESTART : switch_InterruptPlugin_l123_string = "RESTART   ";
      InterruptOp_CAUSEERROR : switch_InterruptPlugin_l123_string = "CAUSEERROR";
      default : switch_InterruptPlugin_l123_string = "??????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_25)
      RegName_Areg : switch_Misc_l245_25_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_25_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_25_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_25_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_25_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_25_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_25_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_25_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_25_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_25_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_25_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_25_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_25_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_25_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_25_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_25_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_25_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_25_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_25_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_25_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_25_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_25_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_25_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_25_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_25_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_25_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_25_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_25_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_25_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_25_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_25_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_25_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_25_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_25_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_25_string = "TnextReg1   ";
      default : switch_Misc_l245_25_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_26)
      RegName_Areg : switch_Misc_l245_26_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_26_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_26_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_26_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_26_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_26_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_26_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_26_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_26_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_26_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_26_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_26_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_26_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_26_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_26_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_26_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_26_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_26_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_26_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_26_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_26_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_26_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_26_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_26_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_26_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_26_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_26_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_26_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_26_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_26_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_26_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_26_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_26_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_26_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_26_string = "TnextReg1   ";
      default : switch_Misc_l245_26_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_27)
      RegName_Areg : switch_Misc_l245_27_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_27_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_27_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_27_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_27_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_27_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_27_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_27_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_27_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_27_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_27_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_27_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_27_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_27_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_27_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_27_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_27_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_27_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_27_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_27_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_27_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_27_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_27_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_27_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_27_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_27_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_27_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_27_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_27_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_27_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_27_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_27_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_27_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_27_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_27_string = "TnextReg1   ";
      default : switch_Misc_l245_27_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_ResourcePlugin_l119)
      ResourceOp_GRANT : switch_ResourcePlugin_l119_string = "GRANT   ";
      ResourceOp_ENBG : switch_ResourcePlugin_l119_string = "ENBG    ";
      ResourceOp_DISG : switch_ResourcePlugin_l119_string = "DISG    ";
      ResourceOp_MKRC : switch_ResourcePlugin_l119_string = "MKRC    ";
      ResourceOp_UNMKRC : switch_ResourcePlugin_l119_string = "UNMKRC  ";
      ResourceOp_IRDSQ : switch_ResourcePlugin_l119_string = "IRDSQ   ";
      ResourceOp_ERDSQ : switch_ResourcePlugin_l119_string = "ERDSQ   ";
      ResourceOp_STRESPTR : switch_ResourcePlugin_l119_string = "STRESPTR";
      ResourceOp_LDRESPTR : switch_ResourcePlugin_l119_string = "LDRESPTR";
      default : switch_ResourcePlugin_l119_string = "????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_28)
      RegName_Areg : switch_Misc_l245_28_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_28_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_28_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_28_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_28_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_28_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_28_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_28_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_28_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_28_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_28_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_28_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_28_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_28_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_28_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_28_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_28_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_28_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_28_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_28_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_28_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_28_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_28_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_28_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_28_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_28_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_28_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_28_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_28_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_28_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_28_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_28_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_28_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_28_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_28_string = "TnextReg1   ";
      default : switch_Misc_l245_28_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_29)
      RegName_Areg : switch_Misc_l245_29_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_29_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_29_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_29_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_29_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_29_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_29_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_29_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_29_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_29_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_29_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_29_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_29_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_29_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_29_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_29_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_29_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_29_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_29_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_29_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_29_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_29_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_29_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_29_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_29_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_29_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_29_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_29_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_29_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_29_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_29_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_29_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_29_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_29_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_29_string = "TnextReg1   ";
      default : switch_Misc_l245_29_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_30)
      RegName_Areg : switch_Misc_l245_30_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_30_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_30_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_30_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_30_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_30_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_30_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_30_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_30_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_30_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_30_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_30_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_30_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_30_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_30_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_30_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_30_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_30_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_30_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_30_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_30_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_30_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_30_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_30_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_30_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_30_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_30_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_30_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_30_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_30_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_30_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_30_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_30_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_30_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_30_string = "TnextReg1   ";
      default : switch_Misc_l245_30_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_SystemPlugin_l128)
      SystemOp_TESTPRANAL : switch_SystemPlugin_l128_string = "TESTPRANAL   ";
      SystemOp_LDCONF : switch_SystemPlugin_l128_string = "LDCONF       ";
      SystemOp_STCONF : switch_SystemPlugin_l128_string = "STCONF       ";
      SystemOp_SYSREQ : switch_SystemPlugin_l128_string = "SYSREQ       ";
      SystemOp_DEVMOVE : switch_SystemPlugin_l128_string = "DEVMOVE      ";
      SystemOp_SETTIMESLICE : switch_SystemPlugin_l128_string = "SETTIMESLICE ";
      SystemOp_LDMEMSTARTVAL : switch_SystemPlugin_l128_string = "LDMEMSTARTVAL";
      default : switch_SystemPlugin_l128_string = "?????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_31)
      RegName_Areg : switch_Misc_l245_31_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_31_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_31_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_31_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_31_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_31_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_31_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_31_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_31_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_31_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_31_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_31_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_31_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_31_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_31_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_31_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_31_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_31_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_31_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_31_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_31_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_31_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_31_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_31_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_31_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_31_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_31_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_31_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_31_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_31_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_31_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_31_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_31_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_31_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_31_string = "TnextReg1   ";
      default : switch_Misc_l245_31_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_32)
      RegName_Areg : switch_Misc_l245_32_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_32_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_32_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_32_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_32_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_32_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_32_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_32_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_32_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_32_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_32_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_32_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_32_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_32_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_32_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_32_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_32_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_32_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_32_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_32_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_32_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_32_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_32_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_32_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_32_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_32_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_32_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_32_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_32_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_32_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_32_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_32_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_32_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_32_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_32_string = "TnextReg1   ";
      default : switch_Misc_l245_32_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_33)
      RegName_Areg : switch_Misc_l245_33_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_33_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_33_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_33_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_33_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_33_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_33_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_33_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_33_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_33_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_33_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_33_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_33_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_33_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_33_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_33_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_33_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_33_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_33_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_33_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_33_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_33_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_33_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_33_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_33_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_33_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_33_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_33_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_33_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_33_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_33_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_33_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_33_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_33_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_33_string = "TnextReg1   ";
      default : switch_Misc_l245_33_string = "????????????";
    endcase
  end
  always @(*) begin
    case(switch_Misc_l245_35)
      RegName_Areg : switch_Misc_l245_35_string = "Areg        ";
      RegName_Breg : switch_Misc_l245_35_string = "Breg        ";
      RegName_Creg : switch_Misc_l245_35_string = "Creg        ";
      RegName_WdescReg : switch_Misc_l245_35_string = "WdescReg    ";
      RegName_IptrReg : switch_Misc_l245_35_string = "IptrReg     ";
      RegName_StatusReg : switch_Misc_l245_35_string = "StatusReg   ";
      RegName_ThReg : switch_Misc_l245_35_string = "ThReg       ";
      RegName_FPstatusReg : switch_Misc_l245_35_string = "FPstatusReg ";
      RegName_FPAreg : switch_Misc_l245_35_string = "FPAreg      ";
      RegName_FPBreg : switch_Misc_l245_35_string = "FPBreg      ";
      RegName_FPCreg : switch_Misc_l245_35_string = "FPCreg      ";
      RegName_BMreg0 : switch_Misc_l245_35_string = "BMreg0      ";
      RegName_BMreg1 : switch_Misc_l245_35_string = "BMreg1      ";
      RegName_BMreg2 : switch_Misc_l245_35_string = "BMreg2      ";
      RegName_WIReg : switch_Misc_l245_35_string = "WIReg       ";
      RegName_WuReg : switch_Misc_l245_35_string = "WuReg       ";
      RegName_Ereg : switch_Misc_l245_35_string = "Ereg        ";
      RegName_Xreg : switch_Misc_l245_35_string = "Xreg        ";
      RegName_EptrReg : switch_Misc_l245_35_string = "EptrReg     ";
      RegName_RegionReg0 : switch_Misc_l245_35_string = "RegionReg0  ";
      RegName_RegionReg1 : switch_Misc_l245_35_string = "RegionReg1  ";
      RegName_RegionReg2 : switch_Misc_l245_35_string = "RegionReg2  ";
      RegName_RegionReg3 : switch_Misc_l245_35_string = "RegionReg3  ";
      RegName_PstateReg : switch_Misc_l245_35_string = "PstateReg   ";
      RegName_WdescStubReg : switch_Misc_l245_35_string = "WdescStubReg";
      RegName_FptrReg0 : switch_Misc_l245_35_string = "FptrReg0    ";
      RegName_FptrReg1 : switch_Misc_l245_35_string = "FptrReg1    ";
      RegName_BptrReg0 : switch_Misc_l245_35_string = "BptrReg0    ";
      RegName_BptrReg1 : switch_Misc_l245_35_string = "BptrReg1    ";
      RegName_ClockReg0 : switch_Misc_l245_35_string = "ClockReg0   ";
      RegName_ClockReg1 : switch_Misc_l245_35_string = "ClockReg1   ";
      RegName_TptrReg0 : switch_Misc_l245_35_string = "TptrReg0    ";
      RegName_TptrReg1 : switch_Misc_l245_35_string = "TptrReg1    ";
      RegName_TnextReg0 : switch_Misc_l245_35_string = "TnextReg0   ";
      RegName_TnextReg1 : switch_Misc_l245_35_string = "TnextReg1   ";
      default : switch_Misc_l245_35_string = "????????????";
    endcase
  end
  always @(*) begin
    case(_zz_when_SchedulerPlugin_l168)
      ProcessState_RUNNING : _zz_when_SchedulerPlugin_l168_string = "RUNNING   ";
      ProcessState_READY : _zz_when_SchedulerPlugin_l168_string = "READY     ";
      ProcessState_WAITING : _zz_when_SchedulerPlugin_l168_string = "WAITING   ";
      ProcessState_TERMINATED : _zz_when_SchedulerPlugin_l168_string = "TERMINATED";
      default : _zz_when_SchedulerPlugin_l168_string = "??????????";
    endcase
  end
  `endif

  assign fetchGroup_down_isFiring = (fetchGroup_down_isValid && fetchGroup_down_isReady);
  assign fetchGroup_down_isValid = 1'b1;
  assign fetchGroup_down_isReady = 1'b1;
  assign localDecode_down_isFiring = (localDecode_down_isValid && localDecode_down_isReady);
  assign localDecode_down_isValid = 1'b1;
  assign localDecode_down_isReady = 1'b1;
  assign addressCache_down_isFiring = (addressCache_down_isValid && addressCache_down_isReady);
  assign addressCache_down_isValid = 1'b1;
  assign addressCache_down_isReady = 1'b1;
  assign execute_down_isFiring = (execute_down_isValid && execute_down_isReady);
  assign execute_down_isValid = 1'b1;
  assign execute_down_isReady = 1'b1;
  assign writeback_down_isFiring = (writeback_down_isValid && writeback_down_isReady);
  assign writeback_down_isValid = 1'b1;
  assign writeback_down_isReady = 1'b1;
  assign systemBus_cmd_valid = 1'b0;
  assign systemBus_cmd_payload_fragment_source = 4'b0000;
  assign systemBus_cmd_payload_fragment_opcode = 1'b0;
  assign systemBus_cmd_payload_fragment_address = 32'h0;
  assign systemBus_cmd_payload_fragment_length = 4'b0000;
  assign systemBus_cmd_payload_fragment_data = 128'h0;
  assign systemBus_cmd_payload_fragment_mask = 16'h0;
  assign systemBus_cmd_payload_last = 1'b1;
  assign systemBus_rsp_ready = 1'b1;
  assign _zz_io_input_cmd_valid_1 = (_zz_io_input_cmd_valid && (! _zz_io_input_cmd_valid_2));
  assign _zz_io_input_rsp_ready = _zz_io_input_cmd_valid;
  assign _zz_3 = _zz_2[3 : 0];
  always @(*) begin
    switch_ArithmeticPlugin_l160 = AluOp_ADD;
    if((_zz_3 == _zz_8)) begin
        switch_ArithmeticPlugin_l160 = AluOp_ADD;
    end else if((_zz_3 == _zz_10)) begin
        switch_ArithmeticPlugin_l160 = AluOp_SUB;
    end else if((_zz_3 == _zz_12)) begin
        switch_ArithmeticPlugin_l160 = AluOp_AND_1;
    end else if((_zz_3 == _zz_14)) begin
        switch_ArithmeticPlugin_l160 = AluOp_OR_1;
    end else if((_zz_3 == _zz_16)) begin
        switch_ArithmeticPlugin_l160 = AluOp_XOR_1;
    end else if((_zz_3 == _zz_18)) begin
        switch_ArithmeticPlugin_l160 = AluOp_NOT_1;
    end else if((_zz_3 == _zz_20)) begin
        switch_ArithmeticPlugin_l160 = AluOp_SHL;
    end else if((_zz_3 == _zz_22)) begin
        switch_ArithmeticPlugin_l160 = AluOp_SHR;
    end else if((_zz_3 == _zz_24)) begin
        switch_ArithmeticPlugin_l160 = AluOp_GT;
    end else if((_zz_3 == _zz_26)) begin
        switch_ArithmeticPlugin_l160 = AluOp_REV;
    end
  end

  assign switch_Misc_l245 = RegName_Areg;
  always @(*) begin
    case(switch_Misc_l245)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_9 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_9 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_9 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_9 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_9 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_9 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_9 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_10 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_9 & 32'hffffffff);
  assign switch_Misc_l245_1 = RegName_Breg;
  always @(*) begin
    case(switch_Misc_l245_1)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_11 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_11 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_11 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_11 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_11 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_11 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_11 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_12 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_11 & 32'hffffffff);
  assign switch_Misc_l245_2 = RegName_Creg;
  always @(*) begin
    case(switch_Misc_l245_2)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_13 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_13 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_13 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_13 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_13 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_13 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_13 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_14 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_13 & 32'hffffffff);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_15 = (_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_15 & 32'hffffffff);
  assign when_RegStackPlugin_l144 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_16 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_14 & 32'hffffffff);
  assign when_RegStackPlugin_l144_1 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_1 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_1 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_1 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_1 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_1 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_17 = (_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_17 & 32'hffffffff);
  assign when_RegStackPlugin_l144_2 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_2 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_2 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_2 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_2 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_2 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_18 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_14 & 32'hffffffff);
  assign when_RegStackPlugin_l144_3 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_3 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_3 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_3 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_3 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_3 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_19 = ((_zz_fpu_fpPipe_ctrl_0_down_RESULT_12 & _zz_fpu_fpPipe_ctrl_0_down_RESULT_10) & 32'hffffffff);
  assign when_RegStackPlugin_l144_4 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_4 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_4 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_4 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_4 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_4 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_20 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_14 & 32'hffffffff);
  assign when_RegStackPlugin_l144_5 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_5 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_5 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_5 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_5 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_5 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_21 = ((_zz_fpu_fpPipe_ctrl_0_down_RESULT_12 | _zz_fpu_fpPipe_ctrl_0_down_RESULT_10) & 32'hffffffff);
  assign when_RegStackPlugin_l144_6 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_6 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_6 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_6 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_6 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_6 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_22 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_14 & 32'hffffffff);
  assign when_RegStackPlugin_l144_7 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_7 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_7 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_7 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_7 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_7 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_23 = ((_zz_fpu_fpPipe_ctrl_0_down_RESULT_12 ^ _zz_fpu_fpPipe_ctrl_0_down_RESULT_10) & 32'hffffffff);
  assign when_RegStackPlugin_l144_8 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_8 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_8 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_8 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_8 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_8 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_24 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_14 & 32'hffffffff);
  assign when_RegStackPlugin_l144_9 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_9 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_9 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_9 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_9 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_9 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_25 = ((~ _zz_fpu_fpPipe_ctrl_0_down_RESULT_10) & 32'hffffffff);
  assign when_RegStackPlugin_l144_10 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_10 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_10 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_10 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_10 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_10 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_26 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_12 & 32'hffffffff);
  assign when_RegStackPlugin_l144_11 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_11 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_11 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_11 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_11 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_11 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_27 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_10 & 32'hffffffff);
  assign when_RegStackPlugin_l144_12 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_12 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_12 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_12 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_12 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_12 = (RegName_StatusReg == RegName_Breg);
  assign _zz_when_LongArithPlugin_l142_1 = (_zz_when_LongArithPlugin_l142[7 : 4] == PrimaryOpcode_OPR);
  assign _zz_when_LongArithPlugin_l142_2 = _zz_when_LongArithPlugin_l142[3 : 0];
  always @(*) begin
    switch_LongArithPlugin_l132 = LongArithOp_LADD;
    if(_zz_when_LongArithPlugin_l142_1) begin
      if((_zz_when_LongArithPlugin_l142_2 == _zz_28)) begin
          switch_LongArithPlugin_l132 = LongArithOp_LADD;
      end else if((_zz_when_LongArithPlugin_l142_2 == _zz_30)) begin
          switch_LongArithPlugin_l132 = LongArithOp_LSUB;
      end else if((_zz_when_LongArithPlugin_l142_2 == _zz_32)) begin
          switch_LongArithPlugin_l132 = LongArithOp_LMUL;
      end else if((_zz_when_LongArithPlugin_l142_2 == _zz_34)) begin
          switch_LongArithPlugin_l132 = LongArithOp_LDIV;
      end else if((_zz_when_LongArithPlugin_l142_2 == _zz_36)) begin
          switch_LongArithPlugin_l132 = LongArithOp_LSHL;
      end else if((_zz_when_LongArithPlugin_l142_2 == _zz_38)) begin
          switch_LongArithPlugin_l132 = LongArithOp_LSHR;
      end else if((_zz_when_LongArithPlugin_l142_2 == _zz_40)) begin
          switch_LongArithPlugin_l132 = LongArithOp_MINT;
      end else if((_zz_when_LongArithPlugin_l142_2 == _zz_42)) begin
          switch_LongArithPlugin_l132 = LongArithOp_XSWORD;
      end else if((_zz_when_LongArithPlugin_l142_2 == _zz_44)) begin
          switch_LongArithPlugin_l132 = LongArithOp_PROD;
      end
    end
  end

  always @(*) begin
    _zz_when_LongArithPlugin_l312 = 4'b0001;
    case(switch_LongArithPlugin_l132)
      LongArithOp_LADD, LongArithOp_LSUB : begin
        _zz_when_LongArithPlugin_l312 = 4'b0001;
      end
      LongArithOp_LSHL, LongArithOp_LSHR : begin
        _zz_when_LongArithPlugin_l312 = 4'b0001;
      end
      LongArithOp_LMUL : begin
        _zz_when_LongArithPlugin_l312 = 4'b0100;
      end
      LongArithOp_LDIV : begin
        _zz_when_LongArithPlugin_l312 = 4'b1000;
      end
      LongArithOp_MINT, LongArithOp_XSWORD : begin
        _zz_when_LongArithPlugin_l312 = 4'b0001;
      end
      LongArithOp_PROD : begin
        _zz_when_LongArithPlugin_l312 = 4'b0001;
      end
      default : begin
      end
    endcase
  end

  assign when_LongArithPlugin_l142 = ((_zz_when_LongArithPlugin_l142_1 && (((((((((_zz_when_LongArithPlugin_l142_2 == _zz_when_LongArithPlugin_l142_3) || (_zz_when_LongArithPlugin_l142_2 == _zz_when_LongArithPlugin_l142_5)) || (_zz_when_LongArithPlugin_l142_2 == _zz_when_LongArithPlugin_l142_7)) || (_zz_when_LongArithPlugin_l142_2 == _zz_when_LongArithPlugin_l142_9)) || (_zz_when_LongArithPlugin_l142_2 == _zz_when_LongArithPlugin_l142_11)) || (_zz_when_LongArithPlugin_l142_2 == _zz_when_LongArithPlugin_l142_13)) || (_zz_when_LongArithPlugin_l142_2 == _zz_when_LongArithPlugin_l142_15)) || (_zz_when_LongArithPlugin_l142_2 == _zz_when_LongArithPlugin_l142_17)) || (_zz_when_LongArithPlugin_l142_2 == _zz_when_LongArithPlugin_l142_19))) && (! when_LongArithPlugin_l312));
  assign switch_Misc_l245_3 = RegName_Areg;
  always @(*) begin
    case(switch_Misc_l245_3)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_28 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_28 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_28 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_28 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_28 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_28 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_28 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_29 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_28 & 32'hffffffff);
  assign switch_Misc_l245_4 = RegName_Breg;
  always @(*) begin
    case(switch_Misc_l245_4)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_30 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_30 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_30 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_30 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_30 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_30 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_30 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_31 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_30 & 32'hffffffff);
  assign switch_Misc_l245_5 = RegName_Creg;
  always @(*) begin
    case(switch_Misc_l245_5)
      RegName_Areg : begin
        _zz_when_LongArithPlugin_l220 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_when_LongArithPlugin_l220 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_when_LongArithPlugin_l220 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_when_LongArithPlugin_l220 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_when_LongArithPlugin_l220 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_when_LongArithPlugin_l220 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_when_LongArithPlugin_l220 = 32'h0;
      end
    endcase
  end

  assign _zz_when_LongArithPlugin_l220_1 = (_zz_when_LongArithPlugin_l220 & 32'hffffffff);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_32 = ({_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_32,_zz_fpu_fpPipe_ctrl_0_down_RESULT_29} + {33'h0,_zz_when_LongArithPlugin_l220_1});
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_33 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_32[31 : 0] & 32'hffffffff);
  assign when_RegStackPlugin_l144_13 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_13 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_13 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_13 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_13 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_13 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_34 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_32[63 : 32] & 32'hffffffff);
  assign when_RegStackPlugin_l144_14 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_14 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_14 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_14 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_14 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_14 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_35 = ($signed(_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35) - $signed(_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_35_4));
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_36 = (_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_36 & 32'hffffffff);
  assign when_RegStackPlugin_l144_15 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_15 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_15 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_15 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_15 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_15 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_37 = (_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_37 & 32'hffffffff);
  assign when_RegStackPlugin_l144_16 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_16 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_16 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_16 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_16 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_16 = (RegName_StatusReg == RegName_Breg);
  assign when_LongArithPlugin_l203 = (_zz_when_LongArithPlugin_l203 == 4'b0000);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_38 = (_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_38 * _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_38_1);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_39 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_38[31 : 0] & 32'hffffffff);
  assign when_RegStackPlugin_l144_17 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_17 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_17 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_17 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_17 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_17 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_40 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_38[63 : 32] & 32'hffffffff);
  assign when_RegStackPlugin_l144_18 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_18 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_18 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_18 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_18 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_18 = (RegName_StatusReg == RegName_Breg);
  assign when_LongArithPlugin_l220 = ((_zz_when_LongArithPlugin_l203 == 4'b0000) && (_zz_when_LongArithPlugin_l220_1 != 32'h0));
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_41 = {_zz_fpu_fpPipe_ctrl_0_down_RESULT_31,_zz_fpu_fpPipe_ctrl_0_down_RESULT_29};
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_42 = (_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_42[31 : 0] & 32'hffffffff);
  assign when_RegStackPlugin_l144_19 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_19 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_19 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_19 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_19 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_19 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_43 = (_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_43[31 : 0] & 32'hffffffff);
  assign when_RegStackPlugin_l144_20 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_20 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_20 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_20 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_20 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_20 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_44 = ({_zz_fpu_fpPipe_ctrl_0_down_RESULT_31,_zz_fpu_fpPipe_ctrl_0_down_RESULT_29} <<< _zz_when_LongArithPlugin_l220_1[5 : 0]);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_45 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_44[31 : 0] & 32'hffffffff);
  assign when_RegStackPlugin_l144_21 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_21 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_21 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_21 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_21 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_21 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_46 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_44[63 : 32] & 32'hffffffff);
  assign when_RegStackPlugin_l144_22 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_22 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_22 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_22 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_22 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_22 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_47 = ({_zz_fpu_fpPipe_ctrl_0_down_RESULT_31,_zz_fpu_fpPipe_ctrl_0_down_RESULT_29} >>> _zz_when_LongArithPlugin_l220_1[5 : 0]);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_48 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_47[31 : 0] & 32'hffffffff);
  assign when_RegStackPlugin_l144_23 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_23 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_23 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_23 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_23 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_23 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_49 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_47[63 : 32] & 32'hffffffff);
  assign when_RegStackPlugin_l144_24 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_24 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_24 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_24 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_24 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_24 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_50 = (_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_50 & 32'hffffffff);
  assign when_RegStackPlugin_l144_25 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_25 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_25 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_25 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_25 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_25 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_51 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_29 & 32'hffffffff);
  assign when_RegStackPlugin_l144_26 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_26 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_26 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_26 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_26 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_26 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_52 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_31 & 32'hffffffff);
  assign when_RegStackPlugin_l144_27 = (RegName_Areg == RegName_Creg);
  assign when_RegStackPlugin_l146_27 = (RegName_Breg == RegName_Creg);
  assign when_RegStackPlugin_l148_27 = (RegName_Creg == RegName_Creg);
  assign when_RegStackPlugin_l150_27 = (RegName_WdescReg == RegName_Creg);
  assign when_RegStackPlugin_l152_27 = (RegName_IptrReg == RegName_Creg);
  assign when_RegStackPlugin_l154_27 = (RegName_StatusReg == RegName_Creg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_53 = _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_53;
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_54 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_53[31 : 0] & 32'hffffffff);
  assign when_RegStackPlugin_l144_28 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_28 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_28 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_28 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_28 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_28 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_55 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_53[63 : 32] & 32'hffffffff);
  assign when_RegStackPlugin_l144_29 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_29 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_29 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_29 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_29 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_29 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_56 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_31 & 32'hffffffff);
  assign when_RegStackPlugin_l144_30 = (RegName_Areg == RegName_Creg);
  assign when_RegStackPlugin_l146_30 = (RegName_Breg == RegName_Creg);
  assign when_RegStackPlugin_l148_30 = (RegName_Creg == RegName_Creg);
  assign when_RegStackPlugin_l150_30 = (RegName_WdescReg == RegName_Creg);
  assign when_RegStackPlugin_l152_30 = (RegName_IptrReg == RegName_Creg);
  assign when_RegStackPlugin_l154_30 = (RegName_StatusReg == RegName_Creg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_57 = (_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_57 & 32'hffffffff);
  assign when_RegStackPlugin_l144_31 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_31 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_31 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_31 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_31 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_31 = (RegName_StatusReg == RegName_Areg);
  assign when_LongArithPlugin_l313 = (_zz_when_LongArithPlugin_l203 == 4'b0000);
  assign _zz_when_ControlFlowPlugin_l115_1 = (_zz_when_ControlFlowPlugin_l115[7 : 4] == PrimaryOpcode_OPR);
  assign _zz_when_ControlFlowPlugin_l115_2 = _zz_when_ControlFlowPlugin_l115[3 : 0];
  assign when_ControlFlowPlugin_l115 = (_zz_when_ControlFlowPlugin_l115_1 && (((((((_zz_when_ControlFlowPlugin_l115_2 == _zz_when_ControlFlowPlugin_l115_3) || (_zz_when_ControlFlowPlugin_l115_2 == _zz_when_ControlFlowPlugin_l115_5)) || (_zz_when_ControlFlowPlugin_l115_2 == _zz_when_ControlFlowPlugin_l115_7)) || (_zz_when_ControlFlowPlugin_l115_2 == _zz_when_ControlFlowPlugin_l115_9)) || (_zz_when_ControlFlowPlugin_l115_2 == _zz_when_ControlFlowPlugin_l115_11)) || (_zz_when_ControlFlowPlugin_l115_2 == _zz_when_ControlFlowPlugin_l115_13)) || (_zz_when_ControlFlowPlugin_l115_2 == _zz_when_ControlFlowPlugin_l115_15)));
  always @(*) begin
    switch_ControlFlowPlugin_l132 = ControlFlowOp_RET;
    if(_zz_when_ControlFlowPlugin_l115_1) begin
      if((_zz_when_ControlFlowPlugin_l115_2 == _zz_46)) begin
          switch_ControlFlowPlugin_l132 = ControlFlowOp_RET;
      end else if((_zz_when_ControlFlowPlugin_l115_2 == _zz_48)) begin
          switch_ControlFlowPlugin_l132 = ControlFlowOp_LDPI;
      end else if((_zz_when_ControlFlowPlugin_l115_2 == _zz_50)) begin
          switch_ControlFlowPlugin_l132 = ControlFlowOp_GAJW;
      end else if((_zz_when_ControlFlowPlugin_l115_2 == _zz_52)) begin
          switch_ControlFlowPlugin_l132 = ControlFlowOp_GCALL;
      end else if((_zz_when_ControlFlowPlugin_l115_2 == _zz_54)) begin
          switch_ControlFlowPlugin_l132 = ControlFlowOp_LEND;
      end else if((_zz_when_ControlFlowPlugin_l115_2 == _zz_56)) begin
          switch_ControlFlowPlugin_l132 = ControlFlowOp_ENDP;
      end else if((_zz_when_ControlFlowPlugin_l115_2 == _zz_58)) begin
          switch_ControlFlowPlugin_l132 = ControlFlowOp_DISS;
      end
    end
  end

  assign switch_Misc_l245_6 = RegName_Areg;
  always @(*) begin
    case(switch_Misc_l245_6)
      RegName_Areg : begin
        _zz_when_ControlFlowPlugin_l171 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_when_ControlFlowPlugin_l171 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_when_ControlFlowPlugin_l171 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_when_ControlFlowPlugin_l171 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_when_ControlFlowPlugin_l171 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_when_ControlFlowPlugin_l171 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_when_ControlFlowPlugin_l171 = 32'h0;
      end
    endcase
  end

  assign _zz_when_ControlFlowPlugin_l171_1 = (_zz_when_ControlFlowPlugin_l171 & 32'hffffffff);
  assign switch_Misc_l245_7 = RegName_Breg;
  always @(*) begin
    case(switch_Misc_l245_7)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_58 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_58 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_58 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_58 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_58 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_58 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_58 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_59 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_58 & 32'hffffffff);
  assign switch_Misc_l245_8 = RegName_Creg;
  always @(*) begin
    case(switch_Misc_l245_8)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_60 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_60 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_60 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_60 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_60 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_60 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_60 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_61 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_60 & 32'hffffffff);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_62 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_59 & 32'hffffffff);
  assign when_RegStackPlugin_l144_32 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_32 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_32 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_32 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_32 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_32 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_63 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_61 & 32'hffffffff);
  assign when_RegStackPlugin_l144_33 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_33 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_33 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_33 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_33 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_33 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_64 = (_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_64 & 32'hffffffff);
  assign when_RegStackPlugin_l144_34 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_34 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_34 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_34 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_34 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_34 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_65 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_59 & 32'hffffffff);
  assign when_RegStackPlugin_l144_35 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_35 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_35 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_35 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_35 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_35 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_66 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_61 & 32'hffffffff);
  assign when_RegStackPlugin_l144_36 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_36 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_36 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_36 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_36 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_36 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_67 = (_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_67 & 32'hffffffff);
  assign when_RegStackPlugin_l144_37 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_37 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_37 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_37 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_37 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_37 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_68 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_61 & 32'hffffffff);
  assign when_RegStackPlugin_l144_38 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_38 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_38 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_38 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_38 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_38 = (RegName_StatusReg == RegName_Breg);
  assign when_ControlFlowPlugin_l171 = (_zz_when_ControlFlowPlugin_l171_1 != 32'h0);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_69 = (_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_69 & 32'hffffffff);
  assign when_RegStackPlugin_l144_39 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_39 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_39 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_39 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_39 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_39 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_70 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_61 & 32'hffffffff);
  assign when_RegStackPlugin_l144_40 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_40 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_40 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_40 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_40 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_40 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_71 = (32'h0 & 32'hffffffff);
  assign when_RegStackPlugin_l144_41 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_41 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_41 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_41 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_41 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_41 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_72 = (32'h0 & 32'hffffffff);
  assign when_RegStackPlugin_l144_42 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_42 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_42 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_42 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_42 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_42 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_73 = (32'h0 & 32'hffffffff);
  assign when_RegStackPlugin_l144_43 = (RegName_Areg == RegName_Creg);
  assign when_RegStackPlugin_l146_43 = (RegName_Breg == RegName_Creg);
  assign when_RegStackPlugin_l148_43 = (RegName_Creg == RegName_Creg);
  assign when_RegStackPlugin_l150_43 = (RegName_WdescReg == RegName_Creg);
  assign when_RegStackPlugin_l152_43 = (RegName_IptrReg == RegName_Creg);
  assign when_RegStackPlugin_l154_43 = (RegName_StatusReg == RegName_Creg);
  assign _zz_when_BlockMovePlugin_l113_1 = (_zz_when_BlockMovePlugin_l113[7 : 4] == PrimaryOpcode_OPR);
  assign _zz_when_BlockMovePlugin_l113_2 = _zz_when_BlockMovePlugin_l113[3 : 0];
  always @(*) begin
    switch_BlockMovePlugin_l118 = BlockMoveOp_MOVE;
    if(_zz_when_BlockMovePlugin_l113_1) begin
      if((_zz_when_BlockMovePlugin_l113_2 == 4'b1010)) begin
          switch_BlockMovePlugin_l118 = BlockMoveOp_MOVE;
      end else if((_zz_when_BlockMovePlugin_l113_2 == 4'b1011)) begin
          switch_BlockMovePlugin_l118 = BlockMoveOp_MOVE2DINIT;
      end else if((_zz_when_BlockMovePlugin_l113_2 == 4'b1100)) begin
          switch_BlockMovePlugin_l118 = BlockMoveOp_MOVE2DALL;
      end else if((_zz_when_BlockMovePlugin_l113_2 == 4'b1101)) begin
          switch_BlockMovePlugin_l118 = BlockMoveOp_MOVE2DNONZERO;
      end else if((_zz_when_BlockMovePlugin_l113_2 == 4'b1110)) begin
          switch_BlockMovePlugin_l118 = BlockMoveOp_MOVE2DZERO;
      end
    end
  end

  assign when_BlockMovePlugin_l113 = ((_zz_when_BlockMovePlugin_l113_1 && (((((_zz_when_BlockMovePlugin_l113_2 == 4'b1010) || (_zz_when_BlockMovePlugin_l113_2 == 4'b1011)) || (_zz_when_BlockMovePlugin_l113_2 == 4'b1100)) || (_zz_when_BlockMovePlugin_l113_2 == 4'b1101)) || (_zz_when_BlockMovePlugin_l113_2 == 4'b1110))) && (! when_BlockMovePlugin_l165));
  assign switch_Misc_l245_9 = RegName_Areg;
  always @(*) begin
    case(switch_Misc_l245_9)
      RegName_Areg : begin
        _zz_when_BlockMovePlugin_l165_3 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_when_BlockMovePlugin_l165_3 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_when_BlockMovePlugin_l165_3 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_when_BlockMovePlugin_l165_3 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_when_BlockMovePlugin_l165_3 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_when_BlockMovePlugin_l165_3 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_when_BlockMovePlugin_l165_3 = 32'h0;
      end
    endcase
  end

  assign switch_Misc_l245_10 = RegName_Breg;
  always @(*) begin
    case(switch_Misc_l245_10)
      RegName_Areg : begin
        _zz_when_BlockMovePlugin_l165_4 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_when_BlockMovePlugin_l165_4 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_when_BlockMovePlugin_l165_4 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_when_BlockMovePlugin_l165_4 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_when_BlockMovePlugin_l165_4 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_when_BlockMovePlugin_l165_4 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_when_BlockMovePlugin_l165_4 = 32'h0;
      end
    endcase
  end

  assign switch_Misc_l245_11 = RegName_Creg;
  always @(*) begin
    case(switch_Misc_l245_11)
      RegName_Areg : begin
        _zz_when_BlockMovePlugin_l165_5 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_when_BlockMovePlugin_l165_5 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_when_BlockMovePlugin_l165_5 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_when_BlockMovePlugin_l165_5 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_when_BlockMovePlugin_l165_5 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_when_BlockMovePlugin_l165_5 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_when_BlockMovePlugin_l165_5 = 32'h0;
      end
    endcase
  end

  assign _zz_when_BlockMovePlugin_l165_6 = (_zz_when_BlockMovePlugin_l165_5 & 32'hffffffff);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_74 = (32'h0 & 32'hffffffff);
  assign when_RegStackPlugin_l144_44 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_44 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_44 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_44 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_44 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_44 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_75 = (32'h0 & 32'hffffffff);
  assign when_RegStackPlugin_l144_45 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_45 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_45 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_45 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_45 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_45 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_76 = (32'h0 & 32'hffffffff);
  assign when_RegStackPlugin_l144_46 = (RegName_Areg == RegName_Creg);
  assign when_RegStackPlugin_l146_46 = (RegName_Breg == RegName_Creg);
  assign when_RegStackPlugin_l148_46 = (RegName_Creg == RegName_Creg);
  assign when_RegStackPlugin_l150_46 = (RegName_WdescReg == RegName_Creg);
  assign when_RegStackPlugin_l152_46 = (RegName_IptrReg == RegName_Creg);
  assign when_RegStackPlugin_l154_46 = (RegName_StatusReg == RegName_Creg);
  assign when_BlockMovePlugin_l171 = (32'h00000004 <= _zz_when_BlockMovePlugin_l165);
  assign when_BlockMovePlugin_l187 = (32'h0 < _zz_when_BlockMovePlugin_l165);
  assign _zz_when_IndexingPlugin_l135_1 = (_zz_when_IndexingPlugin_l135[7 : 4] != PrimaryOpcode_OPR);
  assign _zz_when_IndexingPlugin_l135_2 = (_zz_when_IndexingPlugin_l135[7 : 4] == PrimaryOpcode_OPR);
  assign _zz_when_IndexingPlugin_l135_3 = _zz_when_IndexingPlugin_l135[3 : 0];
  assign _zz_when_IndexingPlugin_l135_4 = _zz_when_IndexingPlugin_l135[7 : 4];
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_78 = _zz_when_IndexingPlugin_l135[3 : 0];
  assign when_IndexingPlugin_l135 = ((_zz_when_IndexingPlugin_l135_1 && (((((_zz_when_IndexingPlugin_l135_5 || _zz_when_IndexingPlugin_l135_6) || (_zz_when_IndexingPlugin_l135_4 == _zz_when_IndexingPlugin_l135_7)) || (_zz_when_IndexingPlugin_l135_4 == _zz_when_IndexingPlugin_l135_8)) || (_zz_when_IndexingPlugin_l135_4 == PrimaryOpcode_LDLP)) || (_zz_when_IndexingPlugin_l135_4 == PrimaryOpcode_LDNLP))) || (_zz_when_IndexingPlugin_l135_2 && (((((_zz_when_IndexingPlugin_l135_9 || _zz_when_IndexingPlugin_l135_12) || (_zz_when_IndexingPlugin_l135_3 == _zz_when_IndexingPlugin_l135_15)) || (_zz_when_IndexingPlugin_l135_3 == _zz_when_IndexingPlugin_l135_17)) || (_zz_when_IndexingPlugin_l135_3 == _zz_when_IndexingPlugin_l135_19)) || (_zz_when_IndexingPlugin_l135_3 == _zz_when_IndexingPlugin_l135_21))));
  always @(*) begin
    switch_IndexingPlugin_l155 = IndexingOp_LDL;
    if(_zz_when_IndexingPlugin_l135_1) begin
      if((_zz_when_IndexingPlugin_l135_4 == PrimaryOpcode_LDL)) begin
          switch_IndexingPlugin_l155 = IndexingOp_LDL;
      end else if((_zz_when_IndexingPlugin_l135_4 == PrimaryOpcode_STL)) begin
          switch_IndexingPlugin_l155 = IndexingOp_STL;
      end else if((_zz_when_IndexingPlugin_l135_4 == PrimaryOpcode_LDNL)) begin
          switch_IndexingPlugin_l155 = IndexingOp_LDNL;
      end else if((_zz_when_IndexingPlugin_l135_4 == PrimaryOpcode_STNL)) begin
          switch_IndexingPlugin_l155 = IndexingOp_STNL;
      end else if((_zz_when_IndexingPlugin_l135_4 == PrimaryOpcode_LDLP)) begin
          switch_IndexingPlugin_l155 = IndexingOp_LDLP;
      end else if((_zz_when_IndexingPlugin_l135_4 == PrimaryOpcode_LDNLP)) begin
          switch_IndexingPlugin_l155 = IndexingOp_LDNLP;
      end
    end else begin
      if(_zz_when_IndexingPlugin_l135_2) begin
        if((_zz_when_IndexingPlugin_l135_3 == _zz_60)) begin
            switch_IndexingPlugin_l155 = IndexingOp_BSUB;
        end else if((_zz_when_IndexingPlugin_l135_3 == _zz_62)) begin
            switch_IndexingPlugin_l155 = IndexingOp_WSUB;
        end else if((_zz_when_IndexingPlugin_l135_3 == _zz_64)) begin
            switch_IndexingPlugin_l155 = IndexingOp_LB;
        end else if((_zz_when_IndexingPlugin_l135_3 == _zz_66)) begin
            switch_IndexingPlugin_l155 = IndexingOp_SB;
        end else if((_zz_when_IndexingPlugin_l135_3 == _zz_68)) begin
            switch_IndexingPlugin_l155 = IndexingOp_LSX;
        end else if((_zz_when_IndexingPlugin_l135_3 == _zz_70)) begin
            switch_IndexingPlugin_l155 = IndexingOp_SS;
        end
      end
    end
  end

  always @(*) begin
    _zz_fpu_fpPipe_ctrl_0_down_RESULT_77 = 32'h0;
    if(when_IndexingPlugin_l135) begin
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_77 = 32'h0;
      case(switch_IndexingPlugin_l155)
        IndexingOp_LDL : begin
          _zz_fpu_fpPipe_ctrl_0_down_RESULT_77 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_86 + {_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77,2'b00});
        end
        IndexingOp_STL : begin
          _zz_fpu_fpPipe_ctrl_0_down_RESULT_77 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_86 + {_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_1,2'b00});
        end
        IndexingOp_LDNL : begin
          _zz_fpu_fpPipe_ctrl_0_down_RESULT_77 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_80 + {_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_2,2'b00});
        end
        IndexingOp_STNL : begin
          _zz_fpu_fpPipe_ctrl_0_down_RESULT_77 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_82 + {_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_3,2'b00});
        end
        IndexingOp_LDLP : begin
          _zz_fpu_fpPipe_ctrl_0_down_RESULT_77 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_86 + {_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_4,2'b00});
        end
        IndexingOp_LDNLP : begin
          _zz_fpu_fpPipe_ctrl_0_down_RESULT_77 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_80 + {_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_5,2'b00});
        end
        IndexingOp_BSUB : begin
          _zz_fpu_fpPipe_ctrl_0_down_RESULT_77 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_82 + _zz_fpu_fpPipe_ctrl_0_down_RESULT_80);
        end
        IndexingOp_WSUB : begin
          _zz_fpu_fpPipe_ctrl_0_down_RESULT_77 = _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_77_6[31:0];
        end
        IndexingOp_LB : begin
          _zz_fpu_fpPipe_ctrl_0_down_RESULT_77 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_80;
        end
        IndexingOp_SB : begin
          _zz_fpu_fpPipe_ctrl_0_down_RESULT_77 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_80;
        end
        IndexingOp_LSX : begin
          _zz_fpu_fpPipe_ctrl_0_down_RESULT_77 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_80;
        end
        IndexingOp_SS : begin
          _zz_fpu_fpPipe_ctrl_0_down_RESULT_77 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_80;
        end
        default : begin
        end
      endcase
    end
  end

  assign switch_Misc_l245_12 = RegName_Areg;
  always @(*) begin
    case(switch_Misc_l245_12)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_79 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_79 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_79 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_79 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_79 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_79 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_79 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_80 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_79 & 32'hffffffff);
  assign switch_Misc_l245_13 = RegName_Breg;
  always @(*) begin
    case(switch_Misc_l245_13)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_81 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_81 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_81 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_81 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_81 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_81 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_81 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_82 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_81 & 32'hffffffff);
  assign switch_Misc_l245_14 = RegName_Creg;
  always @(*) begin
    case(switch_Misc_l245_14)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_83 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_83 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_83 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_83 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_83 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_83 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_83 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_84 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_83 & 32'hffffffff);
  assign switch_Misc_l245_15 = RegName_WdescReg;
  always @(*) begin
    case(switch_Misc_l245_15)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_85 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_85 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_85 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_85 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_85 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_85 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_85 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_86 = {_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_86[31 : 2],2'b00};
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_87 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_77 & 32'hffffffff);
  assign when_RegStackPlugin_l144_47 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_47 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_47 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_47 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_47 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_47 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_88 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_80 & 32'hffffffff);
  assign when_RegStackPlugin_l144_48 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_48 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_48 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_48 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_48 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_48 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_89 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_82 & 32'hffffffff);
  assign when_RegStackPlugin_l144_49 = (RegName_Areg == RegName_Creg);
  assign when_RegStackPlugin_l146_49 = (RegName_Breg == RegName_Creg);
  assign when_RegStackPlugin_l148_49 = (RegName_Creg == RegName_Creg);
  assign when_RegStackPlugin_l150_49 = (RegName_WdescReg == RegName_Creg);
  assign when_RegStackPlugin_l152_49 = (RegName_IptrReg == RegName_Creg);
  assign when_RegStackPlugin_l154_49 = (RegName_StatusReg == RegName_Creg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_90 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_77 & 32'hffffffff);
  assign when_RegStackPlugin_l144_50 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_50 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_50 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_50 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_50 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_50 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_91 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_77 & 32'hffffffff);
  assign when_RegStackPlugin_l144_51 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_51 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_51 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_51 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_51 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_51 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_92 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_84 & 32'hffffffff);
  assign when_RegStackPlugin_l144_52 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_52 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_52 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_52 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_52 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_52 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_93 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_77 & 32'hffffffff);
  assign when_RegStackPlugin_l144_53 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_53 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_53 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_53 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_53 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_53 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_94 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_84 & 32'hffffffff);
  assign when_RegStackPlugin_l144_54 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_54 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_54 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_54 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_54 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_54 = (RegName_StatusReg == RegName_Breg);
  assign _zz_when_RangeCheckPlugin_l124_1 = (_zz_when_RangeCheckPlugin_l124[7 : 4] != PrimaryOpcode_OPR);
  assign _zz_when_RangeCheckPlugin_l124_2 = (_zz_when_RangeCheckPlugin_l124[7 : 4] == PrimaryOpcode_OPR);
  assign _zz_when_RangeCheckPlugin_l124_3 = _zz_when_RangeCheckPlugin_l124[3 : 0];
  assign _zz_when_RangeCheckPlugin_l124_4 = _zz_when_RangeCheckPlugin_l124[7 : 4];
  assign when_RangeCheckPlugin_l124 = ((_zz_when_RangeCheckPlugin_l124_1 && ((_zz_when_RangeCheckPlugin_l124_4 == 4'b1010) || (_zz_when_RangeCheckPlugin_l124_4 == 4'b1001))) || (_zz_when_RangeCheckPlugin_l124_2 && ((((((_zz_when_RangeCheckPlugin_l124_5 || _zz_when_RangeCheckPlugin_l124_6) || (_zz_when_RangeCheckPlugin_l124_3 == _zz_when_RangeCheckPlugin_l124_7)) || (_zz_when_RangeCheckPlugin_l124_3 == _zz_when_RangeCheckPlugin_l124_8)) || (_zz_when_RangeCheckPlugin_l124_3 == 4'b1101)) || (_zz_when_RangeCheckPlugin_l124_3 == 4'b1100)) || (_zz_when_RangeCheckPlugin_l124_3 == 4'b1111))));
  always @(*) begin
    switch_RangeCheckPlugin_l137 = RangeCheckOp_CIR;
    if(_zz_when_RangeCheckPlugin_l124_1) begin
      if((_zz_when_RangeCheckPlugin_l124_4 == 4'b1010)) begin
          switch_RangeCheckPlugin_l137 = RangeCheckOp_CJ;
      end else if((_zz_when_RangeCheckPlugin_l124_4 == 4'b1001)) begin
          switch_RangeCheckPlugin_l137 = RangeCheckOp_CALL;
      end
    end else begin
      if(_zz_when_RangeCheckPlugin_l124_2) begin
        if((_zz_when_RangeCheckPlugin_l124_3 == 4'b0111)) begin
            switch_RangeCheckPlugin_l137 = RangeCheckOp_CIR;
        end else if((_zz_when_RangeCheckPlugin_l124_3 == 4'b1010)) begin
            switch_RangeCheckPlugin_l137 = RangeCheckOp_CB;
        end else if((_zz_when_RangeCheckPlugin_l124_3 == 4'b1010)) begin
            switch_RangeCheckPlugin_l137 = RangeCheckOp_CS;
        end else if((_zz_when_RangeCheckPlugin_l124_3 == 4'b0110)) begin
            switch_RangeCheckPlugin_l137 = RangeCheckOp_CWORD;
        end else if((_zz_when_RangeCheckPlugin_l124_3 == 4'b1000)) begin
            switch_RangeCheckPlugin_l137 = RangeCheckOp_XSWORD;
        end else if((_zz_when_RangeCheckPlugin_l124_3 == 4'b1101)) begin
            switch_RangeCheckPlugin_l137 = RangeCheckOp_CCNT1;
        end else if((_zz_when_RangeCheckPlugin_l124_3 == 4'b1100)) begin
            switch_RangeCheckPlugin_l137 = RangeCheckOp_CSNGL;
        end else if((_zz_when_RangeCheckPlugin_l124_3 == 4'b1111)) begin
            switch_RangeCheckPlugin_l137 = RangeCheckOp_CDBL;
        end
      end
    end
  end

  assign switch_Misc_l245_16 = RegName_Areg;
  always @(*) begin
    case(switch_Misc_l245_16)
      RegName_Areg : begin
        _zz_when_RangeCheckPlugin_l191 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_when_RangeCheckPlugin_l191 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_when_RangeCheckPlugin_l191 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_when_RangeCheckPlugin_l191 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_when_RangeCheckPlugin_l191 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_when_RangeCheckPlugin_l191 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_when_RangeCheckPlugin_l191 = 32'h0;
      end
    endcase
  end

  assign _zz_when_RangeCheckPlugin_l191_1 = (_zz_when_RangeCheckPlugin_l191 & 32'hffffffff);
  assign switch_Misc_l245_17 = RegName_Breg;
  always @(*) begin
    case(switch_Misc_l245_17)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_95 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_95 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_95 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_95 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_95 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_95 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_95 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_96 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_95 & 32'hffffffff);
  assign switch_Misc_l245_18 = RegName_Creg;
  always @(*) begin
    case(switch_Misc_l245_18)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_97 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_97 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_97 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_97 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_97 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_97 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_97 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_98 = (_zz_when_RangeCheckPlugin_l191_1 & 32'hffffffff);
  assign when_RegStackPlugin_l144_55 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_55 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_55 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_55 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_55 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_55 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_99 = (32'h0 & 32'hffffffff);
  assign when_RegStackPlugin_l144_56 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_56 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_56 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_56 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_56 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_56 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_100 = (32'h0 & 32'hffffffff);
  assign when_RegStackPlugin_l144_57 = (RegName_Areg == RegName_Creg);
  assign when_RegStackPlugin_l146_57 = (RegName_Breg == RegName_Creg);
  assign when_RegStackPlugin_l148_57 = (RegName_Creg == RegName_Creg);
  assign when_RegStackPlugin_l150_57 = (RegName_WdescReg == RegName_Creg);
  assign when_RegStackPlugin_l152_57 = (RegName_IptrReg == RegName_Creg);
  assign when_RegStackPlugin_l154_57 = (RegName_StatusReg == RegName_Creg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_101 = ((_zz_when_RangeCheckPlugin_l191_1[31] ? 32'hffffffff : 32'h0) & 32'hffffffff);
  assign when_RegStackPlugin_l144_58 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_58 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_58 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_58 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_58 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_58 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_102 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_96 & 32'hffffffff);
  assign when_RegStackPlugin_l144_59 = (RegName_Areg == RegName_Creg);
  assign when_RegStackPlugin_l146_59 = (RegName_Breg == RegName_Creg);
  assign when_RegStackPlugin_l148_59 = (RegName_Creg == RegName_Creg);
  assign when_RegStackPlugin_l150_59 = (RegName_WdescReg == RegName_Creg);
  assign when_RegStackPlugin_l152_59 = (RegName_IptrReg == RegName_Creg);
  assign when_RegStackPlugin_l154_59 = (RegName_StatusReg == RegName_Creg);
  assign when_RangeCheckPlugin_l191 = (32'h00000001 <= _zz_when_RangeCheckPlugin_l191_1);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_103 = (_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_103 & 32'hffffffff);
  assign when_RegStackPlugin_l144_60 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_60 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_60 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_60 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_60 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_60 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_104 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_96 & 32'hffffffff);
  assign when_RegStackPlugin_l144_61 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_61 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_61 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_61 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_61 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_61 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_105 = ((_zz_fpu_fpPipe_ctrl_0_down_RESULT_97 & 32'hffffffff) & 32'hffffffff);
  assign when_RegStackPlugin_l144_62 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_62 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_62 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_62 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_62 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_62 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_106 = (_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_106 & 32'hffffffff);
  assign when_RegStackPlugin_l144_63 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_63 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_63 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_63 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_63 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_63 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_107 = (_zz_when_RangeCheckPlugin_l191_1 & 32'hffffffff);
  assign when_RegStackPlugin_l144_64 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_64 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_64 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_64 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_64 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_64 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_108 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_96 & 32'hffffffff);
  assign when_RegStackPlugin_l144_65 = (RegName_Areg == RegName_Creg);
  assign when_RegStackPlugin_l146_65 = (RegName_Breg == RegName_Creg);
  assign when_RegStackPlugin_l148_65 = (RegName_Creg == RegName_Creg);
  assign when_RegStackPlugin_l150_65 = (RegName_WdescReg == RegName_Creg);
  assign when_RegStackPlugin_l152_65 = (RegName_IptrReg == RegName_Creg);
  assign when_RegStackPlugin_l154_65 = (RegName_StatusReg == RegName_Creg);
  assign _zz_when_GeneralPlugin_l69_1 = _zz_when_GeneralPlugin_l69[3 : 0];
  assign when_GeneralPlugin_l69 = ((_zz_when_GeneralPlugin_l69[7 : 4] == PrimaryOpcode_OPR) && (((((_zz_when_GeneralPlugin_l69_1 == _zz_when_GeneralPlugin_l69_2) || (_zz_when_GeneralPlugin_l69_1 == _zz_when_GeneralPlugin_l69_4)) || (_zz_when_GeneralPlugin_l69_1 == _zz_when_GeneralPlugin_l69_6)) || (_zz_when_GeneralPlugin_l69_1 == _zz_when_GeneralPlugin_l69_8)) || (_zz_when_GeneralPlugin_l69_1 == _zz_when_GeneralPlugin_l69_10)));
  assign switch_Misc_l245_19 = RegName_Areg;
  always @(*) begin
    case(switch_Misc_l245_19)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_109 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_109 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_109 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_109 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_109 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_109 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_109 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_110 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_109 & 32'hffffffff);
  assign switch_Misc_l245_20 = RegName_Breg;
  always @(*) begin
    case(switch_Misc_l245_20)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_111 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_111 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_111 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_111 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_111 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_111 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_111 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_112 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_111 & 32'hffffffff);
  assign switch_Misc_l245_21 = RegName_Creg;
  always @(*) begin
    case(switch_Misc_l245_21)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_113 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_113 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_113 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_113 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_113 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_113 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_113 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_114 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_112 & 32'hffffffff);
  assign when_RegStackPlugin_l144_66 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_66 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_66 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_66 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_66 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_66 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_115 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_110 & 32'hffffffff);
  assign when_RegStackPlugin_l144_67 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_67 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_67 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_67 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_67 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_67 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_116 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_110 & 32'hffffffff);
  assign when_RegStackPlugin_l144_68 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_68 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_68 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_68 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_68 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_68 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_117 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_110 & 32'hffffffff);
  assign when_RegStackPlugin_l144_69 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_69 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_69 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_69 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_69 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_69 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_118 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_112 & 32'hffffffff);
  assign when_RegStackPlugin_l144_70 = (RegName_Areg == RegName_Creg);
  assign when_RegStackPlugin_l146_70 = (RegName_Breg == RegName_Creg);
  assign when_RegStackPlugin_l148_70 = (RegName_Creg == RegName_Creg);
  assign when_RegStackPlugin_l150_70 = (RegName_WdescReg == RegName_Creg);
  assign when_RegStackPlugin_l152_70 = (RegName_IptrReg == RegName_Creg);
  assign when_RegStackPlugin_l154_70 = (RegName_StatusReg == RegName_Creg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_119 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_112 & 32'hffffffff);
  assign when_RegStackPlugin_l144_71 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_71 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_71 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_71 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_71 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_71 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_120 = ((_zz_fpu_fpPipe_ctrl_0_down_RESULT_113 & 32'hffffffff) & 32'hffffffff);
  assign when_RegStackPlugin_l144_72 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_72 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_72 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_72 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_72 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_72 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_121 = (_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_121 & 32'hffffffff);
  assign when_RegStackPlugin_l144_73 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_73 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_73 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_73 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_73 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_73 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_122 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_110 & 32'hffffffff);
  assign when_RegStackPlugin_l144_74 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_74 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_74 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_74 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_74 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_74 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_123 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_112 & 32'hffffffff);
  assign when_RegStackPlugin_l144_75 = (RegName_Areg == RegName_Creg);
  assign when_RegStackPlugin_l146_75 = (RegName_Breg == RegName_Creg);
  assign when_RegStackPlugin_l148_75 = (RegName_Creg == RegName_Creg);
  assign when_RegStackPlugin_l150_75 = (RegName_WdescReg == RegName_Creg);
  assign when_RegStackPlugin_l152_75 = (RegName_IptrReg == RegName_Creg);
  assign when_RegStackPlugin_l154_75 = (RegName_StatusReg == RegName_Creg);
  always @(*) begin
    _zz_fpu_fpPipe_ctrl_0_down_RESULT_124 = ChannelType_INVALID;
    if(when_ChannelPlugin_l111) begin
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_124 = ChannelType_INVALID;
      case(switch_ChannelPlugin_l121)
        ChannelOp_CHANTYPE : begin
          if(when_ChannelPlugin_l127) begin
            _zz_fpu_fpPipe_ctrl_0_down_RESULT_124 = ChannelType_RESOURCE;
          end else begin
            if(when_ChannelPlugin_l129) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_124 = ChannelType_VIRTUAL_1;
            end else begin
              if(when_ChannelPlugin_l131) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_124 = ChannelType_PHYSICAL;
              end else begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_124 = ChannelType_INVALID;
              end
            end
          end
        end
        ChannelOp_INITVLCB : begin
        end
        ChannelOp_SETCHMODE : begin
        end
        ChannelOp_SETHDR : begin
        end
        ChannelOp_READHDR : begin
        end
        ChannelOp_MKRC : begin
        end
        ChannelOp_UNMKRC : begin
        end
        default : begin
        end
      endcase
    end
    if(when_ChannelPlugin_l222) begin
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_124 = ChannelType_INVALID;
    end
  end

  assign _zz_when_ChannelPlugin_l111_1 = (_zz_when_ChannelPlugin_l111[7 : 4] == PrimaryOpcode_OPR);
  assign _zz_when_ChannelPlugin_l111_2 = _zz_when_ChannelPlugin_l111[3 : 0];
  assign when_ChannelPlugin_l111 = ((_zz_when_ChannelPlugin_l111_1 && (_zz_when_ChannelPlugin_l111_2 == 4'b1001)) || (1'b0 && _zz_when_ChannelPlugin_l111_1));
  always @(*) begin
    switch_ChannelPlugin_l121 = ChannelOp_CHANTYPE;
    if(_zz_when_ChannelPlugin_l111_1) begin
      if(when_ChannelPlugin_l102) begin
        switch_ChannelPlugin_l121 = ChannelOp_CHANTYPE;
      end else begin
        if(1'b0) begin
          switch_ChannelPlugin_l121 = ChannelOp_INITVLCB;
        end
      end
    end
  end

  assign when_ChannelPlugin_l102 = (_zz_when_ChannelPlugin_l111_2 == 4'b1001);
  assign switch_Misc_l245_22 = RegName_Areg;
  always @(*) begin
    case(switch_Misc_l245_22)
      RegName_Areg : begin
        _zz_when_ChannelPlugin_l127_1 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_when_ChannelPlugin_l127_1 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_when_ChannelPlugin_l127_1 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_when_ChannelPlugin_l127_1 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_when_ChannelPlugin_l127_1 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_when_ChannelPlugin_l127_1 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_when_ChannelPlugin_l127_1 = 32'h0;
      end
    endcase
  end

  assign _zz_when_ChannelPlugin_l127_2 = (_zz_when_ChannelPlugin_l127_1 & 32'hffffffff);
  assign switch_Misc_l245_23 = RegName_Breg;
  always @(*) begin
    case(switch_Misc_l245_23)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_141 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_141 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_141 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_141 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_141 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_141 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_141 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_142 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_141 & 32'hffffffff);
  assign switch_Misc_l245_24 = RegName_Creg;
  always @(*) begin
    case(switch_Misc_l245_24)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_143 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_143 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_143 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_143 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_143 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_143 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_143 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_144 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_143 & 32'hffffffff);
  assign _zz_when_ChannelPlugin_l127_3 = _zz_when_ChannelPlugin_l127_2[3 : 0];
  assign when_ChannelPlugin_l127 = _zz_when_ChannelPlugin_l127[_zz_when_ChannelPlugin_l127_3];
  assign _zz_5 = ({15'd0,1'b1} <<< _zz_when_ChannelPlugin_l127_3);
  assign when_ChannelPlugin_l129 = (1'b1 && _zz_when_ChannelPlugin_l129_17);
  assign when_ChannelPlugin_l131 = (_zz_when_ChannelPlugin_l127_3 < 4'b0100);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_145 = (_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_145 & 32'hffffffff);
  assign when_RegStackPlugin_l144_76 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_76 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_76 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_76 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_76 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_76 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_146 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_142 & 32'hffffffff);
  assign when_RegStackPlugin_l144_77 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_77 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_77 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_77 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_77 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_77 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_147 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_144 & 32'hffffffff);
  assign when_RegStackPlugin_l144_78 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_78 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_78 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_78 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_78 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_78 = (RegName_StatusReg == RegName_Breg);
  assign when_ChannelPlugin_l154 = 1'b1;
  assign _zz_when_ChannelPlugin_l129_16 = _zz_when_ChannelPlugin_l127_2[0];
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_148 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_144 & 32'hffffffff);
  assign when_RegStackPlugin_l144_79 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_79 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_79 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_79 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_79 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_79 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_149 = (32'h0 & 32'hffffffff);
  assign when_RegStackPlugin_l144_80 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_80 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_80 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_80 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_80 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_80 = (RegName_StatusReg == RegName_Breg);
  assign when_ChannelPlugin_l167 = 1'b1;
  assign _zz_6 = ({15'd0,1'b1} <<< _zz_when_ChannelPlugin_l127_3);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_150 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_144 & 32'hffffffff);
  assign when_RegStackPlugin_l144_81 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_81 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_81 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_81 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_81 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_81 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_151 = (32'h0 & 32'hffffffff);
  assign when_RegStackPlugin_l144_82 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_82 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_82 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_82 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_82 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_82 = (RegName_StatusReg == RegName_Breg);
  assign when_ChannelPlugin_l177 = 1'b1;
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_152 = (_zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_152 & 32'hffffffff);
  assign when_RegStackPlugin_l144_83 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_83 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_83 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_83 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_83 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_83 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_153 = (32'h0 & 32'hffffffff);
  assign when_RegStackPlugin_l144_84 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_84 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_84 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_84 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_84 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_84 = (RegName_StatusReg == RegName_Areg);
  assign when_ChannelPlugin_l190 = 1'b1;
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_154 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_142 & 32'hffffffff);
  assign when_RegStackPlugin_l144_85 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_85 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_85 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_85 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_85 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_85 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_155 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_144 & 32'hffffffff);
  assign when_RegStackPlugin_l144_86 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_86 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_86 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_86 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_86 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_86 = (RegName_StatusReg == RegName_Breg);
  assign when_ChannelPlugin_l200 = 1'b1;
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_156 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_142 & 32'hffffffff);
  assign when_RegStackPlugin_l144_87 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_87 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_87 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_87 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_87 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_87 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_157 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_144 & 32'hffffffff);
  assign when_RegStackPlugin_l144_88 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_88 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_88 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_88 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_88 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_88 = (RegName_StatusReg == RegName_Breg);
  assign when_ChannelPlugin_l222 = (! when_ChannelPlugin_l111);
  assign _zz_when_InterruptPlugin_l118_1 = (_zz_when_InterruptPlugin_l118[7 : 4] == PrimaryOpcode_OPR);
  assign _zz_when_InterruptPlugin_l118_2 = _zz_when_InterruptPlugin_l118[3 : 0];
  assign when_InterruptPlugin_l118 = (_zz_when_InterruptPlugin_l118_1 && ((((_zz_when_InterruptPlugin_l118_2 == 4'b0100) || (_zz_when_InterruptPlugin_l118_2 == 4'b0101)) || (_zz_when_InterruptPlugin_l118_2 == 4'b1010)) || (_zz_when_InterruptPlugin_l118_2 == 4'b1011)));
  always @(*) begin
    switch_InterruptPlugin_l123 = InterruptOp_INTDIS;
    if(_zz_when_InterruptPlugin_l118_1) begin
      if((_zz_when_InterruptPlugin_l118_2 == 4'b0100)) begin
          switch_InterruptPlugin_l123 = InterruptOp_INTDIS;
      end else if((_zz_when_InterruptPlugin_l118_2 == 4'b0101)) begin
          switch_InterruptPlugin_l123 = InterruptOp_INTENB;
      end else if((_zz_when_InterruptPlugin_l118_2 == 4'b1010)) begin
          switch_InterruptPlugin_l123 = InterruptOp_LDTRAPPED;
      end else if((_zz_when_InterruptPlugin_l118_2 == 4'b1011)) begin
          switch_InterruptPlugin_l123 = InterruptOp_STTRAPPED;
      end
    end
  end

  assign switch_Misc_l245_25 = RegName_Areg;
  always @(*) begin
    case(switch_Misc_l245_25)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_159 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_159 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_159 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_159 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_159 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_159 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_159 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_160 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_159 & 32'hffffffff);
  assign switch_Misc_l245_26 = RegName_Breg;
  always @(*) begin
    case(switch_Misc_l245_26)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_161 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_161 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_161 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_161 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_161 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_161 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_161 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_162 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_161 & 32'hffffffff);
  assign switch_Misc_l245_27 = RegName_Creg;
  always @(*) begin
    case(switch_Misc_l245_27)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_163 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_163 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_163 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_163 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_163 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_163 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_163 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_164 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_162 & 32'hffffffff);
  assign when_RegStackPlugin_l144_89 = (RegName_Areg == RegName_Creg);
  assign when_RegStackPlugin_l146_89 = (RegName_Breg == RegName_Creg);
  assign when_RegStackPlugin_l148_89 = (RegName_Creg == RegName_Creg);
  assign when_RegStackPlugin_l150_89 = (RegName_WdescReg == RegName_Creg);
  assign when_RegStackPlugin_l152_89 = (RegName_IptrReg == RegName_Creg);
  assign when_RegStackPlugin_l154_89 = (RegName_StatusReg == RegName_Creg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_165 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_160 & 32'hffffffff);
  assign when_RegStackPlugin_l144_90 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_90 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_90 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_90 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_90 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_90 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_166 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_158 & 32'hffffffff);
  assign when_RegStackPlugin_l144_91 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_91 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_91 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_91 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_91 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_91 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_167 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_162 & 32'hffffffff);
  assign when_RegStackPlugin_l144_92 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_92 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_92 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_92 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_92 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_92 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_168 = ((_zz_fpu_fpPipe_ctrl_0_down_RESULT_163 & 32'hffffffff) & 32'hffffffff);
  assign when_RegStackPlugin_l144_93 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_93 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_93 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_93 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_93 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_93 = (RegName_StatusReg == RegName_Breg);
  assign _zz_when_ResourcePlugin_l114_1 = (_zz_when_ResourcePlugin_l114[7 : 4] == PrimaryOpcode_OPR);
  assign _zz_when_ResourcePlugin_l114_2 = _zz_when_ResourcePlugin_l114[3 : 0];
  assign when_ResourcePlugin_l114 = (_zz_when_ResourcePlugin_l114_1 && ((_zz_when_ResourcePlugin_l114_2 == 4'b0100) || (_zz_when_ResourcePlugin_l114_2 == 4'b0011)));
  always @(*) begin
    switch_ResourcePlugin_l119 = ResourceOp_GRANT;
    if(_zz_when_ResourcePlugin_l114_1) begin
      case(_zz_when_ResourcePlugin_l114_2)
        4'b0100 : begin
          switch_ResourcePlugin_l119 = ResourceOp_MKRC;
        end
        4'b0011 : begin
          switch_ResourcePlugin_l119 = ResourceOp_UNMKRC;
        end
        default : begin
        end
      endcase
    end
  end

  assign switch_Misc_l245_28 = RegName_Areg;
  always @(*) begin
    case(switch_Misc_l245_28)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_170 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_170 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_170 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_170 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_170 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_170 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_170 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_171 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_170 & 32'hffffffff);
  assign switch_Misc_l245_29 = RegName_Breg;
  always @(*) begin
    case(switch_Misc_l245_29)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_172 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_172 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_172 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_172 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_172 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_172 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_172 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_173 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_172 & 32'hffffffff);
  assign switch_Misc_l245_30 = RegName_Creg;
  always @(*) begin
    case(switch_Misc_l245_30)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_174 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_174 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_174 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_174 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_174 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_174 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_174 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_175 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_174 & 32'hffffffff);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_176 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_173 & 32'hffffffff);
  assign when_RegStackPlugin_l144_94 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_94 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_94 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_94 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_94 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_94 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_177 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_175 & 32'hffffffff);
  assign when_RegStackPlugin_l144_95 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_95 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_95 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_95 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_95 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_95 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_178 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_173 & 32'hffffffff);
  assign when_RegStackPlugin_l144_96 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_96 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_96 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_96 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_96 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_96 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_179 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_175 & 32'hffffffff);
  assign when_RegStackPlugin_l144_97 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_97 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_97 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_97 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_97 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_97 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_180 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_173 & 32'hffffffff);
  assign when_RegStackPlugin_l144_98 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_98 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_98 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_98 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_98 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_98 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_181 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_175 & 32'hffffffff);
  assign when_RegStackPlugin_l144_99 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_99 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_99 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_99 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_99 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_99 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_182 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_173 & 32'hffffffff);
  assign when_RegStackPlugin_l144_100 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_100 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_100 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_100 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_100 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_100 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_183 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_175 & 32'hffffffff);
  assign when_RegStackPlugin_l144_101 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_101 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_101 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_101 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_101 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_101 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_184 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_173 & 32'hffffffff);
  assign when_RegStackPlugin_l144_102 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_102 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_102 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_102 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_102 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_102 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_185 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_175 & 32'hffffffff);
  assign when_RegStackPlugin_l144_103 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_103 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_103 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_103 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_103 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_103 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_186 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_173 & 32'hffffffff);
  assign when_RegStackPlugin_l144_104 = (RegName_Areg == RegName_Creg);
  assign when_RegStackPlugin_l146_104 = (RegName_Breg == RegName_Creg);
  assign when_RegStackPlugin_l148_104 = (RegName_Creg == RegName_Creg);
  assign when_RegStackPlugin_l150_104 = (RegName_WdescReg == RegName_Creg);
  assign when_RegStackPlugin_l152_104 = (RegName_IptrReg == RegName_Creg);
  assign when_RegStackPlugin_l154_104 = (RegName_StatusReg == RegName_Creg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_187 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_171 & 32'hffffffff);
  assign when_RegStackPlugin_l144_105 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_105 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_105 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_105 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_105 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_105 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_188 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_169 & 32'hffffffff);
  assign when_RegStackPlugin_l144_106 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_106 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_106 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_106 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_106 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_106 = (RegName_StatusReg == RegName_Areg);
  always @(*) begin
    _zz_fpu_fpPipe_ctrl_0_down_RESULT_189 = 32'h0;
    if(when_SystemPlugin_l123) begin
      case(switch_SystemPlugin_l128)
        SystemOp_TESTPRANAL : begin
          _zz_fpu_fpPipe_ctrl_0_down_RESULT_189 = 32'h00009000;
        end
        SystemOp_LDCONF : begin
        end
        SystemOp_STCONF : begin
        end
        SystemOp_SYSREQ : begin
        end
        SystemOp_DEVMOVE : begin
        end
        SystemOp_SETTIMESLICE : begin
        end
        default : begin
        end
      endcase
    end
  end

  assign _zz_when_SystemPlugin_l123_1 = (_zz_when_SystemPlugin_l123[7 : 4] == PrimaryOpcode_OPR);
  assign _zz_when_SystemPlugin_l123_2 = _zz_when_SystemPlugin_l123[3 : 0];
  assign when_SystemPlugin_l123 = (_zz_when_SystemPlugin_l123_1 && (((((((_zz_when_SystemPlugin_l123_2 == 4'b1110) || (_zz_when_SystemPlugin_l123_2 == 4'b1111)) || (_zz_when_SystemPlugin_l123_2 == 4'b0000)) || (_zz_when_SystemPlugin_l123_2 == 4'b0001)) || (_zz_when_SystemPlugin_l123_2 == 4'b0010)) || (_zz_when_SystemPlugin_l123_2 == 4'b0011)) || (_zz_when_SystemPlugin_l123_2 == 4'b0100)));
  always @(*) begin
    switch_SystemPlugin_l128 = SystemOp_TESTPRANAL;
    if(_zz_when_SystemPlugin_l123_1) begin
      case(_zz_when_SystemPlugin_l123_2)
        4'b1110 : begin
          switch_SystemPlugin_l128 = SystemOp_TESTPRANAL;
        end
        4'b1111 : begin
          switch_SystemPlugin_l128 = SystemOp_LDCONF;
        end
        4'b0000 : begin
          switch_SystemPlugin_l128 = SystemOp_STCONF;
        end
        4'b0001 : begin
          switch_SystemPlugin_l128 = SystemOp_SYSREQ;
        end
        4'b0010 : begin
          switch_SystemPlugin_l128 = SystemOp_DEVMOVE;
        end
        4'b0011 : begin
          switch_SystemPlugin_l128 = SystemOp_SETTIMESLICE;
        end
        4'b0100 : begin
          switch_SystemPlugin_l128 = SystemOp_LDMEMSTARTVAL;
        end
        default : begin
        end
      endcase
    end
  end

  assign switch_Misc_l245_31 = RegName_Areg;
  always @(*) begin
    case(switch_Misc_l245_31)
      RegName_Areg : begin
        _zz_switch_SystemPlugin_l145 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_switch_SystemPlugin_l145 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_switch_SystemPlugin_l145 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_switch_SystemPlugin_l145 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_switch_SystemPlugin_l145 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_switch_SystemPlugin_l145 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_switch_SystemPlugin_l145 = 32'h0;
      end
    endcase
  end

  assign _zz_switch_SystemPlugin_l145_1 = (_zz_switch_SystemPlugin_l145 & 32'hffffffff);
  assign switch_Misc_l245_32 = RegName_Breg;
  always @(*) begin
    case(switch_Misc_l245_32)
      RegName_Areg : begin
        _zz_switch_SystemPlugin_l163 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_switch_SystemPlugin_l163 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_switch_SystemPlugin_l163 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_switch_SystemPlugin_l163 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_switch_SystemPlugin_l163 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_switch_SystemPlugin_l163 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_switch_SystemPlugin_l163 = 32'h0;
      end
    endcase
  end

  assign _zz_switch_SystemPlugin_l163_1 = (_zz_switch_SystemPlugin_l163 & 32'hffffffff);
  assign switch_Misc_l245_33 = RegName_Creg;
  always @(*) begin
    case(switch_Misc_l245_33)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_195 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_195 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_195 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_195 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_195 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_195 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_195 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_196 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_195 & 32'hffffffff);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_197 = (_zz_switch_SystemPlugin_l163_1 & 32'hffffffff);
  assign when_RegStackPlugin_l144_107 = (RegName_Areg == RegName_Creg);
  assign when_RegStackPlugin_l146_107 = (RegName_Breg == RegName_Creg);
  assign when_RegStackPlugin_l148_107 = (RegName_Creg == RegName_Creg);
  assign when_RegStackPlugin_l150_107 = (RegName_WdescReg == RegName_Creg);
  assign when_RegStackPlugin_l152_107 = (RegName_IptrReg == RegName_Creg);
  assign when_RegStackPlugin_l154_107 = (RegName_StatusReg == RegName_Creg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_198 = (_zz_switch_SystemPlugin_l145_1 & 32'hffffffff);
  assign when_RegStackPlugin_l144_108 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_108 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_108 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_108 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_108 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_108 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_199 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_189 & 32'hffffffff);
  assign when_RegStackPlugin_l144_109 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_109 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_109 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_109 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_109 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_109 = (RegName_StatusReg == RegName_Areg);
  assign switch_SystemPlugin_l145 = _zz_switch_SystemPlugin_l145_1[3 : 0];
  always @(*) begin
    case(switch_SystemPlugin_l145)
      4'b0000 : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_200 = {24'd0, _zz_fpu_fpPipe_ctrl_0_down_RESULT_190};
      end
      4'b0001 : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_200 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_191;
      end
      4'b0010 : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_200 = {16'd0, _zz_fpu_fpPipe_ctrl_0_down_RESULT_192};
      end
      4'b0011 : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_200 = {16'd0, _zz_fpu_fpPipe_ctrl_0_down_RESULT_193};
      end
      4'b0100 : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_200 = {16'd0, _zz_fpu_fpPipe_ctrl_0_down_RESULT_194};
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_200 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_201 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_200 & 32'hffffffff);
  assign when_RegStackPlugin_l144_110 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_110 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_110 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_110 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_110 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_110 = (RegName_StatusReg == RegName_Areg);
  assign switch_SystemPlugin_l163 = _zz_switch_SystemPlugin_l163_1[3 : 0];
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_202 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_196 & 32'hffffffff);
  assign when_RegStackPlugin_l144_111 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_111 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_111 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_111 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_111 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_111 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_203 = (32'h0 & 32'hffffffff);
  assign when_RegStackPlugin_l144_112 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_112 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_112 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_112 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_112 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_112 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_204 = (_zz_switch_SystemPlugin_l163_1 & 32'hffffffff);
  assign when_RegStackPlugin_l144_113 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_113 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_113 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_113 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_113 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_113 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_205 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_196 & 32'hffffffff);
  assign when_RegStackPlugin_l144_114 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_114 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_114 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_114 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_114 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_114 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_206 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_196 & 32'hffffffff);
  assign when_RegStackPlugin_l144_115 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_115 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_115 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_115 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_115 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_115 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_207 = (32'h0 & 32'hffffffff);
  assign when_RegStackPlugin_l144_116 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_116 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_116 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_116 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_116 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_116 = (RegName_StatusReg == RegName_Breg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_208 = (_zz_switch_SystemPlugin_l163_1 & 32'hffffffff);
  assign when_RegStackPlugin_l144_117 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_117 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_117 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_117 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_117 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_117 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_209 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_196 & 32'hffffffff);
  assign when_RegStackPlugin_l144_118 = (RegName_Areg == RegName_Breg);
  assign when_RegStackPlugin_l146_118 = (RegName_Breg == RegName_Breg);
  assign when_RegStackPlugin_l148_118 = (RegName_Creg == RegName_Breg);
  assign when_RegStackPlugin_l150_118 = (RegName_WdescReg == RegName_Breg);
  assign when_RegStackPlugin_l152_118 = (RegName_IptrReg == RegName_Breg);
  assign when_RegStackPlugin_l154_118 = (RegName_StatusReg == RegName_Breg);
  assign switch_SystemPlugin_l217 = _zz_switch_SystemPlugin_l145_1[3 : 0];
  always @(*) begin
    case(switch_SystemPlugin_l217)
      4'b0000 : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_210 = 32'h80000000;
      end
      4'b0001 : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_210 = 32'h80004000;
      end
      4'b0010 : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_210 = 32'h8000c000;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_210 = 32'h80000000;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_211 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_210 & 32'hffffffff);
  assign when_RegStackPlugin_l144_119 = (RegName_Areg == RegName_Areg);
  assign when_RegStackPlugin_l146_119 = (RegName_Breg == RegName_Areg);
  assign when_RegStackPlugin_l148_119 = (RegName_Creg == RegName_Areg);
  assign when_RegStackPlugin_l150_119 = (RegName_WdescReg == RegName_Areg);
  assign when_RegStackPlugin_l152_119 = (RegName_IptrReg == RegName_Areg);
  assign when_RegStackPlugin_l154_119 = (RegName_StatusReg == RegName_Areg);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_AFIX = 56'h0;
  always @(*) begin
    fpuAdder_1_io_cmd_valid = 1'b0;
    if(fpu_fpPipe_ctrl_0_up_isValid) begin
      if(!_zz_when_20) begin
        if((_zz_7 == 8'h8e)) begin
        end else if((_zz_7 == 8'h8a)) begin
        end else if((_zz_7 == 8'haa)) begin
        end else if((_zz_7 == _zz_112)) begin
        end else if((_zz_7 == _zz_114)) begin
        end else if((_zz_7 == _zz_116)) begin
        end else if((_zz_7 == _zz_118)) begin
        end else if((_zz_7 == _zz_120)) begin
        end else if((_zz_7 == _zz_122)) begin
        end else if((_zz_7 == _zz_124) || (_zz_7 == _zz_126) || (_zz_7 == _zz_128) || (_zz_7 == _zz_130) || (_zz_7 == _zz_132) || (_zz_7 == _zz_134) || (_zz_7 == _zz_136)) begin
        end else if((_zz_7 == _zz_138)) begin
        end else if((_zz_7 == _zz_140)) begin
        end else if((_zz_7 == _zz_142)) begin
        end else if((_zz_7 == _zz_144)) begin
        end else if((_zz_7 == _zz_146)) begin
            fpuAdder_1_io_cmd_valid = 1'b1;
        end else if((_zz_7 == _zz_148)) begin
            fpuAdder_1_io_cmd_valid = 1'b1;
        end else if((_zz_7 == _zz_150)) begin
        end else if((_zz_7 == _zz_152)) begin
        end else if((_zz_7 == _zz_154)) begin
        end else if((_zz_7 == _zz_156)) begin
        end else if((_zz_7 == _zz_158)) begin
        end else if((_zz_7 == _zz_160)) begin
        end else if((_zz_7 == _zz_162)) begin
        end else if((_zz_7 == _zz_164)) begin
        end else if((_zz_7 == _zz_166)) begin
        end else if((_zz_7 == _zz_168)) begin
        end else if((_zz_7 == _zz_170)) begin
        end else if((_zz_7 == _zz_172)) begin
        end else if((_zz_7 == _zz_174)) begin
        end else if((_zz_7 == 8'h41)) begin
        end else if((_zz_7 == 8'h42)) begin
        end else if((_zz_7 == 8'h43)) begin
        end else if((_zz_7 == 8'h5f)) begin
        end else if((_zz_7 == 8'h90)) begin
        end
      end
    end
  end

  always @(*) begin
    fpuAdder_1_io_cmd_payload_a = 64'h0;
    if(fpu_fpPipe_ctrl_0_up_isValid) begin
      if(!_zz_when_20) begin
        if((_zz_7 == 8'h8e)) begin
        end else if((_zz_7 == 8'h8a)) begin
        end else if((_zz_7 == 8'haa)) begin
        end else if((_zz_7 == _zz_112)) begin
        end else if((_zz_7 == _zz_114)) begin
        end else if((_zz_7 == _zz_116)) begin
        end else if((_zz_7 == _zz_118)) begin
        end else if((_zz_7 == _zz_120)) begin
        end else if((_zz_7 == _zz_122)) begin
        end else if((_zz_7 == _zz_124) || (_zz_7 == _zz_126) || (_zz_7 == _zz_128) || (_zz_7 == _zz_130) || (_zz_7 == _zz_132) || (_zz_7 == _zz_134) || (_zz_7 == _zz_136)) begin
        end else if((_zz_7 == _zz_138)) begin
        end else if((_zz_7 == _zz_140)) begin
        end else if((_zz_7 == _zz_142)) begin
        end else if((_zz_7 == _zz_144)) begin
        end else if((_zz_7 == _zz_146)) begin
            fpuAdder_1_io_cmd_payload_a = _zz_fpu_fpPipe_ctrl_0_down_RESULT_215;
        end else if((_zz_7 == _zz_148)) begin
            fpuAdder_1_io_cmd_payload_a = _zz_fpu_fpPipe_ctrl_0_down_RESULT_215;
        end else if((_zz_7 == _zz_150)) begin
        end else if((_zz_7 == _zz_152)) begin
        end else if((_zz_7 == _zz_154)) begin
        end else if((_zz_7 == _zz_156)) begin
        end else if((_zz_7 == _zz_158)) begin
        end else if((_zz_7 == _zz_160)) begin
        end else if((_zz_7 == _zz_162)) begin
        end else if((_zz_7 == _zz_164)) begin
        end else if((_zz_7 == _zz_166)) begin
        end else if((_zz_7 == _zz_168)) begin
        end else if((_zz_7 == _zz_170)) begin
        end else if((_zz_7 == _zz_172)) begin
        end else if((_zz_7 == _zz_174)) begin
        end else if((_zz_7 == 8'h41)) begin
        end else if((_zz_7 == 8'h42)) begin
        end else if((_zz_7 == 8'h43)) begin
        end else if((_zz_7 == 8'h5f)) begin
        end else if((_zz_7 == 8'h90)) begin
        end
      end
    end
  end

  always @(*) begin
    fpuAdder_1_io_cmd_payload_b = 64'h0;
    if(fpu_fpPipe_ctrl_0_up_isValid) begin
      if(!_zz_when_20) begin
        if((_zz_7 == 8'h8e)) begin
        end else if((_zz_7 == 8'h8a)) begin
        end else if((_zz_7 == 8'haa)) begin
        end else if((_zz_7 == _zz_112)) begin
        end else if((_zz_7 == _zz_114)) begin
        end else if((_zz_7 == _zz_116)) begin
        end else if((_zz_7 == _zz_118)) begin
        end else if((_zz_7 == _zz_120)) begin
        end else if((_zz_7 == _zz_122)) begin
        end else if((_zz_7 == _zz_124) || (_zz_7 == _zz_126) || (_zz_7 == _zz_128) || (_zz_7 == _zz_130) || (_zz_7 == _zz_132) || (_zz_7 == _zz_134) || (_zz_7 == _zz_136)) begin
        end else if((_zz_7 == _zz_138)) begin
        end else if((_zz_7 == _zz_140)) begin
        end else if((_zz_7 == _zz_142)) begin
        end else if((_zz_7 == _zz_144)) begin
        end else if((_zz_7 == _zz_146)) begin
            fpuAdder_1_io_cmd_payload_b = _zz_io_cmd_payload_b_1;
        end else if((_zz_7 == _zz_148)) begin
            fpuAdder_1_io_cmd_payload_b = _zz_io_cmd_payload_b_1;
        end else if((_zz_7 == _zz_150)) begin
        end else if((_zz_7 == _zz_152)) begin
        end else if((_zz_7 == _zz_154)) begin
        end else if((_zz_7 == _zz_156)) begin
        end else if((_zz_7 == _zz_158)) begin
        end else if((_zz_7 == _zz_160)) begin
        end else if((_zz_7 == _zz_162)) begin
        end else if((_zz_7 == _zz_164)) begin
        end else if((_zz_7 == _zz_166)) begin
        end else if((_zz_7 == _zz_168)) begin
        end else if((_zz_7 == _zz_170)) begin
        end else if((_zz_7 == _zz_172)) begin
        end else if((_zz_7 == _zz_174)) begin
        end else if((_zz_7 == 8'h41)) begin
        end else if((_zz_7 == 8'h42)) begin
        end else if((_zz_7 == 8'h43)) begin
        end else if((_zz_7 == 8'h5f)) begin
        end else if((_zz_7 == 8'h90)) begin
        end
      end
    end
  end

  always @(*) begin
    fpuAdder_1_io_cmd_payload_sub = 1'b0;
    if(fpu_fpPipe_ctrl_0_up_isValid) begin
      if(!_zz_when_20) begin
        if((_zz_7 == 8'h8e)) begin
        end else if((_zz_7 == 8'h8a)) begin
        end else if((_zz_7 == 8'haa)) begin
        end else if((_zz_7 == _zz_112)) begin
        end else if((_zz_7 == _zz_114)) begin
        end else if((_zz_7 == _zz_116)) begin
        end else if((_zz_7 == _zz_118)) begin
        end else if((_zz_7 == _zz_120)) begin
        end else if((_zz_7 == _zz_122)) begin
        end else if((_zz_7 == _zz_124) || (_zz_7 == _zz_126) || (_zz_7 == _zz_128) || (_zz_7 == _zz_130) || (_zz_7 == _zz_132) || (_zz_7 == _zz_134) || (_zz_7 == _zz_136)) begin
        end else if((_zz_7 == _zz_138)) begin
        end else if((_zz_7 == _zz_140)) begin
        end else if((_zz_7 == _zz_142)) begin
        end else if((_zz_7 == _zz_144)) begin
        end else if((_zz_7 == _zz_146)) begin
            fpuAdder_1_io_cmd_payload_sub = 1'b0;
        end else if((_zz_7 == _zz_148)) begin
            fpuAdder_1_io_cmd_payload_sub = 1'b1;
        end else if((_zz_7 == _zz_150)) begin
        end else if((_zz_7 == _zz_152)) begin
        end else if((_zz_7 == _zz_154)) begin
        end else if((_zz_7 == _zz_156)) begin
        end else if((_zz_7 == _zz_158)) begin
        end else if((_zz_7 == _zz_160)) begin
        end else if((_zz_7 == _zz_162)) begin
        end else if((_zz_7 == _zz_164)) begin
        end else if((_zz_7 == _zz_166)) begin
        end else if((_zz_7 == _zz_168)) begin
        end else if((_zz_7 == _zz_170)) begin
        end else if((_zz_7 == _zz_172)) begin
        end else if((_zz_7 == _zz_174)) begin
        end else if((_zz_7 == 8'h41)) begin
        end else if((_zz_7 == 8'h42)) begin
        end else if((_zz_7 == 8'h43)) begin
        end else if((_zz_7 == 8'h5f)) begin
        end else if((_zz_7 == 8'h90)) begin
        end
      end
    end
  end

  always @(*) begin
    fpuAdder_1_io_cmd_payload_rounding = _zz_switch_Misc_l245_1;
    if(fpu_fpPipe_ctrl_0_up_isValid) begin
      if(!_zz_when_20) begin
        if((_zz_7 == 8'h8e)) begin
        end else if((_zz_7 == 8'h8a)) begin
        end else if((_zz_7 == 8'haa)) begin
        end else if((_zz_7 == _zz_112)) begin
        end else if((_zz_7 == _zz_114)) begin
        end else if((_zz_7 == _zz_116)) begin
        end else if((_zz_7 == _zz_118)) begin
        end else if((_zz_7 == _zz_120)) begin
        end else if((_zz_7 == _zz_122)) begin
        end else if((_zz_7 == _zz_124) || (_zz_7 == _zz_126) || (_zz_7 == _zz_128) || (_zz_7 == _zz_130) || (_zz_7 == _zz_132) || (_zz_7 == _zz_134) || (_zz_7 == _zz_136)) begin
        end else if((_zz_7 == _zz_138)) begin
        end else if((_zz_7 == _zz_140)) begin
        end else if((_zz_7 == _zz_142)) begin
        end else if((_zz_7 == _zz_144)) begin
        end else if((_zz_7 == _zz_146)) begin
            fpuAdder_1_io_cmd_payload_rounding = switch_Misc_l245_34;
        end else if((_zz_7 == _zz_148)) begin
            fpuAdder_1_io_cmd_payload_rounding = switch_Misc_l245_34;
        end else if((_zz_7 == _zz_150)) begin
        end else if((_zz_7 == _zz_152)) begin
        end else if((_zz_7 == _zz_154)) begin
        end else if((_zz_7 == _zz_156)) begin
        end else if((_zz_7 == _zz_158)) begin
        end else if((_zz_7 == _zz_160)) begin
        end else if((_zz_7 == _zz_162)) begin
        end else if((_zz_7 == _zz_164)) begin
        end else if((_zz_7 == _zz_166)) begin
        end else if((_zz_7 == _zz_168)) begin
        end else if((_zz_7 == _zz_170)) begin
        end else if((_zz_7 == _zz_172)) begin
        end else if((_zz_7 == _zz_174)) begin
        end else if((_zz_7 == 8'h41)) begin
        end else if((_zz_7 == 8'h42)) begin
        end else if((_zz_7 == 8'h43)) begin
        end else if((_zz_7 == 8'h5f)) begin
        end else if((_zz_7 == 8'h90)) begin
        end
      end
    end
  end

  assign _zz_7 = (_zz_fpu_fpPipe_ctrl_0_up_valid_1 ? _zz_1 : _zz_fpu_fpPipe_ctrl_0_up_valid);
  always @(*) begin
    _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h0;
    if((_zz_7 == 8'haa)) begin
        _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h002;
    end else if((_zz_7 == _zz_82)) begin
        _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h002;
    end else if((_zz_7 == _zz_84)) begin
        _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h002;
    end else if((_zz_7 == _zz_86)) begin
        _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h003;
    end else if((_zz_7 == _zz_88)) begin
        _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h00a;
    end else if((_zz_7 == _zz_90)) begin
        _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h00a;
    end else if((_zz_7 == _zz_92)) begin
        _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h00a;
    end else if((_zz_7 == _zz_94)) begin
        _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h011;
    end else if((_zz_7 == _zz_96)) begin
        _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h001;
    end else if((_zz_7 == _zz_98)) begin
        _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h001;
    end else if((_zz_7 == _zz_100)) begin
        _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h001;
    end else if((_zz_7 == _zz_102)) begin
        _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h001;
    end else if((_zz_7 == _zz_104)) begin
        _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h0;
    end else if((_zz_7 == _zz_106)) begin
        _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h0;
    end else if((_zz_7 == _zz_108)) begin
        _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h0;
    end else if((_zz_7 == _zz_110)) begin
        _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h0;
    end else if((_zz_7 == 8'h41) || (_zz_7 == 8'h42) || (_zz_7 == 8'h43) || (_zz_7 == 8'h5f) || (_zz_7 == 8'h90)) begin
        _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h00a;
    end
  end

  assign fpu_fpPipe_ctrl_0_up_valid = (execute_up_isValid && (_zz_fpu_fpPipe_ctrl_0_up_valid_1 || ((((_zz_fpu_fpPipe_ctrl_0_up_valid == _zz_fpu_fpPipe_ctrl_0_up_valid_2) || (_zz_fpu_fpPipe_ctrl_0_up_valid == _zz_fpu_fpPipe_ctrl_0_up_valid_4)) || (_zz_fpu_fpPipe_ctrl_0_up_valid == _zz_fpu_fpPipe_ctrl_0_up_valid_6)) || (_zz_fpu_fpPipe_ctrl_0_up_valid == _zz_fpu_fpPipe_ctrl_0_up_valid_8))));
  assign fpu_fpPipe_ctrl_0_haltRequest_FpuPlugin_l222 = (_zz_fpu_fpPipe_ctrl_0_down_CYCLE_CNT != 10'h0);
  always @(*) begin
    fpu_fpPipe_ctrl_0_down_RESULT = 64'h0;
    if(fpu_fpPipe_ctrl_0_up_isValid) begin
      if(_zz_when_20) begin
        fpu_fpPipe_ctrl_0_down_RESULT = 64'h0;
      end else begin
        if((_zz_7 == 8'h8e)) begin
        end else if((_zz_7 == 8'h8a)) begin
        end else if((_zz_7 == 8'haa)) begin
            fpu_fpPipe_ctrl_0_down_RESULT = _zz_fpu_fpPipe_ctrl_0_down_RESULT_212;
        end else if((_zz_7 == _zz_112)) begin
        end else if((_zz_7 == _zz_114)) begin
        end else if((_zz_7 == _zz_116)) begin
        end else if((_zz_7 == _zz_118)) begin
        end else if((_zz_7 == _zz_120)) begin
        end else if((_zz_7 == _zz_122)) begin
        end else if((_zz_7 == _zz_124) || (_zz_7 == _zz_126) || (_zz_7 == _zz_128) || (_zz_7 == _zz_130) || (_zz_7 == _zz_132) || (_zz_7 == _zz_134) || (_zz_7 == _zz_136)) begin
        end else if((_zz_7 == _zz_138)) begin
            fpu_fpPipe_ctrl_0_down_RESULT = {{_zz_fpu_fpPipe_ctrl_0_down_RESULT_216[31],_zz_fpu_fpPipe_ctrl_0_down_RESULT_229},{_zz_fpu_fpPipe_ctrl_0_down_RESULT_216[22 : 0],29'h0}};
        end else if((_zz_7 == _zz_140)) begin
            fpu_fpPipe_ctrl_0_down_RESULT = {32'd0, _zz_fpu_fpPipe_ctrl_0_down_RESULT_235};
        end else if((_zz_7 == _zz_142)) begin
        end else if((_zz_7 == _zz_144)) begin
            fpu_fpPipe_ctrl_0_down_RESULT = {32'd0, _zz_fpu_fpPipe_ctrl_0_down_RESULT_241};
        end else if((_zz_7 == _zz_146)) begin
            fpu_fpPipe_ctrl_0_down_RESULT = fpuAdder_1_io_rsp_payload;
        end else if((_zz_7 == _zz_148)) begin
            fpu_fpPipe_ctrl_0_down_RESULT = fpuAdder_1_io_rsp_payload;
        end else if((_zz_7 == _zz_150)) begin
            fpu_fpPipe_ctrl_0_down_RESULT = 64'h0;
        end else if((_zz_7 == _zz_152)) begin
            fpu_fpPipe_ctrl_0_down_RESULT = 64'h0;
        end else if((_zz_7 == _zz_154)) begin
            fpu_fpPipe_ctrl_0_down_RESULT = 64'h0;
        end else if((_zz_7 == _zz_156)) begin
            fpu_fpPipe_ctrl_0_down_RESULT = 64'h0;
        end else if((_zz_7 == _zz_158)) begin
            fpu_fpPipe_ctrl_0_down_RESULT = 64'h0;
        end else if((_zz_7 == _zz_160)) begin
            fpu_fpPipe_ctrl_0_down_RESULT = {{1'b0,_zz_fpu_fpPipe_ctrl_0_down_RESULT_215[62 : 52]},_zz_fpu_fpPipe_ctrl_0_down_RESULT_215[51 : 0]};
        end else if((_zz_7 == _zz_162)) begin
            fpu_fpPipe_ctrl_0_down_RESULT = {{_zz_fpu_fpPipe_ctrl_0_down_RESULT_215[63],_zz_fpu_fpPipe_ctrl_0_down_RESULT_215[62 : 52]},_zz_fpu_fpPipe_ctrl_0_down_RESULT_244};
        end else if((_zz_7 == _zz_164)) begin
            if(when_FpuPlugin_l421) begin
              fpu_fpPipe_ctrl_0_down_RESULT = {{_zz_fpu_fpPipe_ctrl_0_down_RESULT_226,11'h7ff},52'h0};
            end else begin
              fpu_fpPipe_ctrl_0_down_RESULT = {{_zz_fpu_fpPipe_ctrl_0_down_RESULT_226,_zz_fpu_fpPipe_ctrl_0_down_RESULT_227},_zz_fpu_fpPipe_ctrl_0_down_RESULT_215[51 : 0]};
            end
        end else if((_zz_7 == _zz_166)) begin
            if(when_FpuPlugin_l434) begin
              fpu_fpPipe_ctrl_0_down_RESULT = 64'h0;
            end else begin
              fpu_fpPipe_ctrl_0_down_RESULT = {{_zz_fpu_fpPipe_ctrl_0_down_RESULT_215[63],_zz_fpu_fpPipe_ctrl_0_down_RESULT_228},_zz_fpu_fpPipe_ctrl_0_down_RESULT_215[51 : 0]};
            end
        end else if((_zz_7 == _zz_168)) begin
        end else if((_zz_7 == _zz_170)) begin
        end else if((_zz_7 == _zz_172)) begin
        end else if((_zz_7 == _zz_174)) begin
        end else if((_zz_7 == 8'h41)) begin
        end else if((_zz_7 == 8'h42)) begin
        end else if((_zz_7 == 8'h43)) begin
            fpu_fpPipe_ctrl_0_down_RESULT = 64'h0;
        end else if((_zz_7 == 8'h5f)) begin
        end else if((_zz_7 == 8'h90)) begin
        end
      end
    end
  end

  always @(*) begin
    fpu_fpPipe_ctrl_0_down_RESULT_AFIX = 56'h0;
    if(fpu_fpPipe_ctrl_0_up_isValid) begin
      if(!_zz_when_20) begin
        if((_zz_7 == 8'h8e)) begin
        end else if((_zz_7 == 8'h8a)) begin
        end else if((_zz_7 == 8'haa)) begin
        end else if((_zz_7 == _zz_112)) begin
        end else if((_zz_7 == _zz_114)) begin
        end else if((_zz_7 == _zz_116)) begin
        end else if((_zz_7 == _zz_118)) begin
        end else if((_zz_7 == _zz_120)) begin
        end else if((_zz_7 == _zz_122)) begin
        end else if((_zz_7 == _zz_124) || (_zz_7 == _zz_126) || (_zz_7 == _zz_128) || (_zz_7 == _zz_130) || (_zz_7 == _zz_132) || (_zz_7 == _zz_134) || (_zz_7 == _zz_136)) begin
        end else if((_zz_7 == _zz_138)) begin
        end else if((_zz_7 == _zz_140)) begin
        end else if((_zz_7 == _zz_142)) begin
        end else if((_zz_7 == _zz_144)) begin
        end else if((_zz_7 == _zz_146)) begin
        end else if((_zz_7 == _zz_148)) begin
        end else if((_zz_7 == _zz_150)) begin
            fpu_fpPipe_ctrl_0_down_RESULT_AFIX = 56'h0;
        end else if((_zz_7 == _zz_152)) begin
            fpu_fpPipe_ctrl_0_down_RESULT_AFIX = _zz_fpu_fpPipe_ctrl_0_down_RESULT_AFIX;
        end else if((_zz_7 == _zz_154)) begin
            fpu_fpPipe_ctrl_0_down_RESULT_AFIX = _zz_fpu_fpPipe_ctrl_0_down_RESULT_AFIX;
        end else if((_zz_7 == _zz_156)) begin
            fpu_fpPipe_ctrl_0_down_RESULT_AFIX = _zz_fpu_fpPipe_ctrl_0_down_RESULT_AFIX;
        end else if((_zz_7 == _zz_158)) begin
        end else if((_zz_7 == _zz_160)) begin
        end else if((_zz_7 == _zz_162)) begin
        end else if((_zz_7 == _zz_164)) begin
        end else if((_zz_7 == _zz_166)) begin
        end else if((_zz_7 == _zz_168)) begin
        end else if((_zz_7 == _zz_170)) begin
        end else if((_zz_7 == _zz_172)) begin
        end else if((_zz_7 == _zz_174)) begin
        end else if((_zz_7 == 8'h41)) begin
        end else if((_zz_7 == 8'h42)) begin
        end else if((_zz_7 == 8'h43)) begin
        end else if((_zz_7 == 8'h5f)) begin
        end else if((_zz_7 == 8'h90)) begin
        end
      end
    end
  end

  always @(*) begin
    fpu_fpPipe_ctrl_0_down_MAX_CYCLES = 10'h0;
    if(fpu_fpPipe_ctrl_0_up_isFiring) begin
      fpu_fpPipe_ctrl_0_down_MAX_CYCLES = _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES;
    end
  end

  always @(*) begin
    fpu_fpPipe_ctrl_0_down_T805_STATE = 64'h0;
    if(fpu_fpPipe_ctrl_0_up_isValid) begin
      if(!_zz_when_20) begin
        if((_zz_7 == 8'h8e)) begin
        end else if((_zz_7 == 8'h8a)) begin
        end else if((_zz_7 == 8'haa)) begin
        end else if((_zz_7 == _zz_112)) begin
        end else if((_zz_7 == _zz_114)) begin
        end else if((_zz_7 == _zz_116)) begin
        end else if((_zz_7 == _zz_118)) begin
        end else if((_zz_7 == _zz_120)) begin
        end else if((_zz_7 == _zz_122)) begin
        end else if((_zz_7 == _zz_124) || (_zz_7 == _zz_126) || (_zz_7 == _zz_128) || (_zz_7 == _zz_130) || (_zz_7 == _zz_132) || (_zz_7 == _zz_134) || (_zz_7 == _zz_136)) begin
        end else if((_zz_7 == _zz_138)) begin
        end else if((_zz_7 == _zz_140)) begin
        end else if((_zz_7 == _zz_142)) begin
        end else if((_zz_7 == _zz_144)) begin
        end else if((_zz_7 == _zz_146)) begin
        end else if((_zz_7 == _zz_148)) begin
        end else if((_zz_7 == _zz_150)) begin
        end else if((_zz_7 == _zz_152)) begin
        end else if((_zz_7 == _zz_154)) begin
        end else if((_zz_7 == _zz_156)) begin
        end else if((_zz_7 == _zz_158)) begin
        end else if((_zz_7 == _zz_160)) begin
        end else if((_zz_7 == _zz_162)) begin
        end else if((_zz_7 == _zz_164)) begin
        end else if((_zz_7 == _zz_166)) begin
        end else if((_zz_7 == _zz_168)) begin
        end else if((_zz_7 == _zz_170)) begin
        end else if((_zz_7 == _zz_172)) begin
        end else if((_zz_7 == _zz_174)) begin
        end else if((_zz_7 == 8'h41)) begin
            fpu_fpPipe_ctrl_0_down_T805_STATE = 64'h0;
        end else if((_zz_7 == 8'h42)) begin
            fpu_fpPipe_ctrl_0_down_T805_STATE = 64'h0;
        end else if((_zz_7 == 8'h43)) begin
        end else if((_zz_7 == 8'h5f)) begin
            fpu_fpPipe_ctrl_0_down_T805_STATE = 64'h0;
        end else if((_zz_7 == 8'h90)) begin
            fpu_fpPipe_ctrl_0_down_T805_STATE = 64'h0;
        end
      end
    end
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_215 = (_zz_fpu_fpPipe_ctrl_0_up_valid_1 ? _zz_fpu_fpPipe_ctrl_0_down_RESULT : _zz_fpu_fpPipe_ctrl_0_down_RESULT_212);
  assign _zz_io_cmd_payload_b_1 = (_zz_fpu_fpPipe_ctrl_0_up_valid_1 ? _zz_io_cmd_payload_b : _zz_fpu_fpPipe_ctrl_0_down_RESULT_213);
  assign switch_Misc_l245_34 = (_zz_fpu_fpPipe_ctrl_0_up_valid_1 ? _zz_switch_Misc_l245 : _zz_switch_Misc_l245_1);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_216 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_212[31 : 0];
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_217 = _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_217;
  assign switch_Misc_l245_35 = RegName_Areg;
  always @(*) begin
    case(switch_Misc_l245_35)
      RegName_Areg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_218 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_3;
      end
      RegName_Breg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_218 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_4;
      end
      RegName_Creg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_218 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_5;
      end
      RegName_WdescReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_218 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_6;
      end
      RegName_IptrReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_218 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_7;
      end
      RegName_StatusReg : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_218 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_8;
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_218 = 32'h0;
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_219 = _zz__zz_fpu_fpPipe_ctrl_0_down_RESULT_219;
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_220 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_215[63];
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_221 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_215[63 : 12];
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_222 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_215[11];
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_223 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_215[10];
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_224 = (|_zz_fpu_fpPipe_ctrl_0_down_RESULT_215[9 : 0]);
  always @(*) begin
    case(switch_Misc_l245_34)
      2'b00 : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_225 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_222 && ((_zz_fpu_fpPipe_ctrl_0_down_RESULT_223 || _zz_fpu_fpPipe_ctrl_0_down_RESULT_224) || _zz_fpu_fpPipe_ctrl_0_down_RESULT_221[0]));
      end
      2'b01 : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_225 = 1'b0;
      end
      2'b10 : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_225 = (((_zz_fpu_fpPipe_ctrl_0_down_RESULT_222 || _zz_fpu_fpPipe_ctrl_0_down_RESULT_223) || _zz_fpu_fpPipe_ctrl_0_down_RESULT_224) && (! _zz_fpu_fpPipe_ctrl_0_down_RESULT_220));
      end
      default : begin
        _zz_fpu_fpPipe_ctrl_0_down_RESULT_225 = (((_zz_fpu_fpPipe_ctrl_0_down_RESULT_222 || _zz_fpu_fpPipe_ctrl_0_down_RESULT_223) || _zz_fpu_fpPipe_ctrl_0_down_RESULT_224) && _zz_fpu_fpPipe_ctrl_0_down_RESULT_220);
      end
    endcase
  end

  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_226 = _zz_fpu_fpPipe_ctrl_0_down_RESULT_215[63];
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_227 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_215[62 : 52] + 11'h001);
  assign when_FpuPlugin_l421 = (11'h7ff <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_227);
  assign _zz_fpu_fpPipe_ctrl_0_down_RESULT_228 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_215[62 : 52] - 11'h001);
  assign when_FpuPlugin_l434 = (_zz_fpu_fpPipe_ctrl_0_down_RESULT_228 <= 11'h0);
  assign when_FpuPlugin_l492 = (fpu_fpPipe_ctrl_0_down_isFiring && (! 1'b0));
  assign when_FpuPlugin_l512 = (fpu_fpPipe_ctrl_0_up_isValid && (_zz_fpu_fpPipe_ctrl_0_down_CYCLE_CNT != 10'h0));
  assign fpu_fpPipe_ctrl_0_down_CYCLE_CNT = _zz_fpu_fpPipe_ctrl_0_down_CYCLE_CNT;
  always @(*) begin
    fpu_fpPipe_ctrl_0_down_valid = fpu_fpPipe_ctrl_0_up_valid;
    if(when_CtrlLink_l150) begin
      fpu_fpPipe_ctrl_0_down_valid = 1'b0;
    end
  end

  always @(*) begin
    fpu_fpPipe_ctrl_0_up_ready = fpu_fpPipe_ctrl_0_down_isReady;
    if(when_CtrlLink_l150) begin
      fpu_fpPipe_ctrl_0_up_ready = 1'b0;
    end
  end

  assign when_CtrlLink_l150 = (|fpu_fpPipe_ctrl_0_haltRequest_FpuPlugin_l222);
  assign fpu_fpPipe_ctrl_0_up_isFiring = (fpu_fpPipe_ctrl_0_up_isValid && fpu_fpPipe_ctrl_0_up_isReady);
  assign fpu_fpPipe_ctrl_0_up_isValid = fpu_fpPipe_ctrl_0_up_valid;
  assign fpu_fpPipe_ctrl_0_up_isReady = fpu_fpPipe_ctrl_0_up_ready;
  assign fpu_fpPipe_ctrl_0_down_isFiring = (fpu_fpPipe_ctrl_0_down_isValid && fpu_fpPipe_ctrl_0_down_isReady);
  assign fpu_fpPipe_ctrl_0_down_isValid = fpu_fpPipe_ctrl_0_down_valid;
  assign fpu_fpPipe_ctrl_0_down_isReady = 1'b1;
  assign streamFifo_2_io_push_valid = (_zz_io_push_valid_3 && _zz_io_push_valid_2);
  assign io_push_fire = (streamFifo_2_io_push_valid && streamFifo_2_io_push_ready);
  assign streamFifo_3_io_push_valid = (_zz_io_push_valid_1 && (! _zz_io_push_valid));
  assign io_push_fire_1 = (streamFifo_3_io_push_valid && streamFifo_3_io_push_ready);
  always @(*) begin
    streamFifo_2_io_pop_ready = 1'b0;
    if(when_SchedulerPlugin_l131) begin
      if(streamFifo_2_io_pop_valid) begin
        streamFifo_2_io_pop_ready = 1'b1;
      end
    end
  end

  always @(*) begin
    streamFifo_3_io_pop_ready = 1'b0;
    if(when_SchedulerPlugin_l131) begin
      if(!streamFifo_2_io_pop_valid) begin
        if(streamFifo_3_io_pop_valid) begin
          streamFifo_3_io_pop_ready = 1'b1;
        end
      end
    end
  end

  assign when_SchedulerPlugin_l131 = ((_zz_when_SchedulerPlugin_l131 || _zz_when_SchedulerPlugin_l131_1) && _zz_when_SchedulerPlugin_l131_2);
  assign when_SchedulerPlugin_l163 = ((! _zz_when_SchedulerPlugin_l131_2) && ((5'h0 < streamFifo_2_io_occupancy) || (5'h0 < streamFifo_3_io_occupancy)));
  assign when_SchedulerPlugin_l168 = ((((_zz_when_SchedulerPlugin_l168 == ProcessState_RUNNING) && streamFifo_2_io_pop_valid) && (! _zz_when_SchedulerPlugin_l131)) && (! _zz_when_SchedulerPlugin_l131_1));
  assign when_SchedulerPlugin_l170 = (_zz_when_SchedulerPlugin_l170 != 32'h0);
  always @(posedge clk) begin
    if(reset) begin
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= 32'h80000000;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= 32'h0;
      _zz_io_input_cmd_payload_fragment_address <= 32'h80000000;
      _zz_io_input_cmd_valid_2 <= 1'b0;
      when_LongArithPlugin_l312 <= 1'b0;
      _zz_when_LongArithPlugin_l203 <= 4'b0000;
      when_BlockMovePlugin_l165 <= 1'b0;
      _zz_4 <= 3'b000;
      _zz_when_BlockMovePlugin_l165 <= 32'h0;
      _zz_when_BlockMovePlugin_l165_1 <= 32'h0;
      _zz_when_BlockMovePlugin_l165_2 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_125 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_126 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_127 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_128 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_129 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_130 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_131 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_132 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_133 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_134 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_135 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_136 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_137 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_138 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_139 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_140 <= 32'h0;
      _zz_when_ChannelPlugin_l127 <= 16'h0;
      _zz_when_ChannelPlugin_l129 <= 1'b0;
      _zz_when_ChannelPlugin_l129_1 <= 1'b0;
      _zz_when_ChannelPlugin_l129_2 <= 1'b0;
      _zz_when_ChannelPlugin_l129_3 <= 1'b0;
      _zz_when_ChannelPlugin_l129_4 <= 1'b0;
      _zz_when_ChannelPlugin_l129_5 <= 1'b0;
      _zz_when_ChannelPlugin_l129_6 <= 1'b0;
      _zz_when_ChannelPlugin_l129_7 <= 1'b0;
      _zz_when_ChannelPlugin_l129_8 <= 1'b0;
      _zz_when_ChannelPlugin_l129_9 <= 1'b0;
      _zz_when_ChannelPlugin_l129_10 <= 1'b0;
      _zz_when_ChannelPlugin_l129_11 <= 1'b0;
      _zz_when_ChannelPlugin_l129_12 <= 1'b0;
      _zz_when_ChannelPlugin_l129_13 <= 1'b0;
      _zz_when_ChannelPlugin_l129_14 <= 1'b0;
      _zz_when_ChannelPlugin_l129_15 <= 1'b0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_158 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_169 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_190 <= 8'h90;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_191 <= 32'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_192 <= 16'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_193 <= 16'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_194 <= 16'h0100;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_212 <= 64'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_213 <= 64'h0;
      _zz_fpu_fpPipe_ctrl_0_down_RESULT_214 <= 64'h0;
      _zz_switch_Misc_l245_1 <= 2'b00;
      _zz_fpu_fpPipe_ctrl_0_down_CYCLE_CNT <= 10'h0;
      _zz_when_SchedulerPlugin_l170 <= 32'h0;
      _zz_when_SchedulerPlugin_l170_1 <= 32'h0;
      _zz_when_SchedulerPlugin_l131 <= 1'b0;
      _zz_when_SchedulerPlugin_l131_1 <= 1'b0;
      _zz_when_SchedulerPlugin_l131_2 <= 1'b1;
      _zz_when_SchedulerPlugin_l168 <= ProcessState_READY;
    end else begin
      `ifndef SYNTHESIS
        `ifdef FORMAL
          assert(1'b0); // core.scala:L566
        `else
          if(!1'b0) begin
            $display("NOTE Initializing FpuPlugin v0.3"); // core.scala:L566
          end
        `endif
      `endif
      `ifndef SYNTHESIS
        `ifdef FORMAL
          assert(1'b0); // core.scala:L566
        `else
          if(!1'b0) begin
            $display("NOTE Initializing SchedulerPlugin v0.2"); // core.scala:L566
          end
        `endif
      `endif
      `ifndef SYNTHESIS
        `ifdef FORMAL
          assert(1'b0); // core.scala:L566
        `else
          if(!1'b0) begin
            $display("NOTE Initializing TimerPlugin v0.2"); // core.scala:L566
          end
        `endif
      `endif
      `ifndef SYNTHESIS
        `ifdef FORMAL
          assert(1'b0); // core.scala:L566
        `else
          if(!1'b0) begin
            $display("NOTE Initializing PipelineBuilderPlugin v0.5"); // core.scala:L566
          end
        `endif
      `endif
      if((_zz_io_input_cmd_valid_1 && bmbUpSizerBridge_1_io_input_cmd_ready)) begin
        _zz_io_input_cmd_valid_2 <= 1'b1;
        _zz_io_input_cmd_payload_fragment_address <= (_zz_io_input_cmd_payload_fragment_address + 32'h00000008);
      end
      if((bmbUpSizerBridge_1_io_input_rsp_valid && _zz_io_input_rsp_ready)) begin
        _zz_io_input_cmd_valid_2 <= 1'b0;
      end
      if(((_zz_2[7 : 4] == PrimaryOpcode_OPR) && ((((((((((_zz_3 == _zz_when) || (_zz_3 == _zz_when_2)) || (_zz_3 == _zz_when_4)) || (_zz_3 == _zz_when_6)) || (_zz_3 == _zz_when_8)) || (_zz_3 == _zz_when_10)) || (_zz_3 == _zz_when_12)) || (_zz_3 == _zz_when_14)) || (_zz_3 == _zz_when_16)) || (_zz_3 == _zz_when_18)))) begin
        case(switch_ArithmeticPlugin_l160)
          AluOp_ADD : begin
            if(when_RegStackPlugin_l144) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_15;
            end else begin
              if(when_RegStackPlugin_l146) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_15;
              end else begin
                if(when_RegStackPlugin_l148) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_15;
                end else begin
                  if(when_RegStackPlugin_l150) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_15;
                  end else begin
                    if(when_RegStackPlugin_l152) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_15;
                    end else begin
                      if(when_RegStackPlugin_l154) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_15;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_1) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_16;
            end else begin
              if(when_RegStackPlugin_l146_1) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_16;
              end else begin
                if(when_RegStackPlugin_l148_1) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_16;
                end else begin
                  if(when_RegStackPlugin_l150_1) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_16;
                  end else begin
                    if(when_RegStackPlugin_l152_1) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_16;
                    end else begin
                      if(when_RegStackPlugin_l154_1) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_16;
                      end
                    end
                  end
                end
              end
            end
          end
          AluOp_SUB : begin
            if(when_RegStackPlugin_l144_2) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_17;
            end else begin
              if(when_RegStackPlugin_l146_2) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_17;
              end else begin
                if(when_RegStackPlugin_l148_2) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_17;
                end else begin
                  if(when_RegStackPlugin_l150_2) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_17;
                  end else begin
                    if(when_RegStackPlugin_l152_2) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_17;
                    end else begin
                      if(when_RegStackPlugin_l154_2) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_17;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_3) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_18;
            end else begin
              if(when_RegStackPlugin_l146_3) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_18;
              end else begin
                if(when_RegStackPlugin_l148_3) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_18;
                end else begin
                  if(when_RegStackPlugin_l150_3) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_18;
                  end else begin
                    if(when_RegStackPlugin_l152_3) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_18;
                    end else begin
                      if(when_RegStackPlugin_l154_3) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_18;
                      end
                    end
                  end
                end
              end
            end
          end
          AluOp_AND_1 : begin
            if(when_RegStackPlugin_l144_4) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_19;
            end else begin
              if(when_RegStackPlugin_l146_4) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_19;
              end else begin
                if(when_RegStackPlugin_l148_4) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_19;
                end else begin
                  if(when_RegStackPlugin_l150_4) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_19;
                  end else begin
                    if(when_RegStackPlugin_l152_4) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_19;
                    end else begin
                      if(when_RegStackPlugin_l154_4) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_19;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_5) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_20;
            end else begin
              if(when_RegStackPlugin_l146_5) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_20;
              end else begin
                if(when_RegStackPlugin_l148_5) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_20;
                end else begin
                  if(when_RegStackPlugin_l150_5) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_20;
                  end else begin
                    if(when_RegStackPlugin_l152_5) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_20;
                    end else begin
                      if(when_RegStackPlugin_l154_5) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_20;
                      end
                    end
                  end
                end
              end
            end
          end
          AluOp_OR_1 : begin
            if(when_RegStackPlugin_l144_6) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_21;
            end else begin
              if(when_RegStackPlugin_l146_6) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_21;
              end else begin
                if(when_RegStackPlugin_l148_6) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_21;
                end else begin
                  if(when_RegStackPlugin_l150_6) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_21;
                  end else begin
                    if(when_RegStackPlugin_l152_6) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_21;
                    end else begin
                      if(when_RegStackPlugin_l154_6) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_21;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_7) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_22;
            end else begin
              if(when_RegStackPlugin_l146_7) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_22;
              end else begin
                if(when_RegStackPlugin_l148_7) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_22;
                end else begin
                  if(when_RegStackPlugin_l150_7) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_22;
                  end else begin
                    if(when_RegStackPlugin_l152_7) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_22;
                    end else begin
                      if(when_RegStackPlugin_l154_7) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_22;
                      end
                    end
                  end
                end
              end
            end
          end
          AluOp_XOR_1 : begin
            if(when_RegStackPlugin_l144_8) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_23;
            end else begin
              if(when_RegStackPlugin_l146_8) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_23;
              end else begin
                if(when_RegStackPlugin_l148_8) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_23;
                end else begin
                  if(when_RegStackPlugin_l150_8) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_23;
                  end else begin
                    if(when_RegStackPlugin_l152_8) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_23;
                    end else begin
                      if(when_RegStackPlugin_l154_8) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_23;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_9) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_24;
            end else begin
              if(when_RegStackPlugin_l146_9) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_24;
              end else begin
                if(when_RegStackPlugin_l148_9) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_24;
                end else begin
                  if(when_RegStackPlugin_l150_9) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_24;
                  end else begin
                    if(when_RegStackPlugin_l152_9) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_24;
                    end else begin
                      if(when_RegStackPlugin_l154_9) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_24;
                      end
                    end
                  end
                end
              end
            end
          end
          AluOp_NOT_1 : begin
            if(when_RegStackPlugin_l144_10) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_25;
            end else begin
              if(when_RegStackPlugin_l146_10) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_25;
              end else begin
                if(when_RegStackPlugin_l148_10) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_25;
                end else begin
                  if(when_RegStackPlugin_l150_10) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_25;
                  end else begin
                    if(when_RegStackPlugin_l152_10) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_25;
                    end else begin
                      if(when_RegStackPlugin_l154_10) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_25;
                      end
                    end
                  end
                end
              end
            end
          end
          AluOp_REV : begin
            if(when_RegStackPlugin_l144_11) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_26;
            end else begin
              if(when_RegStackPlugin_l146_11) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_26;
              end else begin
                if(when_RegStackPlugin_l148_11) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_26;
                end else begin
                  if(when_RegStackPlugin_l150_11) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_26;
                  end else begin
                    if(when_RegStackPlugin_l152_11) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_26;
                    end else begin
                      if(when_RegStackPlugin_l154_11) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_26;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_12) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_27;
            end else begin
              if(when_RegStackPlugin_l146_12) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_27;
              end else begin
                if(when_RegStackPlugin_l148_12) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_27;
                end else begin
                  if(when_RegStackPlugin_l150_12) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_27;
                  end else begin
                    if(when_RegStackPlugin_l152_12) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_27;
                    end else begin
                      if(when_RegStackPlugin_l154_12) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_27;
                      end
                    end
                  end
                end
              end
            end
          end
          default : begin
          end
        endcase
      end
      if(when_LongArithPlugin_l142) begin
        when_LongArithPlugin_l312 <= (4'b0001 < _zz_when_LongArithPlugin_l312);
        _zz_when_LongArithPlugin_l203 <= (_zz_when_LongArithPlugin_l312 - 4'b0001);
        case(switch_LongArithPlugin_l132)
          LongArithOp_LADD : begin
            if(when_RegStackPlugin_l144_13) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_33;
            end else begin
              if(when_RegStackPlugin_l146_13) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_33;
              end else begin
                if(when_RegStackPlugin_l148_13) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_33;
                end else begin
                  if(when_RegStackPlugin_l150_13) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_33;
                  end else begin
                    if(when_RegStackPlugin_l152_13) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_33;
                    end else begin
                      if(when_RegStackPlugin_l154_13) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_33;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_14) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_34;
            end else begin
              if(when_RegStackPlugin_l146_14) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_34;
              end else begin
                if(when_RegStackPlugin_l148_14) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_34;
                end else begin
                  if(when_RegStackPlugin_l150_14) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_34;
                  end else begin
                    if(when_RegStackPlugin_l152_14) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_34;
                    end else begin
                      if(when_RegStackPlugin_l154_14) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_34;
                      end
                    end
                  end
                end
              end
            end
          end
          LongArithOp_LSUB : begin
            if(when_RegStackPlugin_l144_15) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_36;
            end else begin
              if(when_RegStackPlugin_l146_15) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_36;
              end else begin
                if(when_RegStackPlugin_l148_15) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_36;
                end else begin
                  if(when_RegStackPlugin_l150_15) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_36;
                  end else begin
                    if(when_RegStackPlugin_l152_15) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_36;
                    end else begin
                      if(when_RegStackPlugin_l154_15) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_36;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_16) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_37;
            end else begin
              if(when_RegStackPlugin_l146_16) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_37;
              end else begin
                if(when_RegStackPlugin_l148_16) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_37;
                end else begin
                  if(when_RegStackPlugin_l150_16) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_37;
                  end else begin
                    if(when_RegStackPlugin_l152_16) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_37;
                    end else begin
                      if(when_RegStackPlugin_l154_16) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_37;
                      end
                    end
                  end
                end
              end
            end
          end
          LongArithOp_LMUL : begin
            if(when_LongArithPlugin_l203) begin
              if(when_RegStackPlugin_l144_17) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_39;
              end else begin
                if(when_RegStackPlugin_l146_17) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_39;
                end else begin
                  if(when_RegStackPlugin_l148_17) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_39;
                  end else begin
                    if(when_RegStackPlugin_l150_17) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_39;
                    end else begin
                      if(when_RegStackPlugin_l152_17) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_39;
                      end else begin
                        if(when_RegStackPlugin_l154_17) begin
                          _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_39;
                        end
                      end
                    end
                  end
                end
              end
              if(when_RegStackPlugin_l144_18) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_40;
              end else begin
                if(when_RegStackPlugin_l146_18) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_40;
                end else begin
                  if(when_RegStackPlugin_l148_18) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_40;
                  end else begin
                    if(when_RegStackPlugin_l150_18) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_40;
                    end else begin
                      if(when_RegStackPlugin_l152_18) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_40;
                      end else begin
                        if(when_RegStackPlugin_l154_18) begin
                          _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_40;
                        end
                      end
                    end
                  end
                end
              end
            end
          end
          LongArithOp_LDIV : begin
            if(when_LongArithPlugin_l220) begin
              if(when_RegStackPlugin_l144_19) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_42;
              end else begin
                if(when_RegStackPlugin_l146_19) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_42;
                end else begin
                  if(when_RegStackPlugin_l148_19) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_42;
                  end else begin
                    if(when_RegStackPlugin_l150_19) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_42;
                    end else begin
                      if(when_RegStackPlugin_l152_19) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_42;
                      end else begin
                        if(when_RegStackPlugin_l154_19) begin
                          _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_42;
                        end
                      end
                    end
                  end
                end
              end
              if(when_RegStackPlugin_l144_20) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_43;
              end else begin
                if(when_RegStackPlugin_l146_20) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_43;
                end else begin
                  if(when_RegStackPlugin_l148_20) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_43;
                  end else begin
                    if(when_RegStackPlugin_l150_20) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_43;
                    end else begin
                      if(when_RegStackPlugin_l152_20) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_43;
                      end else begin
                        if(when_RegStackPlugin_l154_20) begin
                          _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_43;
                        end
                      end
                    end
                  end
                end
              end
            end
          end
          LongArithOp_LSHL : begin
            if(when_RegStackPlugin_l144_21) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_45;
            end else begin
              if(when_RegStackPlugin_l146_21) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_45;
              end else begin
                if(when_RegStackPlugin_l148_21) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_45;
                end else begin
                  if(when_RegStackPlugin_l150_21) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_45;
                  end else begin
                    if(when_RegStackPlugin_l152_21) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_45;
                    end else begin
                      if(when_RegStackPlugin_l154_21) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_45;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_22) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_46;
            end else begin
              if(when_RegStackPlugin_l146_22) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_46;
              end else begin
                if(when_RegStackPlugin_l148_22) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_46;
                end else begin
                  if(when_RegStackPlugin_l150_22) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_46;
                  end else begin
                    if(when_RegStackPlugin_l152_22) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_46;
                    end else begin
                      if(when_RegStackPlugin_l154_22) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_46;
                      end
                    end
                  end
                end
              end
            end
          end
          LongArithOp_LSHR : begin
            if(when_RegStackPlugin_l144_23) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_48;
            end else begin
              if(when_RegStackPlugin_l146_23) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_48;
              end else begin
                if(when_RegStackPlugin_l148_23) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_48;
                end else begin
                  if(when_RegStackPlugin_l150_23) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_48;
                  end else begin
                    if(when_RegStackPlugin_l152_23) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_48;
                    end else begin
                      if(when_RegStackPlugin_l154_23) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_48;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_24) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_49;
            end else begin
              if(when_RegStackPlugin_l146_24) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_49;
              end else begin
                if(when_RegStackPlugin_l148_24) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_49;
                end else begin
                  if(when_RegStackPlugin_l150_24) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_49;
                  end else begin
                    if(when_RegStackPlugin_l152_24) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_49;
                    end else begin
                      if(when_RegStackPlugin_l154_24) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_49;
                      end
                    end
                  end
                end
              end
            end
          end
          LongArithOp_MINT : begin
            if(when_RegStackPlugin_l144_25) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_50;
            end else begin
              if(when_RegStackPlugin_l146_25) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_50;
              end else begin
                if(when_RegStackPlugin_l148_25) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_50;
                end else begin
                  if(when_RegStackPlugin_l150_25) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_50;
                  end else begin
                    if(when_RegStackPlugin_l152_25) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_50;
                    end else begin
                      if(when_RegStackPlugin_l154_25) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_50;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_26) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_51;
            end else begin
              if(when_RegStackPlugin_l146_26) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_51;
              end else begin
                if(when_RegStackPlugin_l148_26) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_51;
                end else begin
                  if(when_RegStackPlugin_l150_26) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_51;
                  end else begin
                    if(when_RegStackPlugin_l152_26) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_51;
                    end else begin
                      if(when_RegStackPlugin_l154_26) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_51;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_27) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_52;
            end else begin
              if(when_RegStackPlugin_l146_27) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_52;
              end else begin
                if(when_RegStackPlugin_l148_27) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_52;
                end else begin
                  if(when_RegStackPlugin_l150_27) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_52;
                  end else begin
                    if(when_RegStackPlugin_l152_27) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_52;
                    end else begin
                      if(when_RegStackPlugin_l154_27) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_52;
                      end
                    end
                  end
                end
              end
            end
          end
          LongArithOp_XSWORD : begin
            if(when_RegStackPlugin_l144_28) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_54;
            end else begin
              if(when_RegStackPlugin_l146_28) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_54;
              end else begin
                if(when_RegStackPlugin_l148_28) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_54;
                end else begin
                  if(when_RegStackPlugin_l150_28) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_54;
                  end else begin
                    if(when_RegStackPlugin_l152_28) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_54;
                    end else begin
                      if(when_RegStackPlugin_l154_28) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_54;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_29) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_55;
            end else begin
              if(when_RegStackPlugin_l146_29) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_55;
              end else begin
                if(when_RegStackPlugin_l148_29) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_55;
                end else begin
                  if(when_RegStackPlugin_l150_29) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_55;
                  end else begin
                    if(when_RegStackPlugin_l152_29) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_55;
                    end else begin
                      if(when_RegStackPlugin_l154_29) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_55;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_30) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_56;
            end else begin
              if(when_RegStackPlugin_l146_30) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_56;
              end else begin
                if(when_RegStackPlugin_l148_30) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_56;
                end else begin
                  if(when_RegStackPlugin_l150_30) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_56;
                  end else begin
                    if(when_RegStackPlugin_l152_30) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_56;
                    end else begin
                      if(when_RegStackPlugin_l154_30) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_56;
                      end
                    end
                  end
                end
              end
            end
          end
          LongArithOp_PROD : begin
            if(when_RegStackPlugin_l144_31) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_57;
            end else begin
              if(when_RegStackPlugin_l146_31) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_57;
              end else begin
                if(when_RegStackPlugin_l148_31) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_57;
                end else begin
                  if(when_RegStackPlugin_l150_31) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_57;
                  end else begin
                    if(when_RegStackPlugin_l152_31) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_57;
                    end else begin
                      if(when_RegStackPlugin_l154_31) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_57;
                      end
                    end
                  end
                end
              end
            end
          end
          default : begin
          end
        endcase
      end
      if(when_LongArithPlugin_l312) begin
        if(when_LongArithPlugin_l313) begin
          when_LongArithPlugin_l312 <= 1'b0;
        end else begin
          _zz_when_LongArithPlugin_l203 <= (_zz_when_LongArithPlugin_l203 - 4'b0001);
        end
      end
      if(when_ControlFlowPlugin_l115) begin
        case(switch_ControlFlowPlugin_l132)
          ControlFlowOp_RET : begin
            if(when_RegStackPlugin_l144_32) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_62;
            end else begin
              if(when_RegStackPlugin_l146_32) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_62;
              end else begin
                if(when_RegStackPlugin_l148_32) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_62;
                end else begin
                  if(when_RegStackPlugin_l150_32) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_62;
                  end else begin
                    if(when_RegStackPlugin_l152_32) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_62;
                    end else begin
                      if(when_RegStackPlugin_l154_32) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_62;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_33) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_63;
            end else begin
              if(when_RegStackPlugin_l146_33) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_63;
              end else begin
                if(when_RegStackPlugin_l148_33) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_63;
                end else begin
                  if(when_RegStackPlugin_l150_33) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_63;
                  end else begin
                    if(when_RegStackPlugin_l152_33) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_63;
                    end else begin
                      if(when_RegStackPlugin_l154_33) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_63;
                      end
                    end
                  end
                end
              end
            end
          end
          ControlFlowOp_LDPI : begin
            if(when_RegStackPlugin_l144_34) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_64;
            end else begin
              if(when_RegStackPlugin_l146_34) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_64;
              end else begin
                if(when_RegStackPlugin_l148_34) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_64;
                end else begin
                  if(when_RegStackPlugin_l150_34) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_64;
                  end else begin
                    if(when_RegStackPlugin_l152_34) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_64;
                    end else begin
                      if(when_RegStackPlugin_l154_34) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_64;
                      end
                    end
                  end
                end
              end
            end
          end
          ControlFlowOp_GAJW : begin
            if(when_RegStackPlugin_l144_35) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_65;
            end else begin
              if(when_RegStackPlugin_l146_35) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_65;
              end else begin
                if(when_RegStackPlugin_l148_35) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_65;
                end else begin
                  if(when_RegStackPlugin_l150_35) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_65;
                  end else begin
                    if(when_RegStackPlugin_l152_35) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_65;
                    end else begin
                      if(when_RegStackPlugin_l154_35) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_65;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_36) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_66;
            end else begin
              if(when_RegStackPlugin_l146_36) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_66;
              end else begin
                if(when_RegStackPlugin_l148_36) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_66;
                end else begin
                  if(when_RegStackPlugin_l150_36) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_66;
                  end else begin
                    if(when_RegStackPlugin_l152_36) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_66;
                    end else begin
                      if(when_RegStackPlugin_l154_36) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_66;
                      end
                    end
                  end
                end
              end
            end
          end
          ControlFlowOp_GCALL : begin
            if(when_RegStackPlugin_l144_37) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_67;
            end else begin
              if(when_RegStackPlugin_l146_37) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_67;
              end else begin
                if(when_RegStackPlugin_l148_37) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_67;
                end else begin
                  if(when_RegStackPlugin_l150_37) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_67;
                  end else begin
                    if(when_RegStackPlugin_l152_37) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_67;
                    end else begin
                      if(when_RegStackPlugin_l154_37) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_67;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_38) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_68;
            end else begin
              if(when_RegStackPlugin_l146_38) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_68;
              end else begin
                if(when_RegStackPlugin_l148_38) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_68;
                end else begin
                  if(when_RegStackPlugin_l150_38) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_68;
                  end else begin
                    if(when_RegStackPlugin_l152_38) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_68;
                    end else begin
                      if(when_RegStackPlugin_l154_38) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_68;
                      end
                    end
                  end
                end
              end
            end
          end
          ControlFlowOp_LEND : begin
            if(when_ControlFlowPlugin_l171) begin
              if(when_RegStackPlugin_l144_39) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_69;
              end else begin
                if(when_RegStackPlugin_l146_39) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_69;
                end else begin
                  if(when_RegStackPlugin_l148_39) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_69;
                  end else begin
                    if(when_RegStackPlugin_l150_39) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_69;
                    end else begin
                      if(when_RegStackPlugin_l152_39) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_69;
                      end else begin
                        if(when_RegStackPlugin_l154_39) begin
                          _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_69;
                        end
                      end
                    end
                  end
                end
              end
            end else begin
              if(when_RegStackPlugin_l144_40) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_70;
              end else begin
                if(when_RegStackPlugin_l146_40) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_70;
                end else begin
                  if(when_RegStackPlugin_l148_40) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_70;
                  end else begin
                    if(when_RegStackPlugin_l150_40) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_70;
                    end else begin
                      if(when_RegStackPlugin_l152_40) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_70;
                      end else begin
                        if(when_RegStackPlugin_l154_40) begin
                          _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_70;
                        end
                      end
                    end
                  end
                end
              end
            end
          end
          ControlFlowOp_DISS : begin
            if(when_RegStackPlugin_l144_41) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_71;
            end else begin
              if(when_RegStackPlugin_l146_41) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_71;
              end else begin
                if(when_RegStackPlugin_l148_41) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_71;
                end else begin
                  if(when_RegStackPlugin_l150_41) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_71;
                  end else begin
                    if(when_RegStackPlugin_l152_41) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_71;
                    end else begin
                      if(when_RegStackPlugin_l154_41) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_71;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_42) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_72;
            end else begin
              if(when_RegStackPlugin_l146_42) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_72;
              end else begin
                if(when_RegStackPlugin_l148_42) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_72;
                end else begin
                  if(when_RegStackPlugin_l150_42) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_72;
                  end else begin
                    if(when_RegStackPlugin_l152_42) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_72;
                    end else begin
                      if(when_RegStackPlugin_l154_42) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_72;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_43) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_73;
            end else begin
              if(when_RegStackPlugin_l146_43) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_73;
              end else begin
                if(when_RegStackPlugin_l148_43) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_73;
                end else begin
                  if(when_RegStackPlugin_l150_43) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_73;
                  end else begin
                    if(when_RegStackPlugin_l152_43) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_73;
                    end else begin
                      if(when_RegStackPlugin_l154_43) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_73;
                      end
                    end
                  end
                end
              end
            end
          end
          default : begin
          end
        endcase
      end
      if(when_BlockMovePlugin_l113) begin
        case(switch_BlockMovePlugin_l118)
          BlockMoveOp_MOVE : begin
            _zz_when_BlockMovePlugin_l165 <= _zz_when_BlockMovePlugin_l165_6;
            when_BlockMovePlugin_l165 <= (_zz_when_BlockMovePlugin_l165_6 != 32'h0);
            _zz_4 <= 3'b001;
          end
          BlockMoveOp_MOVE2DINIT : begin
            _zz_when_BlockMovePlugin_l165_1 <= (_zz_when_BlockMovePlugin_l165_3 & 32'hffffffff);
            _zz_when_BlockMovePlugin_l165_2 <= (_zz_when_BlockMovePlugin_l165_4 & 32'hffffffff);
            if(when_RegStackPlugin_l144_44) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_74;
            end else begin
              if(when_RegStackPlugin_l146_44) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_74;
              end else begin
                if(when_RegStackPlugin_l148_44) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_74;
                end else begin
                  if(when_RegStackPlugin_l150_44) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_74;
                  end else begin
                    if(when_RegStackPlugin_l152_44) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_74;
                    end else begin
                      if(when_RegStackPlugin_l154_44) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_74;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_45) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_75;
            end else begin
              if(when_RegStackPlugin_l146_45) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_75;
              end else begin
                if(when_RegStackPlugin_l148_45) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_75;
                end else begin
                  if(when_RegStackPlugin_l150_45) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_75;
                  end else begin
                    if(when_RegStackPlugin_l152_45) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_75;
                    end else begin
                      if(when_RegStackPlugin_l154_45) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_75;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_46) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_76;
            end else begin
              if(when_RegStackPlugin_l146_46) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_76;
              end else begin
                if(when_RegStackPlugin_l148_46) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_76;
                end else begin
                  if(when_RegStackPlugin_l150_46) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_76;
                  end else begin
                    if(when_RegStackPlugin_l152_46) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_76;
                    end else begin
                      if(when_RegStackPlugin_l154_46) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_76;
                      end
                    end
                  end
                end
              end
            end
          end
          BlockMoveOp_MOVE2DALL : begin
            _zz_when_BlockMovePlugin_l165 <= _zz__zz_when_BlockMovePlugin_l165[31:0];
            when_BlockMovePlugin_l165 <= (_zz_when_BlockMovePlugin_l165 != 32'h0);
            _zz_4 <= 3'b010;
          end
          BlockMoveOp_MOVE2DNONZERO : begin
            when_BlockMovePlugin_l165 <= 1'b0;
          end
          default : begin
            when_BlockMovePlugin_l165 <= 1'b0;
          end
        endcase
      end
      if(when_BlockMovePlugin_l165) begin
        case(_zz_4)
          3'b001 : begin
            if(when_BlockMovePlugin_l171) begin
              _zz_when_BlockMovePlugin_l165 <= (_zz_when_BlockMovePlugin_l165 - 32'h00000004);
            end else begin
              _zz_when_BlockMovePlugin_l165 <= 32'h0;
              when_BlockMovePlugin_l165 <= 1'b0;
            end
          end
          3'b010 : begin
            if(when_BlockMovePlugin_l187) begin
              _zz_when_BlockMovePlugin_l165 <= (_zz_when_BlockMovePlugin_l165 - 32'h00000001);
            end else begin
              when_BlockMovePlugin_l165 <= 1'b0;
            end
          end
          default : begin
          end
        endcase
      end
      if(when_IndexingPlugin_l135) begin
        case(switch_IndexingPlugin_l155)
          IndexingOp_LDLP : begin
            if(when_RegStackPlugin_l144_47) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_87;
            end else begin
              if(when_RegStackPlugin_l146_47) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_87;
              end else begin
                if(when_RegStackPlugin_l148_47) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_87;
                end else begin
                  if(when_RegStackPlugin_l150_47) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_87;
                  end else begin
                    if(when_RegStackPlugin_l152_47) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_87;
                    end else begin
                      if(when_RegStackPlugin_l154_47) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_87;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_48) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_88;
            end else begin
              if(when_RegStackPlugin_l146_48) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_88;
              end else begin
                if(when_RegStackPlugin_l148_48) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_88;
                end else begin
                  if(when_RegStackPlugin_l150_48) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_88;
                  end else begin
                    if(when_RegStackPlugin_l152_48) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_88;
                    end else begin
                      if(when_RegStackPlugin_l154_48) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_88;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_49) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_89;
            end else begin
              if(when_RegStackPlugin_l146_49) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_89;
              end else begin
                if(when_RegStackPlugin_l148_49) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_89;
                end else begin
                  if(when_RegStackPlugin_l150_49) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_89;
                  end else begin
                    if(when_RegStackPlugin_l152_49) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_89;
                    end else begin
                      if(when_RegStackPlugin_l154_49) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_89;
                      end
                    end
                  end
                end
              end
            end
          end
          IndexingOp_LDNLP : begin
            if(when_RegStackPlugin_l144_50) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_90;
            end else begin
              if(when_RegStackPlugin_l146_50) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_90;
              end else begin
                if(when_RegStackPlugin_l148_50) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_90;
                end else begin
                  if(when_RegStackPlugin_l150_50) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_90;
                  end else begin
                    if(when_RegStackPlugin_l152_50) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_90;
                    end else begin
                      if(when_RegStackPlugin_l154_50) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_90;
                      end
                    end
                  end
                end
              end
            end
          end
          IndexingOp_BSUB : begin
            if(when_RegStackPlugin_l144_51) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_91;
            end else begin
              if(when_RegStackPlugin_l146_51) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_91;
              end else begin
                if(when_RegStackPlugin_l148_51) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_91;
                end else begin
                  if(when_RegStackPlugin_l150_51) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_91;
                  end else begin
                    if(when_RegStackPlugin_l152_51) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_91;
                    end else begin
                      if(when_RegStackPlugin_l154_51) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_91;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_52) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_92;
            end else begin
              if(when_RegStackPlugin_l146_52) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_92;
              end else begin
                if(when_RegStackPlugin_l148_52) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_92;
                end else begin
                  if(when_RegStackPlugin_l150_52) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_92;
                  end else begin
                    if(when_RegStackPlugin_l152_52) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_92;
                    end else begin
                      if(when_RegStackPlugin_l154_52) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_92;
                      end
                    end
                  end
                end
              end
            end
          end
          IndexingOp_WSUB : begin
            if(when_RegStackPlugin_l144_53) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_93;
            end else begin
              if(when_RegStackPlugin_l146_53) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_93;
              end else begin
                if(when_RegStackPlugin_l148_53) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_93;
                end else begin
                  if(when_RegStackPlugin_l150_53) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_93;
                  end else begin
                    if(when_RegStackPlugin_l152_53) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_93;
                    end else begin
                      if(when_RegStackPlugin_l154_53) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_93;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_54) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_94;
            end else begin
              if(when_RegStackPlugin_l146_54) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_94;
              end else begin
                if(when_RegStackPlugin_l148_54) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_94;
                end else begin
                  if(when_RegStackPlugin_l150_54) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_94;
                  end else begin
                    if(when_RegStackPlugin_l152_54) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_94;
                    end else begin
                      if(when_RegStackPlugin_l154_54) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_94;
                      end
                    end
                  end
                end
              end
            end
          end
          default : begin
          end
        endcase
      end
      if(when_RangeCheckPlugin_l124) begin
        case(switch_RangeCheckPlugin_l137)
          RangeCheckOp_CIR : begin
            if(when_RegStackPlugin_l144_55) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_98;
            end else begin
              if(when_RegStackPlugin_l146_55) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_98;
              end else begin
                if(when_RegStackPlugin_l148_55) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_98;
                end else begin
                  if(when_RegStackPlugin_l150_55) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_98;
                  end else begin
                    if(when_RegStackPlugin_l152_55) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_98;
                    end else begin
                      if(when_RegStackPlugin_l154_55) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_98;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_56) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_99;
            end else begin
              if(when_RegStackPlugin_l146_56) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_99;
              end else begin
                if(when_RegStackPlugin_l148_56) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_99;
                end else begin
                  if(when_RegStackPlugin_l150_56) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_99;
                  end else begin
                    if(when_RegStackPlugin_l152_56) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_99;
                    end else begin
                      if(when_RegStackPlugin_l154_56) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_99;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_57) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_100;
            end else begin
              if(when_RegStackPlugin_l146_57) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_100;
              end else begin
                if(when_RegStackPlugin_l148_57) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_100;
                end else begin
                  if(when_RegStackPlugin_l150_57) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_100;
                  end else begin
                    if(when_RegStackPlugin_l152_57) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_100;
                    end else begin
                      if(when_RegStackPlugin_l154_57) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_100;
                      end
                    end
                  end
                end
              end
            end
          end
          RangeCheckOp_CB : begin
          end
          RangeCheckOp_CS : begin
          end
          RangeCheckOp_CWORD : begin
          end
          RangeCheckOp_XSWORD : begin
            if(when_RegStackPlugin_l144_58) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_101;
            end else begin
              if(when_RegStackPlugin_l146_58) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_101;
              end else begin
                if(when_RegStackPlugin_l148_58) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_101;
                end else begin
                  if(when_RegStackPlugin_l150_58) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_101;
                  end else begin
                    if(when_RegStackPlugin_l152_58) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_101;
                    end else begin
                      if(when_RegStackPlugin_l154_58) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_101;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_59) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_102;
            end else begin
              if(when_RegStackPlugin_l146_59) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_102;
              end else begin
                if(when_RegStackPlugin_l148_59) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_102;
                end else begin
                  if(when_RegStackPlugin_l150_59) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_102;
                  end else begin
                    if(when_RegStackPlugin_l152_59) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_102;
                    end else begin
                      if(when_RegStackPlugin_l154_59) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_102;
                      end
                    end
                  end
                end
              end
            end
          end
          RangeCheckOp_CCNT1 : begin
            if(when_RangeCheckPlugin_l191) begin
              if(when_RegStackPlugin_l144_60) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_103;
              end else begin
                if(when_RegStackPlugin_l146_60) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_103;
                end else begin
                  if(when_RegStackPlugin_l148_60) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_103;
                  end else begin
                    if(when_RegStackPlugin_l150_60) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_103;
                    end else begin
                      if(when_RegStackPlugin_l152_60) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_103;
                      end else begin
                        if(when_RegStackPlugin_l154_60) begin
                          _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_103;
                        end
                      end
                    end
                  end
                end
              end
            end
          end
          RangeCheckOp_CJ : begin
            if(when_RegStackPlugin_l144_61) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_104;
            end else begin
              if(when_RegStackPlugin_l146_61) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_104;
              end else begin
                if(when_RegStackPlugin_l148_61) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_104;
                end else begin
                  if(when_RegStackPlugin_l150_61) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_104;
                  end else begin
                    if(when_RegStackPlugin_l152_61) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_104;
                    end else begin
                      if(when_RegStackPlugin_l154_61) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_104;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_62) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_105;
            end else begin
              if(when_RegStackPlugin_l146_62) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_105;
              end else begin
                if(when_RegStackPlugin_l148_62) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_105;
                end else begin
                  if(when_RegStackPlugin_l150_62) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_105;
                  end else begin
                    if(when_RegStackPlugin_l152_62) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_105;
                    end else begin
                      if(when_RegStackPlugin_l154_62) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_105;
                      end
                    end
                  end
                end
              end
            end
          end
          RangeCheckOp_CALL : begin
            if(when_RegStackPlugin_l144_63) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_106;
            end else begin
              if(when_RegStackPlugin_l146_63) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_106;
              end else begin
                if(when_RegStackPlugin_l148_63) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_106;
                end else begin
                  if(when_RegStackPlugin_l150_63) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_106;
                  end else begin
                    if(when_RegStackPlugin_l152_63) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_106;
                    end else begin
                      if(when_RegStackPlugin_l154_63) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_106;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_64) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_107;
            end else begin
              if(when_RegStackPlugin_l146_64) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_107;
              end else begin
                if(when_RegStackPlugin_l148_64) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_107;
                end else begin
                  if(when_RegStackPlugin_l150_64) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_107;
                  end else begin
                    if(when_RegStackPlugin_l152_64) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_107;
                    end else begin
                      if(when_RegStackPlugin_l154_64) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_107;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_65) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_108;
            end else begin
              if(when_RegStackPlugin_l146_65) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_108;
              end else begin
                if(when_RegStackPlugin_l148_65) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_108;
                end else begin
                  if(when_RegStackPlugin_l150_65) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_108;
                  end else begin
                    if(when_RegStackPlugin_l152_65) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_108;
                    end else begin
                      if(when_RegStackPlugin_l154_65) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_108;
                      end
                    end
                  end
                end
              end
            end
          end
          RangeCheckOp_CSNGL : begin
          end
          default : begin
          end
        endcase
      end
      if(when_GeneralPlugin_l69) begin
        if((_zz_when_GeneralPlugin_l69_1 == _zz_72)) begin
            if(when_RegStackPlugin_l144_66) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_114;
            end else begin
              if(when_RegStackPlugin_l146_66) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_114;
              end else begin
                if(when_RegStackPlugin_l148_66) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_114;
                end else begin
                  if(when_RegStackPlugin_l150_66) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_114;
                  end else begin
                    if(when_RegStackPlugin_l152_66) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_114;
                    end else begin
                      if(when_RegStackPlugin_l154_66) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_114;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_67) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_115;
            end else begin
              if(when_RegStackPlugin_l146_67) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_115;
              end else begin
                if(when_RegStackPlugin_l148_67) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_115;
                end else begin
                  if(when_RegStackPlugin_l150_67) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_115;
                  end else begin
                    if(when_RegStackPlugin_l152_67) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_115;
                    end else begin
                      if(when_RegStackPlugin_l154_67) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_115;
                      end
                    end
                  end
                end
              end
            end
        end else if((_zz_when_GeneralPlugin_l69_1 == _zz_74)) begin
            if(when_RegStackPlugin_l144_68) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_116;
            end else begin
              if(when_RegStackPlugin_l146_68) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_116;
              end else begin
                if(when_RegStackPlugin_l148_68) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_116;
                end else begin
                  if(when_RegStackPlugin_l150_68) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_116;
                  end else begin
                    if(when_RegStackPlugin_l152_68) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_116;
                    end else begin
                      if(when_RegStackPlugin_l154_68) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_116;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_69) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_117;
            end else begin
              if(when_RegStackPlugin_l146_69) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_117;
              end else begin
                if(when_RegStackPlugin_l148_69) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_117;
                end else begin
                  if(when_RegStackPlugin_l150_69) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_117;
                  end else begin
                    if(when_RegStackPlugin_l152_69) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_117;
                    end else begin
                      if(when_RegStackPlugin_l154_69) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_117;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_70) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_118;
            end else begin
              if(when_RegStackPlugin_l146_70) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_118;
              end else begin
                if(when_RegStackPlugin_l148_70) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_118;
                end else begin
                  if(when_RegStackPlugin_l150_70) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_118;
                  end else begin
                    if(when_RegStackPlugin_l152_70) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_118;
                    end else begin
                      if(when_RegStackPlugin_l154_70) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_118;
                      end
                    end
                  end
                end
              end
            end
        end else if((_zz_when_GeneralPlugin_l69_1 == _zz_76)) begin
            if(when_RegStackPlugin_l144_71) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_119;
            end else begin
              if(when_RegStackPlugin_l146_71) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_119;
              end else begin
                if(when_RegStackPlugin_l148_71) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_119;
                end else begin
                  if(when_RegStackPlugin_l150_71) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_119;
                  end else begin
                    if(when_RegStackPlugin_l152_71) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_119;
                    end else begin
                      if(when_RegStackPlugin_l154_71) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_119;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_72) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_120;
            end else begin
              if(when_RegStackPlugin_l146_72) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_120;
              end else begin
                if(when_RegStackPlugin_l148_72) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_120;
                end else begin
                  if(when_RegStackPlugin_l150_72) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_120;
                  end else begin
                    if(when_RegStackPlugin_l152_72) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_120;
                    end else begin
                      if(when_RegStackPlugin_l154_72) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_120;
                      end
                    end
                  end
                end
              end
            end
        end else if((_zz_when_GeneralPlugin_l69_1 == _zz_78)) begin
        end else if((_zz_when_GeneralPlugin_l69_1 == _zz_80)) begin
            if(when_RegStackPlugin_l144_73) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_121;
            end else begin
              if(when_RegStackPlugin_l146_73) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_121;
              end else begin
                if(when_RegStackPlugin_l148_73) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_121;
                end else begin
                  if(when_RegStackPlugin_l150_73) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_121;
                  end else begin
                    if(when_RegStackPlugin_l152_73) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_121;
                    end else begin
                      if(when_RegStackPlugin_l154_73) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_121;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_74) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_122;
            end else begin
              if(when_RegStackPlugin_l146_74) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_122;
              end else begin
                if(when_RegStackPlugin_l148_74) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_122;
                end else begin
                  if(when_RegStackPlugin_l150_74) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_122;
                  end else begin
                    if(when_RegStackPlugin_l152_74) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_122;
                    end else begin
                      if(when_RegStackPlugin_l154_74) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_122;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_75) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_123;
            end else begin
              if(when_RegStackPlugin_l146_75) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_123;
              end else begin
                if(when_RegStackPlugin_l148_75) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_123;
                end else begin
                  if(when_RegStackPlugin_l150_75) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_123;
                  end else begin
                    if(when_RegStackPlugin_l152_75) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_123;
                    end else begin
                      if(when_RegStackPlugin_l154_75) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_123;
                      end
                    end
                  end
                end
              end
            end
        end
      end
      if(when_ChannelPlugin_l111) begin
        case(switch_ChannelPlugin_l121)
          ChannelOp_CHANTYPE : begin
            if(when_RegStackPlugin_l144_76) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_145;
            end else begin
              if(when_RegStackPlugin_l146_76) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_145;
              end else begin
                if(when_RegStackPlugin_l148_76) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_145;
                end else begin
                  if(when_RegStackPlugin_l150_76) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_145;
                  end else begin
                    if(when_RegStackPlugin_l152_76) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_145;
                    end else begin
                      if(when_RegStackPlugin_l154_76) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_145;
                      end
                    end
                  end
                end
              end
            end
          end
          ChannelOp_INITVLCB : begin
            if(when_RegStackPlugin_l144_77) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_146;
            end else begin
              if(when_RegStackPlugin_l146_77) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_146;
              end else begin
                if(when_RegStackPlugin_l148_77) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_146;
                end else begin
                  if(when_RegStackPlugin_l150_77) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_146;
                  end else begin
                    if(when_RegStackPlugin_l152_77) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_146;
                    end else begin
                      if(when_RegStackPlugin_l154_77) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_146;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_78) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_147;
            end else begin
              if(when_RegStackPlugin_l146_78) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_147;
              end else begin
                if(when_RegStackPlugin_l148_78) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_147;
                end else begin
                  if(when_RegStackPlugin_l150_78) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_147;
                  end else begin
                    if(when_RegStackPlugin_l152_78) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_147;
                    end else begin
                      if(when_RegStackPlugin_l154_78) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_147;
                      end
                    end
                  end
                end
              end
            end
          end
          ChannelOp_SETCHMODE : begin
            if(when_ChannelPlugin_l154) begin
              if(_zz_5[0]) begin
                _zz_when_ChannelPlugin_l129 <= _zz_when_ChannelPlugin_l129_16;
              end
              if(_zz_5[1]) begin
                _zz_when_ChannelPlugin_l129_1 <= _zz_when_ChannelPlugin_l129_16;
              end
              if(_zz_5[2]) begin
                _zz_when_ChannelPlugin_l129_2 <= _zz_when_ChannelPlugin_l129_16;
              end
              if(_zz_5[3]) begin
                _zz_when_ChannelPlugin_l129_3 <= _zz_when_ChannelPlugin_l129_16;
              end
              if(_zz_5[4]) begin
                _zz_when_ChannelPlugin_l129_4 <= _zz_when_ChannelPlugin_l129_16;
              end
              if(_zz_5[5]) begin
                _zz_when_ChannelPlugin_l129_5 <= _zz_when_ChannelPlugin_l129_16;
              end
              if(_zz_5[6]) begin
                _zz_when_ChannelPlugin_l129_6 <= _zz_when_ChannelPlugin_l129_16;
              end
              if(_zz_5[7]) begin
                _zz_when_ChannelPlugin_l129_7 <= _zz_when_ChannelPlugin_l129_16;
              end
              if(_zz_5[8]) begin
                _zz_when_ChannelPlugin_l129_8 <= _zz_when_ChannelPlugin_l129_16;
              end
              if(_zz_5[9]) begin
                _zz_when_ChannelPlugin_l129_9 <= _zz_when_ChannelPlugin_l129_16;
              end
              if(_zz_5[10]) begin
                _zz_when_ChannelPlugin_l129_10 <= _zz_when_ChannelPlugin_l129_16;
              end
              if(_zz_5[11]) begin
                _zz_when_ChannelPlugin_l129_11 <= _zz_when_ChannelPlugin_l129_16;
              end
              if(_zz_5[12]) begin
                _zz_when_ChannelPlugin_l129_12 <= _zz_when_ChannelPlugin_l129_16;
              end
              if(_zz_5[13]) begin
                _zz_when_ChannelPlugin_l129_13 <= _zz_when_ChannelPlugin_l129_16;
              end
              if(_zz_5[14]) begin
                _zz_when_ChannelPlugin_l129_14 <= _zz_when_ChannelPlugin_l129_16;
              end
              if(_zz_5[15]) begin
                _zz_when_ChannelPlugin_l129_15 <= _zz_when_ChannelPlugin_l129_16;
              end
            end
            if(when_RegStackPlugin_l144_79) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_148;
            end else begin
              if(when_RegStackPlugin_l146_79) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_148;
              end else begin
                if(when_RegStackPlugin_l148_79) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_148;
                end else begin
                  if(when_RegStackPlugin_l150_79) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_148;
                  end else begin
                    if(when_RegStackPlugin_l152_79) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_148;
                    end else begin
                      if(when_RegStackPlugin_l154_79) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_148;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_80) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_149;
            end else begin
              if(when_RegStackPlugin_l146_80) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_149;
              end else begin
                if(when_RegStackPlugin_l148_80) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_149;
                end else begin
                  if(when_RegStackPlugin_l150_80) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_149;
                  end else begin
                    if(when_RegStackPlugin_l152_80) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_149;
                    end else begin
                      if(when_RegStackPlugin_l154_80) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_149;
                      end
                    end
                  end
                end
              end
            end
          end
          ChannelOp_SETHDR : begin
            if(when_ChannelPlugin_l167) begin
              if(_zz_6[0]) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_125 <= _zz_when_ChannelPlugin_l127_2;
              end
              if(_zz_6[1]) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_126 <= _zz_when_ChannelPlugin_l127_2;
              end
              if(_zz_6[2]) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_127 <= _zz_when_ChannelPlugin_l127_2;
              end
              if(_zz_6[3]) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_128 <= _zz_when_ChannelPlugin_l127_2;
              end
              if(_zz_6[4]) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_129 <= _zz_when_ChannelPlugin_l127_2;
              end
              if(_zz_6[5]) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_130 <= _zz_when_ChannelPlugin_l127_2;
              end
              if(_zz_6[6]) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_131 <= _zz_when_ChannelPlugin_l127_2;
              end
              if(_zz_6[7]) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_132 <= _zz_when_ChannelPlugin_l127_2;
              end
              if(_zz_6[8]) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_133 <= _zz_when_ChannelPlugin_l127_2;
              end
              if(_zz_6[9]) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_134 <= _zz_when_ChannelPlugin_l127_2;
              end
              if(_zz_6[10]) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_135 <= _zz_when_ChannelPlugin_l127_2;
              end
              if(_zz_6[11]) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_136 <= _zz_when_ChannelPlugin_l127_2;
              end
              if(_zz_6[12]) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_137 <= _zz_when_ChannelPlugin_l127_2;
              end
              if(_zz_6[13]) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_138 <= _zz_when_ChannelPlugin_l127_2;
              end
              if(_zz_6[14]) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_139 <= _zz_when_ChannelPlugin_l127_2;
              end
              if(_zz_6[15]) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_140 <= _zz_when_ChannelPlugin_l127_2;
              end
            end
            if(when_RegStackPlugin_l144_81) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_150;
            end else begin
              if(when_RegStackPlugin_l146_81) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_150;
              end else begin
                if(when_RegStackPlugin_l148_81) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_150;
                end else begin
                  if(when_RegStackPlugin_l150_81) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_150;
                  end else begin
                    if(when_RegStackPlugin_l152_81) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_150;
                    end else begin
                      if(when_RegStackPlugin_l154_81) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_150;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_82) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_151;
            end else begin
              if(when_RegStackPlugin_l146_82) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_151;
              end else begin
                if(when_RegStackPlugin_l148_82) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_151;
                end else begin
                  if(when_RegStackPlugin_l150_82) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_151;
                  end else begin
                    if(when_RegStackPlugin_l152_82) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_151;
                    end else begin
                      if(when_RegStackPlugin_l154_82) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_151;
                      end
                    end
                  end
                end
              end
            end
          end
          ChannelOp_READHDR : begin
            if(when_ChannelPlugin_l177) begin
              if(when_RegStackPlugin_l144_83) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_152;
              end else begin
                if(when_RegStackPlugin_l146_83) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_152;
                end else begin
                  if(when_RegStackPlugin_l148_83) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_152;
                  end else begin
                    if(when_RegStackPlugin_l150_83) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_152;
                    end else begin
                      if(when_RegStackPlugin_l152_83) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_152;
                      end else begin
                        if(when_RegStackPlugin_l154_83) begin
                          _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_152;
                        end
                      end
                    end
                  end
                end
              end
            end else begin
              if(when_RegStackPlugin_l144_84) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_153;
              end else begin
                if(when_RegStackPlugin_l146_84) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_153;
                end else begin
                  if(when_RegStackPlugin_l148_84) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_153;
                  end else begin
                    if(when_RegStackPlugin_l150_84) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_153;
                    end else begin
                      if(when_RegStackPlugin_l152_84) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_153;
                      end else begin
                        if(when_RegStackPlugin_l154_84) begin
                          _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_153;
                        end
                      end
                    end
                  end
                end
              end
            end
          end
          ChannelOp_MKRC : begin
            if(when_ChannelPlugin_l190) begin
              _zz_when_ChannelPlugin_l127[_zz_when_ChannelPlugin_l127_3] <= 1'b1;
            end
            if(when_RegStackPlugin_l144_85) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_154;
            end else begin
              if(when_RegStackPlugin_l146_85) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_154;
              end else begin
                if(when_RegStackPlugin_l148_85) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_154;
                end else begin
                  if(when_RegStackPlugin_l150_85) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_154;
                  end else begin
                    if(when_RegStackPlugin_l152_85) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_154;
                    end else begin
                      if(when_RegStackPlugin_l154_85) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_154;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_86) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_155;
            end else begin
              if(when_RegStackPlugin_l146_86) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_155;
              end else begin
                if(when_RegStackPlugin_l148_86) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_155;
                end else begin
                  if(when_RegStackPlugin_l150_86) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_155;
                  end else begin
                    if(when_RegStackPlugin_l152_86) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_155;
                    end else begin
                      if(when_RegStackPlugin_l154_86) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_155;
                      end
                    end
                  end
                end
              end
            end
          end
          ChannelOp_UNMKRC : begin
            if(when_ChannelPlugin_l200) begin
              _zz_when_ChannelPlugin_l127[_zz_when_ChannelPlugin_l127_3] <= 1'b0;
            end
            if(when_RegStackPlugin_l144_87) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_156;
            end else begin
              if(when_RegStackPlugin_l146_87) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_156;
              end else begin
                if(when_RegStackPlugin_l148_87) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_156;
                end else begin
                  if(when_RegStackPlugin_l150_87) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_156;
                  end else begin
                    if(when_RegStackPlugin_l152_87) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_156;
                    end else begin
                      if(when_RegStackPlugin_l154_87) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_156;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_88) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_157;
            end else begin
              if(when_RegStackPlugin_l146_88) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_157;
              end else begin
                if(when_RegStackPlugin_l148_88) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_157;
                end else begin
                  if(when_RegStackPlugin_l150_88) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_157;
                  end else begin
                    if(when_RegStackPlugin_l152_88) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_157;
                    end else begin
                      if(when_RegStackPlugin_l154_88) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_157;
                      end
                    end
                  end
                end
              end
            end
          end
          default : begin
          end
        endcase
      end
      if(when_InterruptPlugin_l118) begin
        case(switch_InterruptPlugin_l123)
          InterruptOp_INTDIS : begin
          end
          InterruptOp_INTENB : begin
          end
          InterruptOp_LDTRAPPED : begin
            if(when_RegStackPlugin_l144_89) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_164;
            end else begin
              if(when_RegStackPlugin_l146_89) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_164;
              end else begin
                if(when_RegStackPlugin_l148_89) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_164;
                end else begin
                  if(when_RegStackPlugin_l150_89) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_164;
                  end else begin
                    if(when_RegStackPlugin_l152_89) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_164;
                    end else begin
                      if(when_RegStackPlugin_l154_89) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_164;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_90) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_165;
            end else begin
              if(when_RegStackPlugin_l146_90) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_165;
              end else begin
                if(when_RegStackPlugin_l148_90) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_165;
                end else begin
                  if(when_RegStackPlugin_l150_90) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_165;
                  end else begin
                    if(when_RegStackPlugin_l152_90) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_165;
                    end else begin
                      if(when_RegStackPlugin_l154_90) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_165;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_91) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_166;
            end else begin
              if(when_RegStackPlugin_l146_91) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_166;
              end else begin
                if(when_RegStackPlugin_l148_91) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_166;
                end else begin
                  if(when_RegStackPlugin_l150_91) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_166;
                  end else begin
                    if(when_RegStackPlugin_l152_91) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_166;
                    end else begin
                      if(when_RegStackPlugin_l154_91) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_166;
                      end
                    end
                  end
                end
              end
            end
          end
          InterruptOp_STTRAPPED : begin
            _zz_fpu_fpPipe_ctrl_0_down_RESULT_158 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_160;
            if(when_RegStackPlugin_l144_92) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_167;
            end else begin
              if(when_RegStackPlugin_l146_92) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_167;
              end else begin
                if(when_RegStackPlugin_l148_92) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_167;
                end else begin
                  if(when_RegStackPlugin_l150_92) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_167;
                  end else begin
                    if(when_RegStackPlugin_l152_92) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_167;
                    end else begin
                      if(when_RegStackPlugin_l154_92) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_167;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_93) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_168;
            end else begin
              if(when_RegStackPlugin_l146_93) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_168;
              end else begin
                if(when_RegStackPlugin_l148_93) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_168;
                end else begin
                  if(when_RegStackPlugin_l150_93) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_168;
                  end else begin
                    if(when_RegStackPlugin_l152_93) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_168;
                    end else begin
                      if(when_RegStackPlugin_l154_93) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_168;
                      end
                    end
                  end
                end
              end
            end
          end
          InterruptOp_LDSHADOW : begin
          end
          InterruptOp_STSHADOW : begin
          end
          InterruptOp_RESTART : begin
          end
          default : begin
          end
        endcase
      end
      if(when_ResourcePlugin_l114) begin
        case(switch_ResourcePlugin_l119)
          ResourceOp_GRANT : begin
            if(when_RegStackPlugin_l144_94) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_176;
            end else begin
              if(when_RegStackPlugin_l146_94) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_176;
              end else begin
                if(when_RegStackPlugin_l148_94) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_176;
                end else begin
                  if(when_RegStackPlugin_l150_94) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_176;
                  end else begin
                    if(when_RegStackPlugin_l152_94) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_176;
                    end else begin
                      if(when_RegStackPlugin_l154_94) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_176;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_95) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_177;
            end else begin
              if(when_RegStackPlugin_l146_95) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_177;
              end else begin
                if(when_RegStackPlugin_l148_95) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_177;
                end else begin
                  if(when_RegStackPlugin_l150_95) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_177;
                  end else begin
                    if(when_RegStackPlugin_l152_95) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_177;
                    end else begin
                      if(when_RegStackPlugin_l154_95) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_177;
                      end
                    end
                  end
                end
              end
            end
          end
          ResourceOp_ENBG : begin
          end
          ResourceOp_DISG : begin
          end
          ResourceOp_MKRC : begin
            if(when_RegStackPlugin_l144_96) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_178;
            end else begin
              if(when_RegStackPlugin_l146_96) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_178;
              end else begin
                if(when_RegStackPlugin_l148_96) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_178;
                end else begin
                  if(when_RegStackPlugin_l150_96) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_178;
                  end else begin
                    if(when_RegStackPlugin_l152_96) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_178;
                    end else begin
                      if(when_RegStackPlugin_l154_96) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_178;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_97) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_179;
            end else begin
              if(when_RegStackPlugin_l146_97) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_179;
              end else begin
                if(when_RegStackPlugin_l148_97) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_179;
                end else begin
                  if(when_RegStackPlugin_l150_97) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_179;
                  end else begin
                    if(when_RegStackPlugin_l152_97) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_179;
                    end else begin
                      if(when_RegStackPlugin_l154_97) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_179;
                      end
                    end
                  end
                end
              end
            end
          end
          ResourceOp_UNMKRC : begin
            if(when_RegStackPlugin_l144_98) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_180;
            end else begin
              if(when_RegStackPlugin_l146_98) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_180;
              end else begin
                if(when_RegStackPlugin_l148_98) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_180;
                end else begin
                  if(when_RegStackPlugin_l150_98) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_180;
                  end else begin
                    if(when_RegStackPlugin_l152_98) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_180;
                    end else begin
                      if(when_RegStackPlugin_l154_98) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_180;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_99) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_181;
            end else begin
              if(when_RegStackPlugin_l146_99) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_181;
              end else begin
                if(when_RegStackPlugin_l148_99) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_181;
                end else begin
                  if(when_RegStackPlugin_l150_99) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_181;
                  end else begin
                    if(when_RegStackPlugin_l152_99) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_181;
                    end else begin
                      if(when_RegStackPlugin_l154_99) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_181;
                      end
                    end
                  end
                end
              end
            end
          end
          ResourceOp_IRDSQ : begin
            if(when_RegStackPlugin_l144_100) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_182;
            end else begin
              if(when_RegStackPlugin_l146_100) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_182;
              end else begin
                if(when_RegStackPlugin_l148_100) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_182;
                end else begin
                  if(when_RegStackPlugin_l150_100) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_182;
                  end else begin
                    if(when_RegStackPlugin_l152_100) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_182;
                    end else begin
                      if(when_RegStackPlugin_l154_100) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_182;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_101) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_183;
            end else begin
              if(when_RegStackPlugin_l146_101) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_183;
              end else begin
                if(when_RegStackPlugin_l148_101) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_183;
                end else begin
                  if(when_RegStackPlugin_l150_101) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_183;
                  end else begin
                    if(when_RegStackPlugin_l152_101) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_183;
                    end else begin
                      if(when_RegStackPlugin_l154_101) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_183;
                      end
                    end
                  end
                end
              end
            end
          end
          ResourceOp_ERDSQ : begin
          end
          ResourceOp_STRESPTR : begin
            _zz_fpu_fpPipe_ctrl_0_down_RESULT_169 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_171;
            if(when_RegStackPlugin_l144_102) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_184;
            end else begin
              if(when_RegStackPlugin_l146_102) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_184;
              end else begin
                if(when_RegStackPlugin_l148_102) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_184;
                end else begin
                  if(when_RegStackPlugin_l150_102) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_184;
                  end else begin
                    if(when_RegStackPlugin_l152_102) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_184;
                    end else begin
                      if(when_RegStackPlugin_l154_102) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_184;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_103) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_185;
            end else begin
              if(when_RegStackPlugin_l146_103) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_185;
              end else begin
                if(when_RegStackPlugin_l148_103) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_185;
                end else begin
                  if(when_RegStackPlugin_l150_103) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_185;
                  end else begin
                    if(when_RegStackPlugin_l152_103) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_185;
                    end else begin
                      if(when_RegStackPlugin_l154_103) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_185;
                      end
                    end
                  end
                end
              end
            end
          end
          default : begin
            if(when_RegStackPlugin_l144_104) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_186;
            end else begin
              if(when_RegStackPlugin_l146_104) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_186;
              end else begin
                if(when_RegStackPlugin_l148_104) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_186;
                end else begin
                  if(when_RegStackPlugin_l150_104) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_186;
                  end else begin
                    if(when_RegStackPlugin_l152_104) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_186;
                    end else begin
                      if(when_RegStackPlugin_l154_104) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_186;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_105) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_187;
            end else begin
              if(when_RegStackPlugin_l146_105) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_187;
              end else begin
                if(when_RegStackPlugin_l148_105) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_187;
                end else begin
                  if(when_RegStackPlugin_l150_105) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_187;
                  end else begin
                    if(when_RegStackPlugin_l152_105) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_187;
                    end else begin
                      if(when_RegStackPlugin_l154_105) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_187;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_106) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_188;
            end else begin
              if(when_RegStackPlugin_l146_106) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_188;
              end else begin
                if(when_RegStackPlugin_l148_106) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_188;
                end else begin
                  if(when_RegStackPlugin_l150_106) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_188;
                  end else begin
                    if(when_RegStackPlugin_l152_106) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_188;
                    end else begin
                      if(when_RegStackPlugin_l154_106) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_188;
                      end
                    end
                  end
                end
              end
            end
          end
        endcase
      end
      if(when_SystemPlugin_l123) begin
        case(switch_SystemPlugin_l128)
          SystemOp_TESTPRANAL : begin
            if(when_RegStackPlugin_l144_107) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_197;
            end else begin
              if(when_RegStackPlugin_l146_107) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_197;
              end else begin
                if(when_RegStackPlugin_l148_107) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_197;
                end else begin
                  if(when_RegStackPlugin_l150_107) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_197;
                  end else begin
                    if(when_RegStackPlugin_l152_107) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_197;
                    end else begin
                      if(when_RegStackPlugin_l154_107) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_197;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_108) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_198;
            end else begin
              if(when_RegStackPlugin_l146_108) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_198;
              end else begin
                if(when_RegStackPlugin_l148_108) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_198;
                end else begin
                  if(when_RegStackPlugin_l150_108) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_198;
                  end else begin
                    if(when_RegStackPlugin_l152_108) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_198;
                    end else begin
                      if(when_RegStackPlugin_l154_108) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_198;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_109) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_199;
            end else begin
              if(when_RegStackPlugin_l146_109) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_199;
              end else begin
                if(when_RegStackPlugin_l148_109) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_199;
                end else begin
                  if(when_RegStackPlugin_l150_109) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_199;
                  end else begin
                    if(when_RegStackPlugin_l152_109) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_199;
                    end else begin
                      if(when_RegStackPlugin_l154_109) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_199;
                      end
                    end
                  end
                end
              end
            end
          end
          SystemOp_LDCONF : begin
            if(when_RegStackPlugin_l144_110) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_201;
            end else begin
              if(when_RegStackPlugin_l146_110) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_201;
              end else begin
                if(when_RegStackPlugin_l148_110) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_201;
                end else begin
                  if(when_RegStackPlugin_l150_110) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_201;
                  end else begin
                    if(when_RegStackPlugin_l152_110) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_201;
                    end else begin
                      if(when_RegStackPlugin_l154_110) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_201;
                      end
                    end
                  end
                end
              end
            end
          end
          SystemOp_STCONF : begin
            case(switch_SystemPlugin_l163)
              4'b0000 : begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_190 <= _zz_switch_SystemPlugin_l145_1[7 : 0];
              end
              4'b0001 : begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_191 <= _zz_switch_SystemPlugin_l145_1;
              end
              4'b0010 : begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_192 <= _zz_switch_SystemPlugin_l145_1[15 : 0];
              end
              4'b0011 : begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_193 <= _zz_switch_SystemPlugin_l145_1[15 : 0];
              end
              4'b0100 : begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_194 <= _zz_switch_SystemPlugin_l145_1[15 : 0];
              end
              default : begin
              end
            endcase
            if(when_RegStackPlugin_l144_111) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_202;
            end else begin
              if(when_RegStackPlugin_l146_111) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_202;
              end else begin
                if(when_RegStackPlugin_l148_111) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_202;
                end else begin
                  if(when_RegStackPlugin_l150_111) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_202;
                  end else begin
                    if(when_RegStackPlugin_l152_111) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_202;
                    end else begin
                      if(when_RegStackPlugin_l154_111) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_202;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_112) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_203;
            end else begin
              if(when_RegStackPlugin_l146_112) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_203;
              end else begin
                if(when_RegStackPlugin_l148_112) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_203;
                end else begin
                  if(when_RegStackPlugin_l150_112) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_203;
                  end else begin
                    if(when_RegStackPlugin_l152_112) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_203;
                    end else begin
                      if(when_RegStackPlugin_l154_112) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_203;
                      end
                    end
                  end
                end
              end
            end
          end
          SystemOp_SYSREQ : begin
            if(when_RegStackPlugin_l144_113) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_204;
            end else begin
              if(when_RegStackPlugin_l146_113) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_204;
              end else begin
                if(when_RegStackPlugin_l148_113) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_204;
                end else begin
                  if(when_RegStackPlugin_l150_113) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_204;
                  end else begin
                    if(when_RegStackPlugin_l152_113) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_204;
                    end else begin
                      if(when_RegStackPlugin_l154_113) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_204;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_114) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_205;
            end else begin
              if(when_RegStackPlugin_l146_114) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_205;
              end else begin
                if(when_RegStackPlugin_l148_114) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_205;
                end else begin
                  if(when_RegStackPlugin_l150_114) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_205;
                  end else begin
                    if(when_RegStackPlugin_l152_114) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_205;
                    end else begin
                      if(when_RegStackPlugin_l154_114) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_205;
                      end
                    end
                  end
                end
              end
            end
          end
          SystemOp_DEVMOVE : begin
            if(when_RegStackPlugin_l144_115) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_206;
            end else begin
              if(when_RegStackPlugin_l146_115) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_206;
              end else begin
                if(when_RegStackPlugin_l148_115) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_206;
                end else begin
                  if(when_RegStackPlugin_l150_115) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_206;
                  end else begin
                    if(when_RegStackPlugin_l152_115) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_206;
                    end else begin
                      if(when_RegStackPlugin_l154_115) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_206;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_116) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_207;
            end else begin
              if(when_RegStackPlugin_l146_116) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_207;
              end else begin
                if(when_RegStackPlugin_l148_116) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_207;
                end else begin
                  if(when_RegStackPlugin_l150_116) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_207;
                  end else begin
                    if(when_RegStackPlugin_l152_116) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_207;
                    end else begin
                      if(when_RegStackPlugin_l154_116) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_207;
                      end
                    end
                  end
                end
              end
            end
          end
          SystemOp_SETTIMESLICE : begin
            _zz_fpu_fpPipe_ctrl_0_down_RESULT_194 <= _zz_switch_SystemPlugin_l145_1[15 : 0];
            if(when_RegStackPlugin_l144_117) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_208;
            end else begin
              if(when_RegStackPlugin_l146_117) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_208;
              end else begin
                if(when_RegStackPlugin_l148_117) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_208;
                end else begin
                  if(when_RegStackPlugin_l150_117) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_208;
                  end else begin
                    if(when_RegStackPlugin_l152_117) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_208;
                    end else begin
                      if(when_RegStackPlugin_l154_117) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_208;
                      end
                    end
                  end
                end
              end
            end
            if(when_RegStackPlugin_l144_118) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_209;
            end else begin
              if(when_RegStackPlugin_l146_118) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_209;
              end else begin
                if(when_RegStackPlugin_l148_118) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_209;
                end else begin
                  if(when_RegStackPlugin_l150_118) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_209;
                  end else begin
                    if(when_RegStackPlugin_l152_118) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_209;
                    end else begin
                      if(when_RegStackPlugin_l154_118) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_209;
                      end
                    end
                  end
                end
              end
            end
          end
          default : begin
            if(when_RegStackPlugin_l144_119) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_3 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_211;
            end else begin
              if(when_RegStackPlugin_l146_119) begin
                _zz_fpu_fpPipe_ctrl_0_down_RESULT_4 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_211;
              end else begin
                if(when_RegStackPlugin_l148_119) begin
                  _zz_fpu_fpPipe_ctrl_0_down_RESULT_5 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_211;
                end else begin
                  if(when_RegStackPlugin_l150_119) begin
                    _zz_fpu_fpPipe_ctrl_0_down_RESULT_6 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_211;
                  end else begin
                    if(when_RegStackPlugin_l152_119) begin
                      _zz_fpu_fpPipe_ctrl_0_down_RESULT_7 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_211;
                    end else begin
                      if(when_RegStackPlugin_l154_119) begin
                        _zz_fpu_fpPipe_ctrl_0_down_RESULT_8 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_211;
                      end
                    end
                  end
                end
              end
            end
          end
        endcase
      end
      if(fpu_fpPipe_ctrl_0_up_isValid) begin
        if(!_zz_when_20) begin
          if((_zz_7 == 8'h8e)) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_213 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_214;
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_214 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_214;
          end else if((_zz_7 == 8'h8a)) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_213 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_214;
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_214 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_214;
          end else if((_zz_7 == 8'haa)) begin
          end else if((_zz_7 == _zz_112)) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_212 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_213;
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_213 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_212;
          end else if((_zz_7 == _zz_114)) begin
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_214 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_213;
              _zz_fpu_fpPipe_ctrl_0_down_RESULT_213 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_212;
          end else if((_zz_7 == _zz_116)) begin
              _zz_switch_Misc_l245_1 <= 2'b00;
          end else if((_zz_7 == _zz_118)) begin
              _zz_switch_Misc_l245_1 <= 2'b01;
          end else if((_zz_7 == _zz_120)) begin
              _zz_switch_Misc_l245_1 <= 2'b10;
          end else if((_zz_7 == _zz_122)) begin
              _zz_switch_Misc_l245_1 <= 2'b11;
          end else if((_zz_7 == _zz_124) || (_zz_7 == _zz_126) || (_zz_7 == _zz_128) || (_zz_7 == _zz_130) || (_zz_7 == _zz_132) || (_zz_7 == _zz_134) || (_zz_7 == _zz_136)) begin
          end else if((_zz_7 == _zz_138)) begin
          end else if((_zz_7 == _zz_140)) begin
          end else if((_zz_7 == _zz_142)) begin
          end else if((_zz_7 == _zz_144)) begin
          end else if((_zz_7 == _zz_146)) begin
          end else if((_zz_7 == _zz_148)) begin
          end else if((_zz_7 == _zz_150)) begin
          end else if((_zz_7 == _zz_152)) begin
          end else if((_zz_7 == _zz_154)) begin
          end else if((_zz_7 == _zz_156)) begin
          end else if((_zz_7 == _zz_158)) begin
          end else if((_zz_7 == _zz_160)) begin
          end else if((_zz_7 == _zz_162)) begin
          end else if((_zz_7 == _zz_164)) begin
          end else if((_zz_7 == _zz_166)) begin
          end else if((_zz_7 == _zz_168)) begin
              _zz_switch_Misc_l245_1 <= 2'b00;
          end else if((_zz_7 == _zz_170)) begin
              _zz_switch_Misc_l245_1 <= 2'b10;
          end else if((_zz_7 == _zz_172)) begin
              _zz_switch_Misc_l245_1 <= 2'b11;
          end else if((_zz_7 == _zz_174)) begin
              _zz_switch_Misc_l245_1 <= 2'b01;
          end else if((_zz_7 == 8'h41)) begin
          end else if((_zz_7 == 8'h42)) begin
          end else if((_zz_7 == 8'h43)) begin
          end else if((_zz_7 == 8'h5f)) begin
          end else if((_zz_7 == 8'h90)) begin
          end
        end
        if(when_FpuPlugin_l492) begin
          _zz_fpu_fpPipe_ctrl_0_down_RESULT_212 <= fpu_fpPipe_ctrl_0_down_RESULT;
          _zz_fpu_fpPipe_ctrl_0_down_RESULT_213 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_214;
          _zz_fpu_fpPipe_ctrl_0_down_RESULT_214 <= _zz_fpu_fpPipe_ctrl_0_down_RESULT_214;
        end
        if(1'b0) begin
          _zz_fpu_fpPipe_ctrl_0_down_RESULT_212 <= 64'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;
          _zz_fpu_fpPipe_ctrl_0_down_RESULT_213 <= 64'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;
        end
      end
      if(fpu_fpPipe_ctrl_0_up_isFiring) begin
        _zz_fpu_fpPipe_ctrl_0_down_CYCLE_CNT <= _zz_fpu_fpPipe_ctrl_0_down_MAX_CYCLES;
      end else begin
        if(when_FpuPlugin_l512) begin
          _zz_fpu_fpPipe_ctrl_0_down_CYCLE_CNT <= (_zz_fpu_fpPipe_ctrl_0_down_CYCLE_CNT - 10'h001);
        end else begin
          _zz_fpu_fpPipe_ctrl_0_down_CYCLE_CNT <= 10'h0;
        end
      end
      if(when_SchedulerPlugin_l131) begin
        if(streamFifo_2_io_pop_valid) begin
          _zz_when_SchedulerPlugin_l170 <= _zz_when_SchedulerPlugin_l170_1;
          _zz_when_SchedulerPlugin_l170_1 <= streamFifo_2_io_pop_payload;
          _zz_when_SchedulerPlugin_l168 <= ProcessState_RUNNING;
          _zz_when_SchedulerPlugin_l131 <= 1'b0;
          _zz_when_SchedulerPlugin_l131_1 <= 1'b0;
        end else begin
          if(streamFifo_3_io_pop_valid) begin
            _zz_when_SchedulerPlugin_l170 <= _zz_when_SchedulerPlugin_l170_1;
            _zz_when_SchedulerPlugin_l170_1 <= streamFifo_3_io_pop_payload;
            _zz_when_SchedulerPlugin_l168 <= ProcessState_RUNNING;
            _zz_when_SchedulerPlugin_l131 <= 1'b0;
            _zz_when_SchedulerPlugin_l131_1 <= 1'b0;
          end else begin
            _zz_when_SchedulerPlugin_l168 <= ProcessState_WAITING;
            _zz_when_SchedulerPlugin_l131_2 <= 1'b0;
          end
        end
      end
      if(when_SchedulerPlugin_l163) begin
        _zz_when_SchedulerPlugin_l131_2 <= 1'b1;
      end
      if(when_SchedulerPlugin_l168) begin
        if(when_SchedulerPlugin_l170) begin
          _zz_when_SchedulerPlugin_l131_1 <= 1'b1;
        end
      end
    end
  end


endmodule

//StreamFifo_1 replaced by StreamFifo

module StreamFifo (
  input  wire          io_push_valid,
  output wire          io_push_ready,
  input  wire [31:0]   io_push_payload,
  output wire          io_pop_valid,
  input  wire          io_pop_ready,
  output wire [31:0]   io_pop_payload,
  input  wire          io_flush,
  output wire [4:0]    io_occupancy,
  output wire [4:0]    io_availability,
  input  wire          clk,
  input  wire          reset
);

  reg        [31:0]   logic_ram_spinal_port1;
  wire       [31:0]   _zz_logic_ram_port;
  reg                 _zz_1;
  wire                logic_ptr_doPush;
  wire                logic_ptr_doPop;
  wire                logic_ptr_full;
  wire                logic_ptr_empty;
  reg        [4:0]    logic_ptr_push;
  reg        [4:0]    logic_ptr_pop;
  wire       [4:0]    logic_ptr_occupancy;
  wire       [4:0]    logic_ptr_popOnIo;
  wire                when_Stream_l1383;
  reg                 logic_ptr_wentUp;
  wire                io_push_fire;
  wire                logic_push_onRam_write_valid;
  wire       [3:0]    logic_push_onRam_write_payload_address;
  wire       [31:0]   logic_push_onRam_write_payload_data;
  wire                logic_pop_addressGen_valid;
  reg                 logic_pop_addressGen_ready;
  wire       [3:0]    logic_pop_addressGen_payload;
  wire                logic_pop_addressGen_fire;
  wire                logic_pop_sync_readArbitation_valid;
  wire                logic_pop_sync_readArbitation_ready;
  wire       [3:0]    logic_pop_sync_readArbitation_payload;
  reg                 logic_pop_addressGen_rValid;
  reg        [3:0]    logic_pop_addressGen_rData;
  wire                when_Stream_l448;
  wire                logic_pop_sync_readPort_cmd_valid;
  wire       [3:0]    logic_pop_sync_readPort_cmd_payload;
  wire       [31:0]   logic_pop_sync_readPort_rsp;
  wire                logic_pop_addressGen_toFlowFire_valid;
  wire       [3:0]    logic_pop_addressGen_toFlowFire_payload;
  wire                logic_pop_sync_readArbitation_translated_valid;
  wire                logic_pop_sync_readArbitation_translated_ready;
  wire       [31:0]   logic_pop_sync_readArbitation_translated_payload;
  wire                logic_pop_sync_readArbitation_fire;
  reg        [4:0]    logic_pop_sync_popReg;
  reg [31:0] logic_ram [0:15];

  assign _zz_logic_ram_port = logic_push_onRam_write_payload_data;
  always @(posedge clk) begin
    if(_zz_1) begin
      logic_ram[logic_push_onRam_write_payload_address] <= _zz_logic_ram_port;
    end
  end

  always @(posedge clk) begin
    if(logic_pop_sync_readPort_cmd_valid) begin
      logic_ram_spinal_port1 <= logic_ram[logic_pop_sync_readPort_cmd_payload];
    end
  end

  always @(*) begin
    _zz_1 = 1'b0;
    if(logic_push_onRam_write_valid) begin
      _zz_1 = 1'b1;
    end
  end

  assign when_Stream_l1383 = (logic_ptr_doPush != logic_ptr_doPop);
  assign logic_ptr_full = (((logic_ptr_push ^ logic_ptr_popOnIo) ^ 5'h10) == 5'h0);
  assign logic_ptr_empty = (logic_ptr_push == logic_ptr_pop);
  assign logic_ptr_occupancy = (logic_ptr_push - logic_ptr_popOnIo);
  assign io_push_ready = (! logic_ptr_full);
  assign io_push_fire = (io_push_valid && io_push_ready);
  assign logic_ptr_doPush = io_push_fire;
  assign logic_push_onRam_write_valid = io_push_fire;
  assign logic_push_onRam_write_payload_address = logic_ptr_push[3:0];
  assign logic_push_onRam_write_payload_data = io_push_payload;
  assign logic_pop_addressGen_valid = (! logic_ptr_empty);
  assign logic_pop_addressGen_payload = logic_ptr_pop[3:0];
  assign logic_pop_addressGen_fire = (logic_pop_addressGen_valid && logic_pop_addressGen_ready);
  assign logic_ptr_doPop = logic_pop_addressGen_fire;
  always @(*) begin
    logic_pop_addressGen_ready = logic_pop_sync_readArbitation_ready;
    if(when_Stream_l448) begin
      logic_pop_addressGen_ready = 1'b1;
    end
  end

  assign when_Stream_l448 = (! logic_pop_sync_readArbitation_valid);
  assign logic_pop_sync_readArbitation_valid = logic_pop_addressGen_rValid;
  assign logic_pop_sync_readArbitation_payload = logic_pop_addressGen_rData;
  assign logic_pop_sync_readPort_rsp = logic_ram_spinal_port1;
  assign logic_pop_addressGen_toFlowFire_valid = logic_pop_addressGen_fire;
  assign logic_pop_addressGen_toFlowFire_payload = logic_pop_addressGen_payload;
  assign logic_pop_sync_readPort_cmd_valid = logic_pop_addressGen_toFlowFire_valid;
  assign logic_pop_sync_readPort_cmd_payload = logic_pop_addressGen_toFlowFire_payload;
  assign logic_pop_sync_readArbitation_translated_valid = logic_pop_sync_readArbitation_valid;
  assign logic_pop_sync_readArbitation_ready = logic_pop_sync_readArbitation_translated_ready;
  assign logic_pop_sync_readArbitation_translated_payload = logic_pop_sync_readPort_rsp;
  assign io_pop_valid = logic_pop_sync_readArbitation_translated_valid;
  assign logic_pop_sync_readArbitation_translated_ready = io_pop_ready;
  assign io_pop_payload = logic_pop_sync_readArbitation_translated_payload;
  assign logic_pop_sync_readArbitation_fire = (logic_pop_sync_readArbitation_valid && logic_pop_sync_readArbitation_ready);
  assign logic_ptr_popOnIo = logic_pop_sync_popReg;
  assign io_occupancy = logic_ptr_occupancy;
  assign io_availability = (5'h10 - logic_ptr_occupancy);
  always @(posedge clk) begin
    if(reset) begin
      logic_ptr_push <= 5'h0;
      logic_ptr_pop <= 5'h0;
      logic_ptr_wentUp <= 1'b0;
      logic_pop_addressGen_rValid <= 1'b0;
      logic_pop_sync_popReg <= 5'h0;
    end else begin
      if(when_Stream_l1383) begin
        logic_ptr_wentUp <= logic_ptr_doPush;
      end
      if(io_flush) begin
        logic_ptr_wentUp <= 1'b0;
      end
      if(logic_ptr_doPush) begin
        logic_ptr_push <= (logic_ptr_push + 5'h01);
      end
      if(logic_ptr_doPop) begin
        logic_ptr_pop <= (logic_ptr_pop + 5'h01);
      end
      if(io_flush) begin
        logic_ptr_push <= 5'h0;
        logic_ptr_pop <= 5'h0;
      end
      if(logic_pop_addressGen_ready) begin
        logic_pop_addressGen_rValid <= logic_pop_addressGen_valid;
      end
      if(io_flush) begin
        logic_pop_addressGen_rValid <= 1'b0;
      end
      if(logic_pop_sync_readArbitation_fire) begin
        logic_pop_sync_popReg <= logic_ptr_pop;
      end
      if(io_flush) begin
        logic_pop_sync_popReg <= 5'h0;
      end
    end
  end

  always @(posedge clk) begin
    if(logic_pop_addressGen_ready) begin
      logic_pop_addressGen_rData <= logic_pop_addressGen_payload;
    end
  end


endmodule

module FpuAdder (
  input  wire          io_cmd_valid,
  output wire          io_cmd_ready,
  input  wire [63:0]   io_cmd_payload_a,
  input  wire [63:0]   io_cmd_payload_b,
  input  wire          io_cmd_payload_sub,
  input  wire [1:0]    io_cmd_payload_rounding,
  output wire          io_rsp_valid,
  input  wire          io_rsp_ready,
  output wire [63:0]   io_rsp_payload,
  input  wire          clk,
  input  wire          reset
);

  wire       [10:0]   _zz__zz_pipe_ctrl_0_down_MB;
  wire       [10:0]   _zz__zz_pipe_ctrl_0_down_MA;
  wire       [10:0]   _zz_pipe_ctrl_0_down_MA_2;
  wire       [10:0]   _zz_pipe_ctrl_0_down_MA_3;
  wire       [10:0]   _zz_pipe_ctrl_0_down_MB_3;
  wire       [10:0]   _zz_pipe_ctrl_0_down_MB_4;
  wire       [55:0]   _zz__zz_pipe_ctrl_1_down_RESULT_2;
  wire       [55:0]   _zz__zz_pipe_ctrl_1_down_RESULT_2_1;
  wire       [55:0]   _zz__zz_pipe_ctrl_1_down_RESULT_2_2;
  wire       [55:0]   _zz__zz_pipe_ctrl_1_down_RESULT_2_3;
  wire       [55:0]   _zz__zz_pipe_ctrl_1_down_RESULT_2_4;
  wire       [56:0]   _zz__zz_pipe_ctrl_1_down_RESULT_4;
  wire       [56:0]   _zz__zz_pipe_ctrl_1_down_RESULT_4_1;
  wire       [11:0]   _zz__zz_pipe_ctrl_1_down_RESULT_7;
  wire       [11:0]   _zz__zz_pipe_ctrl_1_down_RESULT_7_1;
  wire       [0:0]    _zz__zz_pipe_ctrl_1_down_RESULT_7_2;
  wire       [6:0]    _zz__zz_when_Adder_l123_189;
  wire       [6:0]    _zz__zz_when_Adder_l123_189_1;
  wire       [182:0]  _zz__zz_pipe_ctrl_1_down_RESULT_8;
  wire       [11:0]   _zz__zz_pipe_ctrl_1_down_RESULT_9;
  wire       [11:0]   _zz__zz_pipe_ctrl_1_down_RESULT_9_1;
  wire       [6:0]    _zz__zz_pipe_ctrl_1_down_RESULT_10;
  wire       [55:0]   _zz__zz_pipe_ctrl_1_down_RESULT_8_1;
  wire       [11:0]   _zz__zz_pipe_ctrl_1_down_RESULT_9_2;
  wire       [11:0]   _zz__zz_pipe_ctrl_1_down_RESULT_9_3;
  wire       [6:0]    _zz__zz_pipe_ctrl_1_down_RESULT_9_4;
  wire                _zz_when;
  wire       [11:0]   _zz__zz_pipe_ctrl_1_down_RESULT_13;
  wire       [11:0]   _zz__zz_pipe_ctrl_1_down_RESULT_12;
  wire       [11:0]   _zz__zz_pipe_ctrl_1_down_RESULT_12_1;
  wire       [11:0]   _zz__zz_pipe_ctrl_1_down_RESULT_11;
  wire       [51:0]   _zz__zz_pipe_ctrl_1_down_RESULT_21;
  wire       [0:0]    _zz__zz_pipe_ctrl_1_down_RESULT_21_1;
  wire       [10:0]   _zz_pipe_ctrl_1_down_RESULT_23;
  wire       [51:0]   _zz_pipe_ctrl_1_down_RESULT_24;
  wire       [50:0]   _zz_pipe_ctrl_1_down_RESULT_25;
  wire                pipe_ctrl_1_down_isReady;
  wire                pipe_ctrl_1_up_isValid;
  wire       [1:0]    pipe_ctrl_0_down_ROUND;
  wire                pipe_ctrl_0_down_SUB;
  wire                pipe_ctrl_0_down_isValid;
  wire                pipe_ctrl_0_down_isReady;
  reg        [52:0]   pipe_ctrl_1_up_MB;
  reg        [52:0]   pipe_ctrl_1_up_MA;
  reg        [10:0]   pipe_ctrl_1_up_EXP;
  reg                 pipe_ctrl_1_up_SB;
  reg                 pipe_ctrl_1_up_SA;
  reg        [1:0]    pipe_ctrl_1_up_ROUND;
  reg                 pipe_ctrl_1_up_SUB;
  wire                pipe_ctrl_1_down_valid;
  reg                 pipe_ctrl_1_up_valid;
  wire                pipe_ctrl_0_down_valid;
  wire                pipe_ctrl_0_up_ready;
  reg                 pipe_ctrl_0_down_ready;
  wire                pipe_ctrl_1_up_ready;
  wire                pipe_ctrl_1_down_ready;
  wire                pipe_ctrl_1_down_isValid;
  wire       [63:0]   pipe_ctrl_1_down_RESULT;
  wire       [1:0]    pipe_ctrl_1_down_ROUND;
  wire       [10:0]   pipe_ctrl_1_down_EXP;
  wire       [52:0]   pipe_ctrl_1_down_MB;
  wire       [52:0]   pipe_ctrl_1_down_MA;
  wire                pipe_ctrl_1_down_SUB;
  wire                pipe_ctrl_1_down_SB;
  wire                pipe_ctrl_1_down_SA;
  wire       [52:0]   pipe_ctrl_0_down_MB;
  wire       [52:0]   pipe_ctrl_0_down_MA;
  wire       [10:0]   pipe_ctrl_0_down_EXP;
  wire       [52:0]   pipe_ctrl_0_down_MB_RAW;
  wire       [52:0]   pipe_ctrl_0_down_MA_RAW;
  wire       [10:0]   pipe_ctrl_0_down_EB;
  wire       [10:0]   pipe_ctrl_0_down_EA;
  wire                pipe_ctrl_0_down_SB;
  wire                pipe_ctrl_0_down_SA;
  wire       [63:0]   pipe_ctrl_0_down_B;
  wire       [63:0]   pipe_ctrl_0_down_A;
  wire       [1:0]    pipe_ctrl_0_up_ROUND;
  wire                pipe_ctrl_0_up_SUB;
  wire       [63:0]   pipe_ctrl_0_up_B;
  wire       [63:0]   pipe_ctrl_0_up_A;
  wire                pipe_ctrl_0_up_isCancel;
  wire                pipe_ctrl_0_up_isReady;
  wire                pipe_ctrl_0_up_valid;
  wire       [10:0]   _zz_pipe_ctrl_0_down_MA_RAW;
  wire       [10:0]   _zz_pipe_ctrl_0_down_MB_RAW;
  wire       [10:0]   _zz_pipe_ctrl_0_down_MB;
  wire       [10:0]   _zz_pipe_ctrl_0_down_MA;
  wire       [5:0]    _zz_pipe_ctrl_0_down_MA_1;
  wire       [10:0]   _zz_pipe_ctrl_0_down_MB_1;
  wire       [5:0]    _zz_pipe_ctrl_0_down_MB_2;
  wire       [55:0]   _zz_pipe_ctrl_1_down_RESULT;
  wire       [55:0]   _zz_pipe_ctrl_1_down_RESULT_1;
  wire       [56:0]   _zz_pipe_ctrl_1_down_RESULT_2;
  wire                _zz_pipe_ctrl_1_down_RESULT_3;
  wire       [56:0]   _zz_pipe_ctrl_1_down_RESULT_4;
  wire                _zz_pipe_ctrl_1_down_RESULT_5;
  reg        [55:0]   _zz_pipe_ctrl_1_down_RESULT_6;
  wire       [11:0]   _zz_pipe_ctrl_1_down_RESULT_7;
  wire       [63:0]   _zz_when_Adder_l123;
  wire       [31:0]   _zz_when_Adder_l123_1;
  wire       [15:0]   _zz_when_Adder_l123_2;
  wire       [7:0]    _zz_when_Adder_l123_3;
  wire       [3:0]    _zz_when_Adder_l123_4;
  wire       [1:0]    _zz_when_Adder_l123_5;
  wire       [0:0]    _zz_when_Adder_l123_6;
  wire       [0:0]    _zz_when_Adder_l123_7;
  wire       [1:0]    _zz_when_Adder_l123_8;
  wire       [1:0]    _zz_when_Adder_l123_9;
  wire       [0:0]    _zz_when_Adder_l123_10;
  wire       [0:0]    _zz_when_Adder_l123_11;
  wire       [1:0]    _zz_when_Adder_l123_12;
  wire       [2:0]    _zz_when_Adder_l123_13;
  wire       [3:0]    _zz_when_Adder_l123_14;
  wire       [1:0]    _zz_when_Adder_l123_15;
  wire       [0:0]    _zz_when_Adder_l123_16;
  wire       [0:0]    _zz_when_Adder_l123_17;
  wire       [1:0]    _zz_when_Adder_l123_18;
  wire       [1:0]    _zz_when_Adder_l123_19;
  wire       [0:0]    _zz_when_Adder_l123_20;
  wire       [0:0]    _zz_when_Adder_l123_21;
  wire       [1:0]    _zz_when_Adder_l123_22;
  wire       [2:0]    _zz_when_Adder_l123_23;
  wire       [3:0]    _zz_when_Adder_l123_24;
  wire       [7:0]    _zz_when_Adder_l123_25;
  wire       [3:0]    _zz_when_Adder_l123_26;
  wire       [1:0]    _zz_when_Adder_l123_27;
  wire       [0:0]    _zz_when_Adder_l123_28;
  wire       [0:0]    _zz_when_Adder_l123_29;
  wire       [1:0]    _zz_when_Adder_l123_30;
  wire       [1:0]    _zz_when_Adder_l123_31;
  wire       [0:0]    _zz_when_Adder_l123_32;
  wire       [0:0]    _zz_when_Adder_l123_33;
  wire       [1:0]    _zz_when_Adder_l123_34;
  wire       [2:0]    _zz_when_Adder_l123_35;
  wire       [3:0]    _zz_when_Adder_l123_36;
  wire       [1:0]    _zz_when_Adder_l123_37;
  wire       [0:0]    _zz_when_Adder_l123_38;
  wire       [0:0]    _zz_when_Adder_l123_39;
  wire       [1:0]    _zz_when_Adder_l123_40;
  wire       [1:0]    _zz_when_Adder_l123_41;
  wire       [0:0]    _zz_when_Adder_l123_42;
  wire       [0:0]    _zz_when_Adder_l123_43;
  wire       [1:0]    _zz_when_Adder_l123_44;
  wire       [2:0]    _zz_when_Adder_l123_45;
  wire       [3:0]    _zz_when_Adder_l123_46;
  wire       [4:0]    _zz_when_Adder_l123_47;
  wire       [15:0]   _zz_when_Adder_l123_48;
  wire       [7:0]    _zz_when_Adder_l123_49;
  wire       [3:0]    _zz_when_Adder_l123_50;
  wire       [1:0]    _zz_when_Adder_l123_51;
  wire       [0:0]    _zz_when_Adder_l123_52;
  wire       [0:0]    _zz_when_Adder_l123_53;
  wire       [1:0]    _zz_when_Adder_l123_54;
  wire       [1:0]    _zz_when_Adder_l123_55;
  wire       [0:0]    _zz_when_Adder_l123_56;
  wire       [0:0]    _zz_when_Adder_l123_57;
  wire       [1:0]    _zz_when_Adder_l123_58;
  wire       [2:0]    _zz_when_Adder_l123_59;
  wire       [3:0]    _zz_when_Adder_l123_60;
  wire       [1:0]    _zz_when_Adder_l123_61;
  wire       [0:0]    _zz_when_Adder_l123_62;
  wire       [0:0]    _zz_when_Adder_l123_63;
  wire       [1:0]    _zz_when_Adder_l123_64;
  wire       [1:0]    _zz_when_Adder_l123_65;
  wire       [0:0]    _zz_when_Adder_l123_66;
  wire       [0:0]    _zz_when_Adder_l123_67;
  wire       [1:0]    _zz_when_Adder_l123_68;
  wire       [2:0]    _zz_when_Adder_l123_69;
  wire       [3:0]    _zz_when_Adder_l123_70;
  wire       [7:0]    _zz_when_Adder_l123_71;
  wire       [3:0]    _zz_when_Adder_l123_72;
  wire       [1:0]    _zz_when_Adder_l123_73;
  wire       [0:0]    _zz_when_Adder_l123_74;
  wire       [0:0]    _zz_when_Adder_l123_75;
  wire       [1:0]    _zz_when_Adder_l123_76;
  wire       [1:0]    _zz_when_Adder_l123_77;
  wire       [0:0]    _zz_when_Adder_l123_78;
  wire       [0:0]    _zz_when_Adder_l123_79;
  wire       [1:0]    _zz_when_Adder_l123_80;
  wire       [2:0]    _zz_when_Adder_l123_81;
  wire       [3:0]    _zz_when_Adder_l123_82;
  wire       [1:0]    _zz_when_Adder_l123_83;
  wire       [0:0]    _zz_when_Adder_l123_84;
  wire       [0:0]    _zz_when_Adder_l123_85;
  wire       [1:0]    _zz_when_Adder_l123_86;
  wire       [1:0]    _zz_when_Adder_l123_87;
  wire       [0:0]    _zz_when_Adder_l123_88;
  wire       [0:0]    _zz_when_Adder_l123_89;
  wire       [1:0]    _zz_when_Adder_l123_90;
  wire       [2:0]    _zz_when_Adder_l123_91;
  wire       [3:0]    _zz_when_Adder_l123_92;
  wire       [4:0]    _zz_when_Adder_l123_93;
  wire       [5:0]    _zz_when_Adder_l123_94;
  wire       [31:0]   _zz_when_Adder_l123_95;
  wire       [15:0]   _zz_when_Adder_l123_96;
  wire       [7:0]    _zz_when_Adder_l123_97;
  wire       [3:0]    _zz_when_Adder_l123_98;
  wire       [1:0]    _zz_when_Adder_l123_99;
  wire       [0:0]    _zz_when_Adder_l123_100;
  wire       [0:0]    _zz_when_Adder_l123_101;
  wire       [1:0]    _zz_when_Adder_l123_102;
  wire       [1:0]    _zz_when_Adder_l123_103;
  wire       [0:0]    _zz_when_Adder_l123_104;
  wire       [0:0]    _zz_when_Adder_l123_105;
  wire       [1:0]    _zz_when_Adder_l123_106;
  wire       [2:0]    _zz_when_Adder_l123_107;
  wire       [3:0]    _zz_when_Adder_l123_108;
  wire       [1:0]    _zz_when_Adder_l123_109;
  wire       [0:0]    _zz_when_Adder_l123_110;
  wire       [0:0]    _zz_when_Adder_l123_111;
  wire       [1:0]    _zz_when_Adder_l123_112;
  wire       [1:0]    _zz_when_Adder_l123_113;
  wire       [0:0]    _zz_when_Adder_l123_114;
  wire       [0:0]    _zz_when_Adder_l123_115;
  wire       [1:0]    _zz_when_Adder_l123_116;
  wire       [2:0]    _zz_when_Adder_l123_117;
  wire       [3:0]    _zz_when_Adder_l123_118;
  wire       [7:0]    _zz_when_Adder_l123_119;
  wire       [3:0]    _zz_when_Adder_l123_120;
  wire       [1:0]    _zz_when_Adder_l123_121;
  wire       [0:0]    _zz_when_Adder_l123_122;
  wire       [0:0]    _zz_when_Adder_l123_123;
  wire       [1:0]    _zz_when_Adder_l123_124;
  wire       [1:0]    _zz_when_Adder_l123_125;
  wire       [0:0]    _zz_when_Adder_l123_126;
  wire       [0:0]    _zz_when_Adder_l123_127;
  wire       [1:0]    _zz_when_Adder_l123_128;
  wire       [2:0]    _zz_when_Adder_l123_129;
  wire       [3:0]    _zz_when_Adder_l123_130;
  wire       [1:0]    _zz_when_Adder_l123_131;
  wire       [0:0]    _zz_when_Adder_l123_132;
  wire       [0:0]    _zz_when_Adder_l123_133;
  wire       [1:0]    _zz_when_Adder_l123_134;
  wire       [1:0]    _zz_when_Adder_l123_135;
  wire       [0:0]    _zz_when_Adder_l123_136;
  wire       [0:0]    _zz_when_Adder_l123_137;
  wire       [1:0]    _zz_when_Adder_l123_138;
  wire       [2:0]    _zz_when_Adder_l123_139;
  wire       [3:0]    _zz_when_Adder_l123_140;
  wire       [4:0]    _zz_when_Adder_l123_141;
  wire       [15:0]   _zz_when_Adder_l123_142;
  wire       [7:0]    _zz_when_Adder_l123_143;
  wire       [3:0]    _zz_when_Adder_l123_144;
  wire       [1:0]    _zz_when_Adder_l123_145;
  wire       [0:0]    _zz_when_Adder_l123_146;
  wire       [0:0]    _zz_when_Adder_l123_147;
  wire       [1:0]    _zz_when_Adder_l123_148;
  wire       [1:0]    _zz_when_Adder_l123_149;
  wire       [0:0]    _zz_when_Adder_l123_150;
  wire       [0:0]    _zz_when_Adder_l123_151;
  wire       [1:0]    _zz_when_Adder_l123_152;
  wire       [2:0]    _zz_when_Adder_l123_153;
  wire       [3:0]    _zz_when_Adder_l123_154;
  wire       [1:0]    _zz_when_Adder_l123_155;
  wire       [0:0]    _zz_when_Adder_l123_156;
  wire       [0:0]    _zz_when_Adder_l123_157;
  wire       [1:0]    _zz_when_Adder_l123_158;
  wire       [1:0]    _zz_when_Adder_l123_159;
  wire       [0:0]    _zz_when_Adder_l123_160;
  wire       [0:0]    _zz_when_Adder_l123_161;
  wire       [1:0]    _zz_when_Adder_l123_162;
  wire       [2:0]    _zz_when_Adder_l123_163;
  wire       [3:0]    _zz_when_Adder_l123_164;
  wire       [7:0]    _zz_when_Adder_l123_165;
  wire       [3:0]    _zz_when_Adder_l123_166;
  wire       [1:0]    _zz_when_Adder_l123_167;
  wire       [0:0]    _zz_when_Adder_l123_168;
  wire       [0:0]    _zz_when_Adder_l123_169;
  wire       [1:0]    _zz_when_Adder_l123_170;
  wire       [1:0]    _zz_when_Adder_l123_171;
  wire       [0:0]    _zz_when_Adder_l123_172;
  wire       [0:0]    _zz_when_Adder_l123_173;
  wire       [1:0]    _zz_when_Adder_l123_174;
  wire       [2:0]    _zz_when_Adder_l123_175;
  wire       [3:0]    _zz_when_Adder_l123_176;
  wire       [1:0]    _zz_when_Adder_l123_177;
  wire       [0:0]    _zz_when_Adder_l123_178;
  wire       [0:0]    _zz_when_Adder_l123_179;
  wire       [1:0]    _zz_when_Adder_l123_180;
  wire       [1:0]    _zz_when_Adder_l123_181;
  wire       [0:0]    _zz_when_Adder_l123_182;
  wire       [0:0]    _zz_when_Adder_l123_183;
  wire       [1:0]    _zz_when_Adder_l123_184;
  wire       [2:0]    _zz_when_Adder_l123_185;
  wire       [3:0]    _zz_when_Adder_l123_186;
  wire       [4:0]    _zz_when_Adder_l123_187;
  wire       [5:0]    _zz_when_Adder_l123_188;
  wire       [6:0]    _zz_when_Adder_l123_189;
  reg        [52:0]   _zz_pipe_ctrl_1_down_RESULT_8;
  reg        [11:0]   _zz_pipe_ctrl_1_down_RESULT_9;
  wire                when_Adder_l123;
  wire       [6:0]    _zz_pipe_ctrl_1_down_RESULT_10;
  reg        [10:0]   _zz_pipe_ctrl_1_down_RESULT_11;
  reg        [52:0]   _zz_pipe_ctrl_1_down_RESULT_12;
  wire       [11:0]   _zz_pipe_ctrl_1_down_RESULT_13;
  wire       [5:0]    _zz_pipe_ctrl_1_down_RESULT_14;
  wire       [52:0]   _zz_pipe_ctrl_1_down_RESULT_15;
  wire                _zz_pipe_ctrl_1_down_RESULT_16;
  wire                _zz_pipe_ctrl_1_down_RESULT_17;
  wire                _zz_pipe_ctrl_1_down_RESULT_18;
  wire       [51:0]   _zz_pipe_ctrl_1_down_RESULT_19;
  reg                 _zz_pipe_ctrl_1_down_RESULT_20;
  wire       [51:0]   _zz_pipe_ctrl_1_down_RESULT_21;
  wire                _zz_pipe_ctrl_1_down_RESULT_22;
  wire                when_StageLink_l67;

  assign _zz_when = ($signed(_zz_pipe_ctrl_1_down_RESULT_9) <= $signed(12'h0));
  assign _zz__zz_pipe_ctrl_0_down_MB = (pipe_ctrl_0_down_EA - pipe_ctrl_0_down_EB);
  assign _zz__zz_pipe_ctrl_0_down_MA = (- _zz_pipe_ctrl_0_down_MB);
  assign _zz_pipe_ctrl_0_down_MA_2 = {5'd0, _zz_pipe_ctrl_0_down_MA_1};
  assign _zz_pipe_ctrl_0_down_MA_3 = {5'd0, _zz_pipe_ctrl_0_down_MA_1};
  assign _zz_pipe_ctrl_0_down_MB_3 = {5'd0, _zz_pipe_ctrl_0_down_MB_2};
  assign _zz_pipe_ctrl_0_down_MB_4 = {5'd0, _zz_pipe_ctrl_0_down_MB_2};
  assign _zz__zz_pipe_ctrl_1_down_RESULT_2 = ($signed(_zz__zz_pipe_ctrl_1_down_RESULT_2_1) + $signed(_zz__zz_pipe_ctrl_1_down_RESULT_2_3));
  assign _zz__zz_pipe_ctrl_1_down_RESULT_2_1 = (pipe_ctrl_1_down_SA ? _zz__zz_pipe_ctrl_1_down_RESULT_2_2 : _zz_pipe_ctrl_1_down_RESULT);
  assign _zz__zz_pipe_ctrl_1_down_RESULT_2_2 = ((~ _zz_pipe_ctrl_1_down_RESULT) + 56'h00000000000001);
  assign _zz__zz_pipe_ctrl_1_down_RESULT_2_3 = ((pipe_ctrl_1_down_SB ^ pipe_ctrl_1_down_SUB) ? _zz__zz_pipe_ctrl_1_down_RESULT_2_4 : _zz_pipe_ctrl_1_down_RESULT_1);
  assign _zz__zz_pipe_ctrl_1_down_RESULT_2_4 = ((~ _zz_pipe_ctrl_1_down_RESULT_1) + 56'h00000000000001);
  assign _zz__zz_pipe_ctrl_1_down_RESULT_4 = ($signed(_zz__zz_pipe_ctrl_1_down_RESULT_4_1) + $signed(57'h000000000000001));
  assign _zz__zz_pipe_ctrl_1_down_RESULT_4_1 = (~ _zz_pipe_ctrl_1_down_RESULT_2);
  assign _zz__zz_pipe_ctrl_1_down_RESULT_7 = {1'd0, pipe_ctrl_1_down_EXP};
  assign _zz__zz_pipe_ctrl_1_down_RESULT_7_2 = _zz_pipe_ctrl_1_down_RESULT_5;
  assign _zz__zz_pipe_ctrl_1_down_RESULT_7_1 = {11'd0, _zz__zz_pipe_ctrl_1_down_RESULT_7_2};
  assign _zz__zz_when_Adder_l123_189 = _zz__zz_when_Adder_l123_189_1;
  assign _zz__zz_when_Adder_l123_189_1 = ({(_zz_when_Adder_l123_94[5] && _zz_when_Adder_l123_188[5]),((! _zz_when_Adder_l123_94[5]) ? {1'b0,_zz_when_Adder_l123_94[4 : 0]} : {(! _zz_when_Adder_l123_188[5]),_zz_when_Adder_l123_188[4 : 0]})} - 7'h08);
  assign _zz__zz_pipe_ctrl_1_down_RESULT_8 = ({127'd0,_zz_pipe_ctrl_1_down_RESULT_6} <<< _zz_when_Adder_l123_189);
  assign _zz__zz_pipe_ctrl_1_down_RESULT_9 = _zz_pipe_ctrl_1_down_RESULT_7;
  assign _zz__zz_pipe_ctrl_1_down_RESULT_9_1 = {{5{_zz_when_Adder_l123_189[6]}}, _zz_when_Adder_l123_189};
  assign _zz__zz_pipe_ctrl_1_down_RESULT_10 = (- _zz_when_Adder_l123_189);
  assign _zz__zz_pipe_ctrl_1_down_RESULT_8_1 = (_zz_pipe_ctrl_1_down_RESULT_6 >>> _zz_pipe_ctrl_1_down_RESULT_10);
  assign _zz__zz_pipe_ctrl_1_down_RESULT_9_2 = _zz_pipe_ctrl_1_down_RESULT_7;
  assign _zz__zz_pipe_ctrl_1_down_RESULT_9_4 = _zz_pipe_ctrl_1_down_RESULT_10;
  assign _zz__zz_pipe_ctrl_1_down_RESULT_9_3 = {{5{_zz__zz_pipe_ctrl_1_down_RESULT_9_4[6]}}, _zz__zz_pipe_ctrl_1_down_RESULT_9_4};
  assign _zz__zz_pipe_ctrl_1_down_RESULT_13 = ($signed(12'h001) - $signed(_zz_pipe_ctrl_1_down_RESULT_9));
  assign _zz__zz_pipe_ctrl_1_down_RESULT_12 = {6'd0, _zz_pipe_ctrl_1_down_RESULT_14};
  assign _zz__zz_pipe_ctrl_1_down_RESULT_12_1 = {6'd0, _zz_pipe_ctrl_1_down_RESULT_14};
  assign _zz__zz_pipe_ctrl_1_down_RESULT_11 = _zz_pipe_ctrl_1_down_RESULT_9;
  assign _zz__zz_pipe_ctrl_1_down_RESULT_21_1 = _zz_pipe_ctrl_1_down_RESULT_20;
  assign _zz__zz_pipe_ctrl_1_down_RESULT_21 = {51'd0, _zz__zz_pipe_ctrl_1_down_RESULT_21_1};
  assign _zz_pipe_ctrl_1_down_RESULT_23 = (_zz_pipe_ctrl_1_down_RESULT_11 + 11'h001);
  assign _zz_pipe_ctrl_1_down_RESULT_25 = (_zz_pipe_ctrl_1_down_RESULT_21 >>> 1'd1);
  assign _zz_pipe_ctrl_1_down_RESULT_24 = {1'd0, _zz_pipe_ctrl_1_down_RESULT_25};
  assign pipe_ctrl_0_up_valid = io_cmd_valid;
  assign io_cmd_ready = (pipe_ctrl_0_up_isReady || pipe_ctrl_0_up_isCancel);
  assign pipe_ctrl_0_up_A = io_cmd_payload_a;
  assign pipe_ctrl_0_up_B = io_cmd_payload_b;
  assign pipe_ctrl_0_up_SUB = io_cmd_payload_sub;
  assign pipe_ctrl_0_up_ROUND = io_cmd_payload_rounding;
  assign _zz_pipe_ctrl_0_down_MA_RAW = pipe_ctrl_0_down_A[62 : 52];
  assign _zz_pipe_ctrl_0_down_MB_RAW = pipe_ctrl_0_down_B[62 : 52];
  assign pipe_ctrl_0_down_SA = pipe_ctrl_0_down_A[63];
  assign pipe_ctrl_0_down_SB = pipe_ctrl_0_down_B[63];
  assign pipe_ctrl_0_down_EA = ((_zz_pipe_ctrl_0_down_MA_RAW == 11'h0) ? 11'h001 : _zz_pipe_ctrl_0_down_MA_RAW);
  assign pipe_ctrl_0_down_EB = ((_zz_pipe_ctrl_0_down_MB_RAW == 11'h0) ? 11'h001 : _zz_pipe_ctrl_0_down_MB_RAW);
  assign pipe_ctrl_0_down_MA_RAW = {((_zz_pipe_ctrl_0_down_MA_RAW == 11'h0) ? 1'b0 : 1'b1),pipe_ctrl_0_down_A[51 : 0]};
  assign pipe_ctrl_0_down_MB_RAW = {((_zz_pipe_ctrl_0_down_MB_RAW == 11'h0) ? 1'b0 : 1'b1),pipe_ctrl_0_down_B[51 : 0]};
  assign _zz_pipe_ctrl_0_down_MB = _zz__zz_pipe_ctrl_0_down_MB;
  assign _zz_pipe_ctrl_0_down_MA = _zz__zz_pipe_ctrl_0_down_MA;
  assign _zz_pipe_ctrl_0_down_MA_1 = 6'h3f;
  assign _zz_pipe_ctrl_0_down_MB_1 = _zz_pipe_ctrl_0_down_MB;
  assign _zz_pipe_ctrl_0_down_MB_2 = 6'h3f;
  assign pipe_ctrl_0_down_EXP = (($signed(11'h0) <= $signed(_zz_pipe_ctrl_0_down_MB)) ? pipe_ctrl_0_down_EA : pipe_ctrl_0_down_EB);
  assign pipe_ctrl_0_down_MA = (pipe_ctrl_0_down_MA_RAW >>> (($signed(_zz_pipe_ctrl_0_down_MB) < $signed(11'h0)) ? ((_zz_pipe_ctrl_0_down_MA < _zz_pipe_ctrl_0_down_MA_2) ? _zz_pipe_ctrl_0_down_MA : _zz_pipe_ctrl_0_down_MA_3) : 11'h0));
  assign pipe_ctrl_0_down_MB = (pipe_ctrl_0_down_MB_RAW >>> (($signed(11'h0) < $signed(_zz_pipe_ctrl_0_down_MB)) ? ((_zz_pipe_ctrl_0_down_MB_1 < _zz_pipe_ctrl_0_down_MB_3) ? _zz_pipe_ctrl_0_down_MB_1 : _zz_pipe_ctrl_0_down_MB_4) : 11'h0));
  assign _zz_pipe_ctrl_1_down_RESULT = {3'd0, pipe_ctrl_1_down_MA};
  assign _zz_pipe_ctrl_1_down_RESULT_1 = {3'd0, pipe_ctrl_1_down_MB};
  assign _zz_pipe_ctrl_1_down_RESULT_2 = {{1{_zz__zz_pipe_ctrl_1_down_RESULT_2[55]}}, _zz__zz_pipe_ctrl_1_down_RESULT_2};
  assign _zz_pipe_ctrl_1_down_RESULT_3 = _zz_pipe_ctrl_1_down_RESULT_2[56];
  assign _zz_pipe_ctrl_1_down_RESULT_4 = (_zz_pipe_ctrl_1_down_RESULT_3 ? _zz__zz_pipe_ctrl_1_down_RESULT_4 : _zz_pipe_ctrl_1_down_RESULT_2);
  assign _zz_pipe_ctrl_1_down_RESULT_5 = _zz_pipe_ctrl_1_down_RESULT_4[56];
  always @(*) begin
    if(_zz_pipe_ctrl_1_down_RESULT_5) begin
      _zz_pipe_ctrl_1_down_RESULT_6 = (_zz_pipe_ctrl_1_down_RESULT_4 >>> 1'd1);
    end else begin
      _zz_pipe_ctrl_1_down_RESULT_6 = _zz_pipe_ctrl_1_down_RESULT_4[55 : 0];
    end
  end

  assign _zz_pipe_ctrl_1_down_RESULT_7 = (_zz__zz_pipe_ctrl_1_down_RESULT_7 + _zz__zz_pipe_ctrl_1_down_RESULT_7_1);
  assign _zz_when_Adder_l123 = {8'h0,_zz_pipe_ctrl_1_down_RESULT_6};
  assign _zz_when_Adder_l123_1 = _zz_when_Adder_l123[63 : 32];
  assign _zz_when_Adder_l123_2 = _zz_when_Adder_l123_1[31 : 16];
  assign _zz_when_Adder_l123_3 = _zz_when_Adder_l123_2[15 : 8];
  assign _zz_when_Adder_l123_4 = _zz_when_Adder_l123_3[7 : 4];
  assign _zz_when_Adder_l123_5 = _zz_when_Adder_l123_4[3 : 2];
  assign _zz_when_Adder_l123_6 = (~ _zz_when_Adder_l123_5[1 : 1]);
  assign _zz_when_Adder_l123_7 = (~ _zz_when_Adder_l123_5[0 : 0]);
  assign _zz_when_Adder_l123_8 = {(_zz_when_Adder_l123_6[0] && _zz_when_Adder_l123_7[0]),((! _zz_when_Adder_l123_6[0]) ? 1'b0 : (! _zz_when_Adder_l123_7[0]))};
  assign _zz_when_Adder_l123_9 = _zz_when_Adder_l123_4[1 : 0];
  assign _zz_when_Adder_l123_10 = (~ _zz_when_Adder_l123_9[1 : 1]);
  assign _zz_when_Adder_l123_11 = (~ _zz_when_Adder_l123_9[0 : 0]);
  assign _zz_when_Adder_l123_12 = {(_zz_when_Adder_l123_10[0] && _zz_when_Adder_l123_11[0]),((! _zz_when_Adder_l123_10[0]) ? 1'b0 : (! _zz_when_Adder_l123_11[0]))};
  assign _zz_when_Adder_l123_13 = {(_zz_when_Adder_l123_8[1] && _zz_when_Adder_l123_12[1]),((! _zz_when_Adder_l123_8[1]) ? {1'b0,_zz_when_Adder_l123_8[0 : 0]} : {(! _zz_when_Adder_l123_12[1]),_zz_when_Adder_l123_12[0 : 0]})};
  assign _zz_when_Adder_l123_14 = _zz_when_Adder_l123_3[3 : 0];
  assign _zz_when_Adder_l123_15 = _zz_when_Adder_l123_14[3 : 2];
  assign _zz_when_Adder_l123_16 = (~ _zz_when_Adder_l123_15[1 : 1]);
  assign _zz_when_Adder_l123_17 = (~ _zz_when_Adder_l123_15[0 : 0]);
  assign _zz_when_Adder_l123_18 = {(_zz_when_Adder_l123_16[0] && _zz_when_Adder_l123_17[0]),((! _zz_when_Adder_l123_16[0]) ? 1'b0 : (! _zz_when_Adder_l123_17[0]))};
  assign _zz_when_Adder_l123_19 = _zz_when_Adder_l123_14[1 : 0];
  assign _zz_when_Adder_l123_20 = (~ _zz_when_Adder_l123_19[1 : 1]);
  assign _zz_when_Adder_l123_21 = (~ _zz_when_Adder_l123_19[0 : 0]);
  assign _zz_when_Adder_l123_22 = {(_zz_when_Adder_l123_20[0] && _zz_when_Adder_l123_21[0]),((! _zz_when_Adder_l123_20[0]) ? 1'b0 : (! _zz_when_Adder_l123_21[0]))};
  assign _zz_when_Adder_l123_23 = {(_zz_when_Adder_l123_18[1] && _zz_when_Adder_l123_22[1]),((! _zz_when_Adder_l123_18[1]) ? {1'b0,_zz_when_Adder_l123_18[0 : 0]} : {(! _zz_when_Adder_l123_22[1]),_zz_when_Adder_l123_22[0 : 0]})};
  assign _zz_when_Adder_l123_24 = {(_zz_when_Adder_l123_13[2] && _zz_when_Adder_l123_23[2]),((! _zz_when_Adder_l123_13[2]) ? {1'b0,_zz_when_Adder_l123_13[1 : 0]} : {(! _zz_when_Adder_l123_23[2]),_zz_when_Adder_l123_23[1 : 0]})};
  assign _zz_when_Adder_l123_25 = _zz_when_Adder_l123_2[7 : 0];
  assign _zz_when_Adder_l123_26 = _zz_when_Adder_l123_25[7 : 4];
  assign _zz_when_Adder_l123_27 = _zz_when_Adder_l123_26[3 : 2];
  assign _zz_when_Adder_l123_28 = (~ _zz_when_Adder_l123_27[1 : 1]);
  assign _zz_when_Adder_l123_29 = (~ _zz_when_Adder_l123_27[0 : 0]);
  assign _zz_when_Adder_l123_30 = {(_zz_when_Adder_l123_28[0] && _zz_when_Adder_l123_29[0]),((! _zz_when_Adder_l123_28[0]) ? 1'b0 : (! _zz_when_Adder_l123_29[0]))};
  assign _zz_when_Adder_l123_31 = _zz_when_Adder_l123_26[1 : 0];
  assign _zz_when_Adder_l123_32 = (~ _zz_when_Adder_l123_31[1 : 1]);
  assign _zz_when_Adder_l123_33 = (~ _zz_when_Adder_l123_31[0 : 0]);
  assign _zz_when_Adder_l123_34 = {(_zz_when_Adder_l123_32[0] && _zz_when_Adder_l123_33[0]),((! _zz_when_Adder_l123_32[0]) ? 1'b0 : (! _zz_when_Adder_l123_33[0]))};
  assign _zz_when_Adder_l123_35 = {(_zz_when_Adder_l123_30[1] && _zz_when_Adder_l123_34[1]),((! _zz_when_Adder_l123_30[1]) ? {1'b0,_zz_when_Adder_l123_30[0 : 0]} : {(! _zz_when_Adder_l123_34[1]),_zz_when_Adder_l123_34[0 : 0]})};
  assign _zz_when_Adder_l123_36 = _zz_when_Adder_l123_25[3 : 0];
  assign _zz_when_Adder_l123_37 = _zz_when_Adder_l123_36[3 : 2];
  assign _zz_when_Adder_l123_38 = (~ _zz_when_Adder_l123_37[1 : 1]);
  assign _zz_when_Adder_l123_39 = (~ _zz_when_Adder_l123_37[0 : 0]);
  assign _zz_when_Adder_l123_40 = {(_zz_when_Adder_l123_38[0] && _zz_when_Adder_l123_39[0]),((! _zz_when_Adder_l123_38[0]) ? 1'b0 : (! _zz_when_Adder_l123_39[0]))};
  assign _zz_when_Adder_l123_41 = _zz_when_Adder_l123_36[1 : 0];
  assign _zz_when_Adder_l123_42 = (~ _zz_when_Adder_l123_41[1 : 1]);
  assign _zz_when_Adder_l123_43 = (~ _zz_when_Adder_l123_41[0 : 0]);
  assign _zz_when_Adder_l123_44 = {(_zz_when_Adder_l123_42[0] && _zz_when_Adder_l123_43[0]),((! _zz_when_Adder_l123_42[0]) ? 1'b0 : (! _zz_when_Adder_l123_43[0]))};
  assign _zz_when_Adder_l123_45 = {(_zz_when_Adder_l123_40[1] && _zz_when_Adder_l123_44[1]),((! _zz_when_Adder_l123_40[1]) ? {1'b0,_zz_when_Adder_l123_40[0 : 0]} : {(! _zz_when_Adder_l123_44[1]),_zz_when_Adder_l123_44[0 : 0]})};
  assign _zz_when_Adder_l123_46 = {(_zz_when_Adder_l123_35[2] && _zz_when_Adder_l123_45[2]),((! _zz_when_Adder_l123_35[2]) ? {1'b0,_zz_when_Adder_l123_35[1 : 0]} : {(! _zz_when_Adder_l123_45[2]),_zz_when_Adder_l123_45[1 : 0]})};
  assign _zz_when_Adder_l123_47 = {(_zz_when_Adder_l123_24[3] && _zz_when_Adder_l123_46[3]),((! _zz_when_Adder_l123_24[3]) ? {1'b0,_zz_when_Adder_l123_24[2 : 0]} : {(! _zz_when_Adder_l123_46[3]),_zz_when_Adder_l123_46[2 : 0]})};
  assign _zz_when_Adder_l123_48 = _zz_when_Adder_l123_1[15 : 0];
  assign _zz_when_Adder_l123_49 = _zz_when_Adder_l123_48[15 : 8];
  assign _zz_when_Adder_l123_50 = _zz_when_Adder_l123_49[7 : 4];
  assign _zz_when_Adder_l123_51 = _zz_when_Adder_l123_50[3 : 2];
  assign _zz_when_Adder_l123_52 = (~ _zz_when_Adder_l123_51[1 : 1]);
  assign _zz_when_Adder_l123_53 = (~ _zz_when_Adder_l123_51[0 : 0]);
  assign _zz_when_Adder_l123_54 = {(_zz_when_Adder_l123_52[0] && _zz_when_Adder_l123_53[0]),((! _zz_when_Adder_l123_52[0]) ? 1'b0 : (! _zz_when_Adder_l123_53[0]))};
  assign _zz_when_Adder_l123_55 = _zz_when_Adder_l123_50[1 : 0];
  assign _zz_when_Adder_l123_56 = (~ _zz_when_Adder_l123_55[1 : 1]);
  assign _zz_when_Adder_l123_57 = (~ _zz_when_Adder_l123_55[0 : 0]);
  assign _zz_when_Adder_l123_58 = {(_zz_when_Adder_l123_56[0] && _zz_when_Adder_l123_57[0]),((! _zz_when_Adder_l123_56[0]) ? 1'b0 : (! _zz_when_Adder_l123_57[0]))};
  assign _zz_when_Adder_l123_59 = {(_zz_when_Adder_l123_54[1] && _zz_when_Adder_l123_58[1]),((! _zz_when_Adder_l123_54[1]) ? {1'b0,_zz_when_Adder_l123_54[0 : 0]} : {(! _zz_when_Adder_l123_58[1]),_zz_when_Adder_l123_58[0 : 0]})};
  assign _zz_when_Adder_l123_60 = _zz_when_Adder_l123_49[3 : 0];
  assign _zz_when_Adder_l123_61 = _zz_when_Adder_l123_60[3 : 2];
  assign _zz_when_Adder_l123_62 = (~ _zz_when_Adder_l123_61[1 : 1]);
  assign _zz_when_Adder_l123_63 = (~ _zz_when_Adder_l123_61[0 : 0]);
  assign _zz_when_Adder_l123_64 = {(_zz_when_Adder_l123_62[0] && _zz_when_Adder_l123_63[0]),((! _zz_when_Adder_l123_62[0]) ? 1'b0 : (! _zz_when_Adder_l123_63[0]))};
  assign _zz_when_Adder_l123_65 = _zz_when_Adder_l123_60[1 : 0];
  assign _zz_when_Adder_l123_66 = (~ _zz_when_Adder_l123_65[1 : 1]);
  assign _zz_when_Adder_l123_67 = (~ _zz_when_Adder_l123_65[0 : 0]);
  assign _zz_when_Adder_l123_68 = {(_zz_when_Adder_l123_66[0] && _zz_when_Adder_l123_67[0]),((! _zz_when_Adder_l123_66[0]) ? 1'b0 : (! _zz_when_Adder_l123_67[0]))};
  assign _zz_when_Adder_l123_69 = {(_zz_when_Adder_l123_64[1] && _zz_when_Adder_l123_68[1]),((! _zz_when_Adder_l123_64[1]) ? {1'b0,_zz_when_Adder_l123_64[0 : 0]} : {(! _zz_when_Adder_l123_68[1]),_zz_when_Adder_l123_68[0 : 0]})};
  assign _zz_when_Adder_l123_70 = {(_zz_when_Adder_l123_59[2] && _zz_when_Adder_l123_69[2]),((! _zz_when_Adder_l123_59[2]) ? {1'b0,_zz_when_Adder_l123_59[1 : 0]} : {(! _zz_when_Adder_l123_69[2]),_zz_when_Adder_l123_69[1 : 0]})};
  assign _zz_when_Adder_l123_71 = _zz_when_Adder_l123_48[7 : 0];
  assign _zz_when_Adder_l123_72 = _zz_when_Adder_l123_71[7 : 4];
  assign _zz_when_Adder_l123_73 = _zz_when_Adder_l123_72[3 : 2];
  assign _zz_when_Adder_l123_74 = (~ _zz_when_Adder_l123_73[1 : 1]);
  assign _zz_when_Adder_l123_75 = (~ _zz_when_Adder_l123_73[0 : 0]);
  assign _zz_when_Adder_l123_76 = {(_zz_when_Adder_l123_74[0] && _zz_when_Adder_l123_75[0]),((! _zz_when_Adder_l123_74[0]) ? 1'b0 : (! _zz_when_Adder_l123_75[0]))};
  assign _zz_when_Adder_l123_77 = _zz_when_Adder_l123_72[1 : 0];
  assign _zz_when_Adder_l123_78 = (~ _zz_when_Adder_l123_77[1 : 1]);
  assign _zz_when_Adder_l123_79 = (~ _zz_when_Adder_l123_77[0 : 0]);
  assign _zz_when_Adder_l123_80 = {(_zz_when_Adder_l123_78[0] && _zz_when_Adder_l123_79[0]),((! _zz_when_Adder_l123_78[0]) ? 1'b0 : (! _zz_when_Adder_l123_79[0]))};
  assign _zz_when_Adder_l123_81 = {(_zz_when_Adder_l123_76[1] && _zz_when_Adder_l123_80[1]),((! _zz_when_Adder_l123_76[1]) ? {1'b0,_zz_when_Adder_l123_76[0 : 0]} : {(! _zz_when_Adder_l123_80[1]),_zz_when_Adder_l123_80[0 : 0]})};
  assign _zz_when_Adder_l123_82 = _zz_when_Adder_l123_71[3 : 0];
  assign _zz_when_Adder_l123_83 = _zz_when_Adder_l123_82[3 : 2];
  assign _zz_when_Adder_l123_84 = (~ _zz_when_Adder_l123_83[1 : 1]);
  assign _zz_when_Adder_l123_85 = (~ _zz_when_Adder_l123_83[0 : 0]);
  assign _zz_when_Adder_l123_86 = {(_zz_when_Adder_l123_84[0] && _zz_when_Adder_l123_85[0]),((! _zz_when_Adder_l123_84[0]) ? 1'b0 : (! _zz_when_Adder_l123_85[0]))};
  assign _zz_when_Adder_l123_87 = _zz_when_Adder_l123_82[1 : 0];
  assign _zz_when_Adder_l123_88 = (~ _zz_when_Adder_l123_87[1 : 1]);
  assign _zz_when_Adder_l123_89 = (~ _zz_when_Adder_l123_87[0 : 0]);
  assign _zz_when_Adder_l123_90 = {(_zz_when_Adder_l123_88[0] && _zz_when_Adder_l123_89[0]),((! _zz_when_Adder_l123_88[0]) ? 1'b0 : (! _zz_when_Adder_l123_89[0]))};
  assign _zz_when_Adder_l123_91 = {(_zz_when_Adder_l123_86[1] && _zz_when_Adder_l123_90[1]),((! _zz_when_Adder_l123_86[1]) ? {1'b0,_zz_when_Adder_l123_86[0 : 0]} : {(! _zz_when_Adder_l123_90[1]),_zz_when_Adder_l123_90[0 : 0]})};
  assign _zz_when_Adder_l123_92 = {(_zz_when_Adder_l123_81[2] && _zz_when_Adder_l123_91[2]),((! _zz_when_Adder_l123_81[2]) ? {1'b0,_zz_when_Adder_l123_81[1 : 0]} : {(! _zz_when_Adder_l123_91[2]),_zz_when_Adder_l123_91[1 : 0]})};
  assign _zz_when_Adder_l123_93 = {(_zz_when_Adder_l123_70[3] && _zz_when_Adder_l123_92[3]),((! _zz_when_Adder_l123_70[3]) ? {1'b0,_zz_when_Adder_l123_70[2 : 0]} : {(! _zz_when_Adder_l123_92[3]),_zz_when_Adder_l123_92[2 : 0]})};
  assign _zz_when_Adder_l123_94 = {(_zz_when_Adder_l123_47[4] && _zz_when_Adder_l123_93[4]),((! _zz_when_Adder_l123_47[4]) ? {1'b0,_zz_when_Adder_l123_47[3 : 0]} : {(! _zz_when_Adder_l123_93[4]),_zz_when_Adder_l123_93[3 : 0]})};
  assign _zz_when_Adder_l123_95 = _zz_when_Adder_l123[31 : 0];
  assign _zz_when_Adder_l123_96 = _zz_when_Adder_l123_95[31 : 16];
  assign _zz_when_Adder_l123_97 = _zz_when_Adder_l123_96[15 : 8];
  assign _zz_when_Adder_l123_98 = _zz_when_Adder_l123_97[7 : 4];
  assign _zz_when_Adder_l123_99 = _zz_when_Adder_l123_98[3 : 2];
  assign _zz_when_Adder_l123_100 = (~ _zz_when_Adder_l123_99[1 : 1]);
  assign _zz_when_Adder_l123_101 = (~ _zz_when_Adder_l123_99[0 : 0]);
  assign _zz_when_Adder_l123_102 = {(_zz_when_Adder_l123_100[0] && _zz_when_Adder_l123_101[0]),((! _zz_when_Adder_l123_100[0]) ? 1'b0 : (! _zz_when_Adder_l123_101[0]))};
  assign _zz_when_Adder_l123_103 = _zz_when_Adder_l123_98[1 : 0];
  assign _zz_when_Adder_l123_104 = (~ _zz_when_Adder_l123_103[1 : 1]);
  assign _zz_when_Adder_l123_105 = (~ _zz_when_Adder_l123_103[0 : 0]);
  assign _zz_when_Adder_l123_106 = {(_zz_when_Adder_l123_104[0] && _zz_when_Adder_l123_105[0]),((! _zz_when_Adder_l123_104[0]) ? 1'b0 : (! _zz_when_Adder_l123_105[0]))};
  assign _zz_when_Adder_l123_107 = {(_zz_when_Adder_l123_102[1] && _zz_when_Adder_l123_106[1]),((! _zz_when_Adder_l123_102[1]) ? {1'b0,_zz_when_Adder_l123_102[0 : 0]} : {(! _zz_when_Adder_l123_106[1]),_zz_when_Adder_l123_106[0 : 0]})};
  assign _zz_when_Adder_l123_108 = _zz_when_Adder_l123_97[3 : 0];
  assign _zz_when_Adder_l123_109 = _zz_when_Adder_l123_108[3 : 2];
  assign _zz_when_Adder_l123_110 = (~ _zz_when_Adder_l123_109[1 : 1]);
  assign _zz_when_Adder_l123_111 = (~ _zz_when_Adder_l123_109[0 : 0]);
  assign _zz_when_Adder_l123_112 = {(_zz_when_Adder_l123_110[0] && _zz_when_Adder_l123_111[0]),((! _zz_when_Adder_l123_110[0]) ? 1'b0 : (! _zz_when_Adder_l123_111[0]))};
  assign _zz_when_Adder_l123_113 = _zz_when_Adder_l123_108[1 : 0];
  assign _zz_when_Adder_l123_114 = (~ _zz_when_Adder_l123_113[1 : 1]);
  assign _zz_when_Adder_l123_115 = (~ _zz_when_Adder_l123_113[0 : 0]);
  assign _zz_when_Adder_l123_116 = {(_zz_when_Adder_l123_114[0] && _zz_when_Adder_l123_115[0]),((! _zz_when_Adder_l123_114[0]) ? 1'b0 : (! _zz_when_Adder_l123_115[0]))};
  assign _zz_when_Adder_l123_117 = {(_zz_when_Adder_l123_112[1] && _zz_when_Adder_l123_116[1]),((! _zz_when_Adder_l123_112[1]) ? {1'b0,_zz_when_Adder_l123_112[0 : 0]} : {(! _zz_when_Adder_l123_116[1]),_zz_when_Adder_l123_116[0 : 0]})};
  assign _zz_when_Adder_l123_118 = {(_zz_when_Adder_l123_107[2] && _zz_when_Adder_l123_117[2]),((! _zz_when_Adder_l123_107[2]) ? {1'b0,_zz_when_Adder_l123_107[1 : 0]} : {(! _zz_when_Adder_l123_117[2]),_zz_when_Adder_l123_117[1 : 0]})};
  assign _zz_when_Adder_l123_119 = _zz_when_Adder_l123_96[7 : 0];
  assign _zz_when_Adder_l123_120 = _zz_when_Adder_l123_119[7 : 4];
  assign _zz_when_Adder_l123_121 = _zz_when_Adder_l123_120[3 : 2];
  assign _zz_when_Adder_l123_122 = (~ _zz_when_Adder_l123_121[1 : 1]);
  assign _zz_when_Adder_l123_123 = (~ _zz_when_Adder_l123_121[0 : 0]);
  assign _zz_when_Adder_l123_124 = {(_zz_when_Adder_l123_122[0] && _zz_when_Adder_l123_123[0]),((! _zz_when_Adder_l123_122[0]) ? 1'b0 : (! _zz_when_Adder_l123_123[0]))};
  assign _zz_when_Adder_l123_125 = _zz_when_Adder_l123_120[1 : 0];
  assign _zz_when_Adder_l123_126 = (~ _zz_when_Adder_l123_125[1 : 1]);
  assign _zz_when_Adder_l123_127 = (~ _zz_when_Adder_l123_125[0 : 0]);
  assign _zz_when_Adder_l123_128 = {(_zz_when_Adder_l123_126[0] && _zz_when_Adder_l123_127[0]),((! _zz_when_Adder_l123_126[0]) ? 1'b0 : (! _zz_when_Adder_l123_127[0]))};
  assign _zz_when_Adder_l123_129 = {(_zz_when_Adder_l123_124[1] && _zz_when_Adder_l123_128[1]),((! _zz_when_Adder_l123_124[1]) ? {1'b0,_zz_when_Adder_l123_124[0 : 0]} : {(! _zz_when_Adder_l123_128[1]),_zz_when_Adder_l123_128[0 : 0]})};
  assign _zz_when_Adder_l123_130 = _zz_when_Adder_l123_119[3 : 0];
  assign _zz_when_Adder_l123_131 = _zz_when_Adder_l123_130[3 : 2];
  assign _zz_when_Adder_l123_132 = (~ _zz_when_Adder_l123_131[1 : 1]);
  assign _zz_when_Adder_l123_133 = (~ _zz_when_Adder_l123_131[0 : 0]);
  assign _zz_when_Adder_l123_134 = {(_zz_when_Adder_l123_132[0] && _zz_when_Adder_l123_133[0]),((! _zz_when_Adder_l123_132[0]) ? 1'b0 : (! _zz_when_Adder_l123_133[0]))};
  assign _zz_when_Adder_l123_135 = _zz_when_Adder_l123_130[1 : 0];
  assign _zz_when_Adder_l123_136 = (~ _zz_when_Adder_l123_135[1 : 1]);
  assign _zz_when_Adder_l123_137 = (~ _zz_when_Adder_l123_135[0 : 0]);
  assign _zz_when_Adder_l123_138 = {(_zz_when_Adder_l123_136[0] && _zz_when_Adder_l123_137[0]),((! _zz_when_Adder_l123_136[0]) ? 1'b0 : (! _zz_when_Adder_l123_137[0]))};
  assign _zz_when_Adder_l123_139 = {(_zz_when_Adder_l123_134[1] && _zz_when_Adder_l123_138[1]),((! _zz_when_Adder_l123_134[1]) ? {1'b0,_zz_when_Adder_l123_134[0 : 0]} : {(! _zz_when_Adder_l123_138[1]),_zz_when_Adder_l123_138[0 : 0]})};
  assign _zz_when_Adder_l123_140 = {(_zz_when_Adder_l123_129[2] && _zz_when_Adder_l123_139[2]),((! _zz_when_Adder_l123_129[2]) ? {1'b0,_zz_when_Adder_l123_129[1 : 0]} : {(! _zz_when_Adder_l123_139[2]),_zz_when_Adder_l123_139[1 : 0]})};
  assign _zz_when_Adder_l123_141 = {(_zz_when_Adder_l123_118[3] && _zz_when_Adder_l123_140[3]),((! _zz_when_Adder_l123_118[3]) ? {1'b0,_zz_when_Adder_l123_118[2 : 0]} : {(! _zz_when_Adder_l123_140[3]),_zz_when_Adder_l123_140[2 : 0]})};
  assign _zz_when_Adder_l123_142 = _zz_when_Adder_l123_95[15 : 0];
  assign _zz_when_Adder_l123_143 = _zz_when_Adder_l123_142[15 : 8];
  assign _zz_when_Adder_l123_144 = _zz_when_Adder_l123_143[7 : 4];
  assign _zz_when_Adder_l123_145 = _zz_when_Adder_l123_144[3 : 2];
  assign _zz_when_Adder_l123_146 = (~ _zz_when_Adder_l123_145[1 : 1]);
  assign _zz_when_Adder_l123_147 = (~ _zz_when_Adder_l123_145[0 : 0]);
  assign _zz_when_Adder_l123_148 = {(_zz_when_Adder_l123_146[0] && _zz_when_Adder_l123_147[0]),((! _zz_when_Adder_l123_146[0]) ? 1'b0 : (! _zz_when_Adder_l123_147[0]))};
  assign _zz_when_Adder_l123_149 = _zz_when_Adder_l123_144[1 : 0];
  assign _zz_when_Adder_l123_150 = (~ _zz_when_Adder_l123_149[1 : 1]);
  assign _zz_when_Adder_l123_151 = (~ _zz_when_Adder_l123_149[0 : 0]);
  assign _zz_when_Adder_l123_152 = {(_zz_when_Adder_l123_150[0] && _zz_when_Adder_l123_151[0]),((! _zz_when_Adder_l123_150[0]) ? 1'b0 : (! _zz_when_Adder_l123_151[0]))};
  assign _zz_when_Adder_l123_153 = {(_zz_when_Adder_l123_148[1] && _zz_when_Adder_l123_152[1]),((! _zz_when_Adder_l123_148[1]) ? {1'b0,_zz_when_Adder_l123_148[0 : 0]} : {(! _zz_when_Adder_l123_152[1]),_zz_when_Adder_l123_152[0 : 0]})};
  assign _zz_when_Adder_l123_154 = _zz_when_Adder_l123_143[3 : 0];
  assign _zz_when_Adder_l123_155 = _zz_when_Adder_l123_154[3 : 2];
  assign _zz_when_Adder_l123_156 = (~ _zz_when_Adder_l123_155[1 : 1]);
  assign _zz_when_Adder_l123_157 = (~ _zz_when_Adder_l123_155[0 : 0]);
  assign _zz_when_Adder_l123_158 = {(_zz_when_Adder_l123_156[0] && _zz_when_Adder_l123_157[0]),((! _zz_when_Adder_l123_156[0]) ? 1'b0 : (! _zz_when_Adder_l123_157[0]))};
  assign _zz_when_Adder_l123_159 = _zz_when_Adder_l123_154[1 : 0];
  assign _zz_when_Adder_l123_160 = (~ _zz_when_Adder_l123_159[1 : 1]);
  assign _zz_when_Adder_l123_161 = (~ _zz_when_Adder_l123_159[0 : 0]);
  assign _zz_when_Adder_l123_162 = {(_zz_when_Adder_l123_160[0] && _zz_when_Adder_l123_161[0]),((! _zz_when_Adder_l123_160[0]) ? 1'b0 : (! _zz_when_Adder_l123_161[0]))};
  assign _zz_when_Adder_l123_163 = {(_zz_when_Adder_l123_158[1] && _zz_when_Adder_l123_162[1]),((! _zz_when_Adder_l123_158[1]) ? {1'b0,_zz_when_Adder_l123_158[0 : 0]} : {(! _zz_when_Adder_l123_162[1]),_zz_when_Adder_l123_162[0 : 0]})};
  assign _zz_when_Adder_l123_164 = {(_zz_when_Adder_l123_153[2] && _zz_when_Adder_l123_163[2]),((! _zz_when_Adder_l123_153[2]) ? {1'b0,_zz_when_Adder_l123_153[1 : 0]} : {(! _zz_when_Adder_l123_163[2]),_zz_when_Adder_l123_163[1 : 0]})};
  assign _zz_when_Adder_l123_165 = _zz_when_Adder_l123_142[7 : 0];
  assign _zz_when_Adder_l123_166 = _zz_when_Adder_l123_165[7 : 4];
  assign _zz_when_Adder_l123_167 = _zz_when_Adder_l123_166[3 : 2];
  assign _zz_when_Adder_l123_168 = (~ _zz_when_Adder_l123_167[1 : 1]);
  assign _zz_when_Adder_l123_169 = (~ _zz_when_Adder_l123_167[0 : 0]);
  assign _zz_when_Adder_l123_170 = {(_zz_when_Adder_l123_168[0] && _zz_when_Adder_l123_169[0]),((! _zz_when_Adder_l123_168[0]) ? 1'b0 : (! _zz_when_Adder_l123_169[0]))};
  assign _zz_when_Adder_l123_171 = _zz_when_Adder_l123_166[1 : 0];
  assign _zz_when_Adder_l123_172 = (~ _zz_when_Adder_l123_171[1 : 1]);
  assign _zz_when_Adder_l123_173 = (~ _zz_when_Adder_l123_171[0 : 0]);
  assign _zz_when_Adder_l123_174 = {(_zz_when_Adder_l123_172[0] && _zz_when_Adder_l123_173[0]),((! _zz_when_Adder_l123_172[0]) ? 1'b0 : (! _zz_when_Adder_l123_173[0]))};
  assign _zz_when_Adder_l123_175 = {(_zz_when_Adder_l123_170[1] && _zz_when_Adder_l123_174[1]),((! _zz_when_Adder_l123_170[1]) ? {1'b0,_zz_when_Adder_l123_170[0 : 0]} : {(! _zz_when_Adder_l123_174[1]),_zz_when_Adder_l123_174[0 : 0]})};
  assign _zz_when_Adder_l123_176 = _zz_when_Adder_l123_165[3 : 0];
  assign _zz_when_Adder_l123_177 = _zz_when_Adder_l123_176[3 : 2];
  assign _zz_when_Adder_l123_178 = (~ _zz_when_Adder_l123_177[1 : 1]);
  assign _zz_when_Adder_l123_179 = (~ _zz_when_Adder_l123_177[0 : 0]);
  assign _zz_when_Adder_l123_180 = {(_zz_when_Adder_l123_178[0] && _zz_when_Adder_l123_179[0]),((! _zz_when_Adder_l123_178[0]) ? 1'b0 : (! _zz_when_Adder_l123_179[0]))};
  assign _zz_when_Adder_l123_181 = _zz_when_Adder_l123_176[1 : 0];
  assign _zz_when_Adder_l123_182 = (~ _zz_when_Adder_l123_181[1 : 1]);
  assign _zz_when_Adder_l123_183 = (~ _zz_when_Adder_l123_181[0 : 0]);
  assign _zz_when_Adder_l123_184 = {(_zz_when_Adder_l123_182[0] && _zz_when_Adder_l123_183[0]),((! _zz_when_Adder_l123_182[0]) ? 1'b0 : (! _zz_when_Adder_l123_183[0]))};
  assign _zz_when_Adder_l123_185 = {(_zz_when_Adder_l123_180[1] && _zz_when_Adder_l123_184[1]),((! _zz_when_Adder_l123_180[1]) ? {1'b0,_zz_when_Adder_l123_180[0 : 0]} : {(! _zz_when_Adder_l123_184[1]),_zz_when_Adder_l123_184[0 : 0]})};
  assign _zz_when_Adder_l123_186 = {(_zz_when_Adder_l123_175[2] && _zz_when_Adder_l123_185[2]),((! _zz_when_Adder_l123_175[2]) ? {1'b0,_zz_when_Adder_l123_175[1 : 0]} : {(! _zz_when_Adder_l123_185[2]),_zz_when_Adder_l123_185[1 : 0]})};
  assign _zz_when_Adder_l123_187 = {(_zz_when_Adder_l123_164[3] && _zz_when_Adder_l123_186[3]),((! _zz_when_Adder_l123_164[3]) ? {1'b0,_zz_when_Adder_l123_164[2 : 0]} : {(! _zz_when_Adder_l123_186[3]),_zz_when_Adder_l123_186[2 : 0]})};
  assign _zz_when_Adder_l123_188 = {(_zz_when_Adder_l123_141[4] && _zz_when_Adder_l123_187[4]),((! _zz_when_Adder_l123_141[4]) ? {1'b0,_zz_when_Adder_l123_141[3 : 0]} : {(! _zz_when_Adder_l123_187[4]),_zz_when_Adder_l123_187[3 : 0]})};
  assign _zz_when_Adder_l123_189 = ($signed(_zz__zz_when_Adder_l123_189) - $signed(7'h03));
  assign when_Adder_l123 = ($signed(7'h0) <= $signed(_zz_when_Adder_l123_189));
  always @(*) begin
    if(when_Adder_l123) begin
      _zz_pipe_ctrl_1_down_RESULT_8 = _zz__zz_pipe_ctrl_1_down_RESULT_8[52:0];
    end else begin
      _zz_pipe_ctrl_1_down_RESULT_8 = _zz__zz_pipe_ctrl_1_down_RESULT_8_1[52:0];
    end
  end

  always @(*) begin
    if(when_Adder_l123) begin
      _zz_pipe_ctrl_1_down_RESULT_9 = ($signed(_zz__zz_pipe_ctrl_1_down_RESULT_9) - $signed(_zz__zz_pipe_ctrl_1_down_RESULT_9_1));
    end else begin
      _zz_pipe_ctrl_1_down_RESULT_9 = ($signed(_zz__zz_pipe_ctrl_1_down_RESULT_9_2) + $signed(_zz__zz_pipe_ctrl_1_down_RESULT_9_3));
    end
  end

  assign _zz_pipe_ctrl_1_down_RESULT_10 = _zz__zz_pipe_ctrl_1_down_RESULT_10;
  assign _zz_pipe_ctrl_1_down_RESULT_13 = _zz__zz_pipe_ctrl_1_down_RESULT_13;
  assign _zz_pipe_ctrl_1_down_RESULT_14 = 6'h3f;
  always @(*) begin
    if(_zz_when) begin
      _zz_pipe_ctrl_1_down_RESULT_12 = (_zz_pipe_ctrl_1_down_RESULT_8 >>> ((_zz_pipe_ctrl_1_down_RESULT_13 < _zz__zz_pipe_ctrl_1_down_RESULT_12) ? _zz_pipe_ctrl_1_down_RESULT_13 : _zz__zz_pipe_ctrl_1_down_RESULT_12_1));
    end else begin
      _zz_pipe_ctrl_1_down_RESULT_12 = _zz_pipe_ctrl_1_down_RESULT_8;
    end
  end

  always @(*) begin
    if(_zz_when) begin
      _zz_pipe_ctrl_1_down_RESULT_11 = 11'h0;
    end else begin
      _zz_pipe_ctrl_1_down_RESULT_11 = _zz__zz_pipe_ctrl_1_down_RESULT_11[10:0];
    end
  end

  assign _zz_pipe_ctrl_1_down_RESULT_15 = _zz_pipe_ctrl_1_down_RESULT_12;
  assign _zz_pipe_ctrl_1_down_RESULT_16 = _zz_pipe_ctrl_1_down_RESULT_15[0];
  assign _zz_pipe_ctrl_1_down_RESULT_17 = 1'b0;
  assign _zz_pipe_ctrl_1_down_RESULT_18 = 1'b0;
  assign _zz_pipe_ctrl_1_down_RESULT_19 = _zz_pipe_ctrl_1_down_RESULT_15[52 : 1];
  always @(*) begin
    case(pipe_ctrl_1_down_ROUND)
      2'b00 : begin
        _zz_pipe_ctrl_1_down_RESULT_20 = (_zz_pipe_ctrl_1_down_RESULT_16 && ((_zz_pipe_ctrl_1_down_RESULT_17 || _zz_pipe_ctrl_1_down_RESULT_18) || _zz_pipe_ctrl_1_down_RESULT_19[0]));
      end
      2'b01 : begin
        _zz_pipe_ctrl_1_down_RESULT_20 = 1'b0;
      end
      2'b10 : begin
        _zz_pipe_ctrl_1_down_RESULT_20 = (((_zz_pipe_ctrl_1_down_RESULT_16 || _zz_pipe_ctrl_1_down_RESULT_17) || _zz_pipe_ctrl_1_down_RESULT_18) && (! _zz_pipe_ctrl_1_down_RESULT_3));
      end
      default : begin
        _zz_pipe_ctrl_1_down_RESULT_20 = (((_zz_pipe_ctrl_1_down_RESULT_16 || _zz_pipe_ctrl_1_down_RESULT_17) || _zz_pipe_ctrl_1_down_RESULT_18) && _zz_pipe_ctrl_1_down_RESULT_3);
      end
    endcase
  end

  assign _zz_pipe_ctrl_1_down_RESULT_21 = (_zz_pipe_ctrl_1_down_RESULT_19 + _zz__zz_pipe_ctrl_1_down_RESULT_21);
  assign _zz_pipe_ctrl_1_down_RESULT_22 = 1'b0;
  assign pipe_ctrl_1_down_RESULT = {{_zz_pipe_ctrl_1_down_RESULT_3,(_zz_pipe_ctrl_1_down_RESULT_22 ? _zz_pipe_ctrl_1_down_RESULT_23 : _zz_pipe_ctrl_1_down_RESULT_11)},(_zz_pipe_ctrl_1_down_RESULT_22 ? _zz_pipe_ctrl_1_down_RESULT_24 : _zz_pipe_ctrl_1_down_RESULT_21)};
  assign io_rsp_valid = pipe_ctrl_1_down_isValid;
  assign pipe_ctrl_1_down_ready = io_rsp_ready;
  assign io_rsp_payload = pipe_ctrl_1_down_RESULT;
  always @(*) begin
    pipe_ctrl_0_down_ready = pipe_ctrl_1_up_ready;
    if(when_StageLink_l67) begin
      pipe_ctrl_0_down_ready = 1'b1;
    end
  end

  assign when_StageLink_l67 = (! pipe_ctrl_1_up_isValid);
  assign pipe_ctrl_0_down_valid = pipe_ctrl_0_up_valid;
  assign pipe_ctrl_0_up_ready = pipe_ctrl_0_down_isReady;
  assign pipe_ctrl_0_down_A = pipe_ctrl_0_up_A;
  assign pipe_ctrl_0_down_B = pipe_ctrl_0_up_B;
  assign pipe_ctrl_0_down_SUB = pipe_ctrl_0_up_SUB;
  assign pipe_ctrl_0_down_ROUND = pipe_ctrl_0_up_ROUND;
  assign pipe_ctrl_1_down_valid = pipe_ctrl_1_up_valid;
  assign pipe_ctrl_1_up_ready = pipe_ctrl_1_down_isReady;
  assign pipe_ctrl_1_down_SUB = pipe_ctrl_1_up_SUB;
  assign pipe_ctrl_1_down_ROUND = pipe_ctrl_1_up_ROUND;
  assign pipe_ctrl_1_down_SA = pipe_ctrl_1_up_SA;
  assign pipe_ctrl_1_down_SB = pipe_ctrl_1_up_SB;
  assign pipe_ctrl_1_down_EXP = pipe_ctrl_1_up_EXP;
  assign pipe_ctrl_1_down_MA = pipe_ctrl_1_up_MA;
  assign pipe_ctrl_1_down_MB = pipe_ctrl_1_up_MB;
  assign pipe_ctrl_0_down_isValid = pipe_ctrl_0_down_valid;
  assign pipe_ctrl_0_down_isReady = pipe_ctrl_0_down_ready;
  assign pipe_ctrl_1_up_isValid = pipe_ctrl_1_up_valid;
  assign pipe_ctrl_0_up_isReady = pipe_ctrl_0_up_ready;
  assign pipe_ctrl_0_up_isCancel = 1'b0;
  assign pipe_ctrl_1_down_isValid = pipe_ctrl_1_down_valid;
  assign pipe_ctrl_1_down_isReady = pipe_ctrl_1_down_ready;
  always @(posedge clk) begin
    if(reset) begin
      pipe_ctrl_1_up_valid <= 1'b0;
    end else begin
      if(pipe_ctrl_0_down_isReady) begin
        pipe_ctrl_1_up_valid <= pipe_ctrl_0_down_isValid;
      end
    end
  end

  always @(posedge clk) begin
    if(pipe_ctrl_0_down_isReady) begin
      pipe_ctrl_1_up_SUB <= pipe_ctrl_0_down_SUB;
      pipe_ctrl_1_up_ROUND <= pipe_ctrl_0_down_ROUND;
      pipe_ctrl_1_up_SA <= pipe_ctrl_0_down_SA;
      pipe_ctrl_1_up_SB <= pipe_ctrl_0_down_SB;
      pipe_ctrl_1_up_EXP <= pipe_ctrl_0_down_EXP;
      pipe_ctrl_1_up_MA <= pipe_ctrl_0_down_MA;
      pipe_ctrl_1_up_MB <= pipe_ctrl_0_down_MB;
    end
  end


endmodule

module BmbUpSizerBridge (
  input  wire          io_input_cmd_valid,
  output wire          io_input_cmd_ready,
  input  wire          io_input_cmd_payload_last,
  input  wire [0:0]    io_input_cmd_payload_fragment_source,
  input  wire [0:0]    io_input_cmd_payload_fragment_opcode,
  input  wire [31:0]   io_input_cmd_payload_fragment_address,
  input  wire [2:0]    io_input_cmd_payload_fragment_length,
  input  wire [63:0]   io_input_cmd_payload_fragment_data,
  input  wire [7:0]    io_input_cmd_payload_fragment_mask,
  output wire          io_input_rsp_valid,
  input  wire          io_input_rsp_ready,
  output reg           io_input_rsp_payload_last,
  output wire [0:0]    io_input_rsp_payload_fragment_source,
  output wire [0:0]    io_input_rsp_payload_fragment_opcode,
  output wire [63:0]   io_input_rsp_payload_fragment_data,
  output wire          io_output_cmd_valid,
  input  wire          io_output_cmd_ready,
  output wire          io_output_cmd_payload_last,
  output wire [0:0]    io_output_cmd_payload_fragment_source,
  output wire [0:0]    io_output_cmd_payload_fragment_opcode,
  output wire [31:0]   io_output_cmd_payload_fragment_address,
  output wire [2:0]    io_output_cmd_payload_fragment_length,
  output reg  [127:0]  io_output_cmd_payload_fragment_data,
  output reg  [15:0]   io_output_cmd_payload_fragment_mask,
  output wire [1:0]    io_output_cmd_payload_fragment_context,
  input  wire          io_output_rsp_valid,
  output wire          io_output_rsp_ready,
  input  wire          io_output_rsp_payload_last,
  input  wire [0:0]    io_output_rsp_payload_fragment_source,
  input  wire [0:0]    io_output_rsp_payload_fragment_opcode,
  input  wire [127:0]  io_output_rsp_payload_fragment_data,
  input  wire [1:0]    io_output_rsp_payload_fragment_context,
  input  wire          clk,
  input  wire          reset
);

  reg        [63:0]   _zz_io_input_rsp_payload_fragment_data;
  wire       [0:0]    cmdArea_selStart;
  wire       [0:0]    cmdArea_context_selStart;
  reg        [0:0]    cmdArea_context_selEnd;
  wire                when_BmbUpSizerBridge_l53;
  reg        [63:0]   cmdArea_writeLogic_dataRegs_0;
  reg        [7:0]    cmdArea_writeLogic_maskRegs_0;
  reg        [0:0]    cmdArea_writeLogic_selReg;
  wire                io_input_cmd_fire;
  reg                 io_input_cmd_payload_first;
  wire       [0:0]    cmdArea_writeLogic_sel;
  wire       [63:0]   cmdArea_writeLogic_outputData_0;
  wire       [63:0]   cmdArea_writeLogic_outputData_1;
  wire       [7:0]    cmdArea_writeLogic_outputMask_0;
  wire       [7:0]    cmdArea_writeLogic_outputMask_1;
  wire                when_BmbUpSizerBridge_l85;
  wire                when_BmbUpSizerBridge_l95;
  wire                io_output_cmd_fire;
  wire                io_output_cmd_isStall;
  wire       [0:0]    rspArea_context_selStart;
  wire       [0:0]    rspArea_context_selEnd;
  wire       [1:0]    _zz_rspArea_context_selStart;
  reg        [0:0]    rspArea_readLogic_selReg;
  wire                io_input_rsp_fire;
  reg                 io_input_rsp_payload_first;
  wire       [0:0]    rspArea_readLogic_sel;
  wire                when_BmbUpSizerBridge_l133;

  always @(*) begin
    case(rspArea_readLogic_sel)
      1'b0 : _zz_io_input_rsp_payload_fragment_data = io_output_rsp_payload_fragment_data[63 : 0];
      default : _zz_io_input_rsp_payload_fragment_data = io_output_rsp_payload_fragment_data[127 : 64];
    endcase
  end

  assign cmdArea_selStart = io_input_cmd_payload_fragment_address[3 : 3];
  assign cmdArea_context_selStart = cmdArea_selStart;
  always @(*) begin
    cmdArea_context_selEnd = (io_input_cmd_payload_fragment_address[3 : 3] + 1'b0);
    if(when_BmbUpSizerBridge_l53) begin
      cmdArea_context_selEnd = io_input_cmd_payload_fragment_address[3 : 3];
    end
  end

  assign when_BmbUpSizerBridge_l53 = (io_input_cmd_payload_fragment_opcode == 1'b1);
  assign io_output_cmd_payload_last = io_input_cmd_payload_last;
  assign io_output_cmd_payload_fragment_opcode = io_input_cmd_payload_fragment_opcode;
  assign io_output_cmd_payload_fragment_address = io_input_cmd_payload_fragment_address;
  assign io_output_cmd_payload_fragment_length = io_input_cmd_payload_fragment_length;
  assign io_output_cmd_payload_fragment_source = io_input_cmd_payload_fragment_source;
  assign io_output_cmd_payload_fragment_context = {cmdArea_context_selEnd,cmdArea_context_selStart};
  assign io_input_cmd_fire = (io_input_cmd_valid && io_input_cmd_ready);
  assign cmdArea_writeLogic_sel = (io_input_cmd_payload_first ? cmdArea_selStart : cmdArea_writeLogic_selReg);
  assign cmdArea_writeLogic_outputData_0 = io_output_cmd_payload_fragment_data[63 : 0];
  assign cmdArea_writeLogic_outputData_1 = io_output_cmd_payload_fragment_data[127 : 64];
  assign cmdArea_writeLogic_outputMask_0 = io_output_cmd_payload_fragment_mask[7 : 0];
  assign cmdArea_writeLogic_outputMask_1 = io_output_cmd_payload_fragment_mask[15 : 8];
  always @(*) begin
    io_output_cmd_payload_fragment_data[63 : 0] = io_input_cmd_payload_fragment_data;
    if(when_BmbUpSizerBridge_l85) begin
      io_output_cmd_payload_fragment_data[63 : 0] = cmdArea_writeLogic_dataRegs_0;
    end
    io_output_cmd_payload_fragment_data[127 : 64] = io_input_cmd_payload_fragment_data;
  end

  assign when_BmbUpSizerBridge_l85 = ((! io_input_cmd_payload_first) && (cmdArea_writeLogic_selReg != 1'b0));
  always @(*) begin
    io_output_cmd_payload_fragment_mask[7 : 0] = ((cmdArea_writeLogic_sel == 1'b0) ? io_input_cmd_payload_fragment_mask : cmdArea_writeLogic_maskRegs_0);
    io_output_cmd_payload_fragment_mask[15 : 8] = ((cmdArea_writeLogic_sel == 1'b1) ? io_input_cmd_payload_fragment_mask : 8'h0);
  end

  assign when_BmbUpSizerBridge_l95 = (io_input_cmd_valid && (cmdArea_writeLogic_sel == 1'b0));
  assign io_output_cmd_fire = (io_output_cmd_valid && io_output_cmd_ready);
  assign io_output_cmd_valid = (io_input_cmd_valid && ((cmdArea_writeLogic_sel == 1'b1) || io_input_cmd_payload_last));
  assign io_output_cmd_isStall = (io_output_cmd_valid && (! io_output_cmd_ready));
  assign io_input_cmd_ready = (! io_output_cmd_isStall);
  assign _zz_rspArea_context_selStart = io_output_rsp_payload_fragment_context;
  assign rspArea_context_selStart = _zz_rspArea_context_selStart[0 : 0];
  assign rspArea_context_selEnd = _zz_rspArea_context_selStart[1 : 1];
  assign io_input_rsp_valid = io_output_rsp_valid;
  assign io_input_rsp_payload_fragment_opcode = io_output_rsp_payload_fragment_opcode;
  assign io_input_rsp_payload_fragment_source = io_output_rsp_payload_fragment_source;
  assign io_input_rsp_fire = (io_input_rsp_valid && io_input_rsp_ready);
  assign rspArea_readLogic_sel = (io_input_rsp_payload_first ? rspArea_context_selStart : rspArea_readLogic_selReg);
  always @(*) begin
    io_input_rsp_payload_last = (io_output_rsp_payload_last && (rspArea_readLogic_sel == rspArea_context_selEnd));
    if(when_BmbUpSizerBridge_l133) begin
      io_input_rsp_payload_last = 1'b0;
    end
  end

  assign io_output_rsp_ready = (io_input_rsp_ready && (io_input_rsp_payload_last || (rspArea_readLogic_sel == 1'b1)));
  assign when_BmbUpSizerBridge_l133 = (rspArea_context_selEnd != rspArea_readLogic_sel);
  assign io_input_rsp_payload_fragment_data = _zz_io_input_rsp_payload_fragment_data;
  always @(posedge clk) begin
    if(reset) begin
      cmdArea_writeLogic_maskRegs_0 <= 8'h0;
      io_input_cmd_payload_first <= 1'b1;
      io_input_rsp_payload_first <= 1'b1;
    end else begin
      if(io_input_cmd_fire) begin
        io_input_cmd_payload_first <= io_input_cmd_payload_last;
      end
      if(when_BmbUpSizerBridge_l95) begin
        cmdArea_writeLogic_maskRegs_0 <= io_input_cmd_payload_fragment_mask;
      end
      if(io_output_cmd_fire) begin
        cmdArea_writeLogic_maskRegs_0 <= 8'h0;
      end
      if(io_input_rsp_fire) begin
        io_input_rsp_payload_first <= io_input_rsp_payload_last;
      end
    end
  end

  always @(posedge clk) begin
    if(io_input_cmd_fire) begin
      cmdArea_writeLogic_selReg <= (cmdArea_writeLogic_sel + 1'b1);
    end
    if(!when_BmbUpSizerBridge_l85) begin
      cmdArea_writeLogic_dataRegs_0 <= io_input_cmd_payload_fragment_data;
    end
    rspArea_readLogic_selReg <= rspArea_readLogic_sel;
    if(io_input_rsp_fire) begin
      rspArea_readLogic_selReg <= (rspArea_readLogic_sel + 1'b1);
    end
  end


endmodule
