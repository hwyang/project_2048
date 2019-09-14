//record each num in each grid, and all possible output of move
`define S16 s15,s14,s13,s12,s11,s10,s9,s8,s7,s6,s5,s4,s3,s2,s1,s0
`define U16 u15,u14,u13,u12,u11,u10,u9,u8,u7,u6,u5,u4,u3,u2,u1,u0
`define D16 d15,d14,d13,d12,d11,d10,d9,d8,d7,d6,d5,d4,d3,d2,d1,d0
`define L16 l15,l14,l13,l12,l11,l10,l9,l8,l7,l6,l5,l4,l3,l2,l1,l0
`define R16 r15,r14,r13,r12,r11,r10,r9,r8,r7,r6,r5,r4,r3,r2,r1,r0
`define P16 p15,p14,p13,p12,p11,p10,p9,p8,p7,p6,p5,p4,p3,p2,p1,p0
//record position of grid
`define b1 (43)
`define b2 (`b1 + 18)
`define b3 (`b2 + 18)
`define b4 (`b3 + 18)
`define b5 (`b4 + 18)
`define t -`b1+18

module Top(rst, clk, key, Hsync, Vsync, vgaRed, vgaGreen, vgaBlue, seg,an, PS2_DATA, PS2_CLK, pmod_1, pmod_2, pmod_4);
	input rst, clk;
	input [3:0] key;
	
	//Keyboard
	inout PS2_DATA;
	inout PS2_CLK;
	reg [4:0] key_num;
	wire [511:0] key_down;
	wire [8:0] last_change;
	wire been_ready;
	reg [5:0] keyboard=6'b000000;
	
	//VGA
	output Hsync,Vsync;
	output [3:0] vgaRed,vgaGreen,vgaBlue;
	
	//7-segment
	output reg [6:0] seg;
	output reg [3:0] an;
	reg [3:0] display;
	
	//speaker stuff
	output pmod_1;
	output pmod_2;
	output pmod_4;
	Speaker(clk, rst, state, scores, pmod_1, pmod_2, pmod_4);
	
	//score
	reg [13:0] scores;
	wire [3:0] scores_1000,scores_100,scores_10,scores_1;
	
	//Generate
	reg [3:0] `P16,`S16;
	wire [3:0] `U16,`D16,`L16,`R16,generate_up3,generate_up2,generate_up1,generate_up0,generate_down15,generate_down14,generate_down13,generate_down12,generate_left12,generate_left8,generate_left4,generate_left0,generate_right15,generate_right11,generate_right7,generate_right3;
	reg [15:0] counter;
	
	//Clock divider
	wire clk_game,clk_rand,clk_vga,clk_fpga,clk_lose,clk22;
	wire [3:0] key_,_key;
	wire [3:0] keyboard_;
	wire [9:0] keyin;
	
	//state transition
	reg [2:0] state;//0 input/move, 1 generate, 2 check game state, 3 loss, 4 start
	reg [1:0] status;//1 dead, 2 win
	
	//CLOCK_DIVIDER
	Clock_Divider Clock_Div(rst, clk, clk_game, clk_rand, clk_vga, clk_fpga, clk_lose, clk22);
	
	//SCORE
	assign scores_1000 = scores/14'd1000;
	assign scores_100 = (scores%14'd1000)/14'd100;
	assign scores_10 = (scores%14'd100)/14'd10;
	assign scores_1 = scores%14'd10;
	always@(posedge clk_game or posedge rst) begin
		if(rst) begin
			scores<=14'd0;
		end else begin
			scores<=((key_||keyboard_)&&state!=3&&({`S16}!={`P16}))?scores+14'd1:scores;
		end
	end
	
	
	//Db and OnePulse
	debounce db1(_key[3],key[3],clk_game);
	debounce db2(_key[2],key[2],clk_game);
	debounce db3(_key[1],key[1],clk_game);
	debounce db4(_key[0],key[0],clk_game);
	OnePulse op1(key_[3],_key[3],clk_game);
	OnePulse op2(key_[2],_key[2],clk_game);
	OnePulse op3(key_[1],_key[1],clk_game);
	OnePulse op4(key_[0],_key[0],clk_game);
	OnePulse op5(keyboard_[0],keyboard[0],clk_game);
	OnePulse op6(keyboard_[1],keyboard[1],clk_game);
	OnePulse op7(keyboard_[2],keyboard[2],clk_game);
	OnePulse op8(keyboard_[3],keyboard[3],clk_game);
	Shake Shake_3(rst,clk_game,key[3],keyin[3]);
	Shake Shake_2(rst,clk_game,key[2],keyin[2]);
	Shake Shake_1(rst,clk_game,key[1],keyin[1]);
	Shake Shake_0(rst,clk_game,key[0],keyin[0]);
	Shake Shake_4(rst,clk_game,keyboard[0],keyin[4]);
	Shake Shake_5(rst,clk_game,keyboard[1],keyin[5]);
	Shake Shake_6(rst,clk_game,keyboard[2],keyin[6]);
	Shake Shake_7(rst,clk_game,keyboard[3],keyin[7]);
	Shake Shake_8(rst,clk_game,keyboard[4],keyin[8]);
	Shake Shake_9(rst,clk_game,keyboard[5],keyin[9]);//space to start
	
	//Move
	Move move_up3(s3,s7,s11,s15,u3,u7,u11,u15);
	Move move_up2(s2,s6,s10,s14,u2,u6,u10,u14);
	Move move_up1(s1,s5,s9,s13,u1,u5,u9,u13);
	Move move_up0(s0,s4,s8,s12,u0,u4,u8,u12);
	Move move_down3(s15,s11,s7,s3,d15,d11,d7,d3);
	Move move_down2(s14,s10,s6,s2,d14,d10,d6,d2);
	Move move_down1(s13,s9,s5,s1,d13,d9,d5,d1);
	Move move_down0(s12,s8,s4,s0,d12,d8,d4,d0);
	Move move_left3(s12,s13,s14,s15,l12,l13,l14,l15);
	Move move_left2(s8,s9,s10,s11,l8,l9,l10,l11);
	Move move_left1(s4,s5,s6,s7,l4,l5,l6,l7);
	Move move_left0(s0,s1,s2,s3,l0,l1,l2,l3);
	Move move_right3(s15,s14,s13,s12,r15,r14,r13,r12);
	Move move_right2(s11,s10,s9,s8,r11,r10,r9,r8);
	Move move_right1(s7,s6,s5,s4,r7,r6,r5,r4);
	Move move_right0(s3,s2,s1,s0,r3,r2,r1,r0);
			
	//Generate random number in the opposite side
	Generate generate_up(rst,clk_rand,s3,s2,s1,s0,generate_up3,generate_up2,generate_up1,generate_up0);
	Generate generate_down(rst,clk_rand,s15,s14,s13,s12,generate_down15,generate_down14,generate_down13,generate_down12);
	Generate generate_left(rst,clk_rand,s12,s8,s4,s0,generate_left12,generate_left8,generate_left4,generate_left0);
	Generate generate_right(rst,clk_rand,s15,s11,s7,s3,generate_right15,generate_right11,generate_right7,generate_right3);
	
	//Keyboard
	KeyboardDecoder key_de(.key_down(key_down), .last_change(last_change), .key_valid(been_ready), .PS2_DATA(PS2_DATA), .PS2_CLK(PS2_CLK), .rst(rst), .clk(clk));
	always @(posedge clk or posedge rst)begin
		if (rst) begin
			keyboard <= 6'b000000;
		end
		else begin
			case(state)
				3'b010:
					keyboard <= 6'b000000;
				3'b011:
					keyboard <= 6'b000000;
				default:begin
					keyboard <= keyboard;
					if (been_ready && key_down[last_change] == 1'b1) begin
						case(key_num)
							5'd0:
								keyboard <= 6'b000010; 
							5'd1:
								keyboard <= 6'b000100;
							5'd2:
								keyboard <= 6'b000001;
							5'd3:
								keyboard <= 6'b001000;
							5'd4:
								keyboard <= 6'b010000;
							5'd5:
								keyboard <= 6'b100000;
							default:
								keyboard <= 6'b000000;
						endcase
					end
				end
			endcase
		end
	end
	always @ (*) begin
		case (last_change)
			KEY_CODES[00] : key_num = 5'b0000;
			KEY_CODES[01] : key_num = 5'b0001;
			KEY_CODES[02] : key_num = 5'b0010;
			KEY_CODES[03] : key_num = 5'b0011;
			KEY_CODES[04] : key_num = 5'b0100;
			KEY_CODES[05] : key_num = 5'b0101;
			default       : key_num = 4'b1111;
		endcase
	end
	parameter [8:0] KEY_CODES [0:5] = {
		9'b0_0001_1100, // left A => 1C
		9'b0_0001_1011, // down S => 1B
		9'b0_0010_0011, // right D => 23
		9'b0_0001_1101, // up W => 1D
		9'b0_0101_1010, // return => 5A
		9'b0_0010_1001  // space => start
	};
	
	//7-segment
	always@(posedge rst or posedge clk_fpga) begin
		if(rst) begin
			an<=4'b1111;
			display<=4'd0;
		end else begin
			case(state)
				2'b11:begin
					case(an)
						4'b1110:begin
							an<=4'b1101;
							display<=(clk_lose)?4'd12:4'd5;
						end
						4'b1101:begin
							an<=4'b1011;
							display<=(clk_lose)?4'd12:4'd0;
						end
						4'b1011:begin
							an<=4'b0111;
							display<=(clk_lose)?4'd12:4'd10;
						end
						4'b0111:begin
							an<=4'b1110;
							display<=(clk_lose)?4'd12:4'd11;
						end
						default:begin
							an<=4'b1110;
							display<=(clk_lose)?4'd12:4'd0;
						end
					endcase
				end
				default:begin
					case(an)
						4'b1110:begin
							an<=4'b1101;
							display<=scores_10;
						end
						4'b1101:begin
							an<=4'b1011;
							display<=scores_100;
						end
						4'b1011:begin
							an<=4'b0111;
							display<=scores_1000;
						end
						4'b0111:begin
							an<=4'b1110;
							display<=scores_1;
						end
						default:begin
							an<=4'b1110;
							display<=scores_1;
						end
					endcase
				end
			endcase
		end
	end
	always @(*) begin
		case(display)
			4'd0:seg = 7'b1000000;
			4'd1:seg = 7'b1111001;
			4'd2:seg = 7'b0100100;
			4'd3:seg = 7'b0110000;
			4'd4:seg = 7'b0011001;
			4'd5:seg = 7'b0010010;
			4'd6:seg = 7'b0000010;
			4'd7:seg = 7'b1111000;
			4'd8:seg = 7'b0000000;
			4'd9:seg = 7'b0010000;
			4'd10:seg = 7'b1000111;//L
			4'd11:seg = 7'b0000110;//E
			default:seg = 7'b1111111;
		endcase
	end
	//state transition
	always @(posedge rst or posedge clk_game) begin
		if(rst) begin
			s0 <= 4'h0;
			s1 <= 4'h1;
			s2 <= 4'h0;
			s3 <= 4'h0;
			s4 <= 4'h0; // s7 change 0->1
			s5 <= 4'h0;
			s6 <= 4'h0;
			s7 <= 4'h1;
			s8 <= 4'h0;
			s9 <= 4'h0;
			s10 <= 4'h2;
			s11 <= 4'h0;
			s12 <= 4'h2;
			s13 <= 4'h0;
			s14 <= 4'h1;
			s15 <= 4'h0;
			{`P16} <= 0;
			status <= 0;
			state <= 4;
		end else begin
			case(state)
				3'b100:begin
					case(keyin)
						10'b1000000000 :begin
							state<=0; 
						end
					endcase
				end
				3'b000 : begin
					case(keyin)
						10'b0000000000 : begin 
							{`S16} <= {`S16};
							{`P16} <= {`P16};
							state <= 0;
						end
						10'b0010000000 : begin 
							{`S16} <= {`U16};
							{`P16} <= {`S16};
							state <= 1;
						end
						10'b0001000000 : begin 
							{`S16} <= {`D16};
							{`P16} <= {`S16};
							state <= 1;
						end
						10'b0000100000 : begin 
							{`S16} <= {`L16};
							{`P16} <= {`S16};
							state <= 1;
						end
						10'b0000010000 : begin 
							{`S16} <= {`R16};
							{`P16} <= {`S16};
							state <= 1;
						end
						10'b0000001000 : begin 
							{`S16} <= {`U16};
							{`P16} <= {`S16};
							state <= 1;
						end
						10'b0000000100 : begin 
							{`S16} <= {`D16};
							{`P16} <= {`S16};
							state <= 1;
						end
						10'b0000000010 : begin 
							{`S16} <= {`L16};
							{`P16} <= {`S16};
							state <= 1;
						end
						10'b0000000001 : begin 
							{`S16} <= {`R16};
							{`P16} <= {`S16};
							state <= 1;
						end
						10'b0100000000:begin
							{`S16} <= {`P16};
							{`P16} <= {`P16};
							state<=1;
						end
						default : begin 
							{`S16} <= {`S16};
							{`P16} <= {`P16};
							state <= 0;
						end
					endcase
					status <= status;//continue game `b1
				end
				3'b001 : begin
					if({`P16} == {`S16})begin
						{`S16} <= {`S16};
					end else begin
						case(keyin)
							10'b0010000000 : {`S16} <= {s15,s14,s13,s12,s11,s10,s9,s8,s7,s6,s5,s4,generate_up3,generate_up2,generate_up1,generate_up0};
							10'b0001000000 : {`S16} <= {generate_down15,generate_down14,generate_down13,generate_down12,s11,s10,s9,s8,s7,s6,s5,s4,s3,s2,s1,s0};
							10'b0000100000 : {`S16} <= {s15,s14,s13,generate_left12,s11,s10,s9,generate_left8,s7,s6,s5,generate_left4,s3,s2,s1,generate_left0};
							10'b0000010000 : {`S16} <= {generate_right15,s14,s13,s12,generate_right11,s10,s9,s8,generate_right7,s6,s5,s4,generate_right3,s2,s1,s0};
							10'b0000001000 : {`S16} <= {s15,s14,s13,s12,s11,s10,s9,s8,s7,s6,s5,s4,generate_up3,generate_up2,generate_up1,generate_up0};
							10'b0000000100 : {`S16} <= {generate_down15,generate_down14,generate_down13,generate_down12,s11,s10,s9,s8,s7,s6,s5,s4,s3,s2,s1,s0};
							10'b0000000010 : {`S16} <= {s15,s14,s13,generate_left12,s11,s10,s9,generate_left8,s7,s6,s5,generate_left4,s3,s2,s1,generate_left0};
							10'b0000000001 : {`S16} <= {generate_right15,s14,s13,s12,generate_right11,s10,s9,s8,generate_right7,s6,s5,s4,generate_right3,s2,s1,s0};
							10'b0100000000 : {`S16} <= {`P16};
							default : {`S16} <= {s15,s14,s13,s12,s11,s10,s9,s8,s7,s6,s5,s4,s3,s2,s1,s0};
						endcase
					end
					{`P16} <= {`P16};
					state <= 2;
					status <= status;
				end
				3'b010 : begin
					{`S16} <= {`S16};
					{`P16} <= {`P16};
					if({`U16}=={`D16} && {`L16}=={`R16})begin //loss
						status <= 1;
						state<= 3;
					end else if(keyin == 9'b000000000) begin
						if(s15==4'd11||s14==4'd11||s13==4'd11||s12==4'd11||s11==4'd11||s10==4'd11||s9==4'd11||s8==4'd11||s7==4'd11||s6==4'd11||s5==4'd11||s4==4'd11||s3==4'd11||s2==4'd11||s1==4'd11||s0==4'd11)
							status <= 2;//win
						else status <= status;
						state <= 0;
					end else begin
						status <= status;
						state <= 2;
					end
				end
				3'b011 : begin
					{`S16} <= {`S16};
					{`P16} <= {`P16};
					status <= status;
					state <= 3;
				end
				default : begin
					{`S16} <= {`S16};
					{`P16} <= {`P16};
					status <= status;
					state <= 4;
				end
			endcase //case(state) end
		end
	end
	

	//===============================================
		
	//-------------display on the screen-------------

	//===============================================
	wire [0:287] e15,e14,e13,e12,e11,e10,e9,e8,e7,e6,e5,e4,e3,e2,e1,e0;
	reg [9:0] position;
	reg [7:0] din;
	wire [6:0] xin,xin_shift;
	wire [2:0] yin;
	assign xin = position[6:0];
	assign yin = position[9:7];
	assign xin_shift = xin + 7'h01;
		
	Color_Display Color_Display15(s15,e15);
	Color_Display Color_Display14(s14,e14);
	Color_Display Color_Display13(s13,e13);
	Color_Display Color_Display12(s12,e12);
	Color_Display Color_Display11(s11,e11);
	Color_Display Color_Display10(s10,e10);
	Color_Display Color_Display9(s9,e9);
	Color_Display Color_Display8(s8,e8);
	Color_Display Color_Display7(s7,e7);
	Color_Display Color_Display6(s6,e6);
	Color_Display Color_Display5(s5,e5);
	Color_Display Color_Display4(s4,e4);
	Color_Display Color_Display3(s3,e3);
	Color_Display Color_Display2(s2,e2);
	Color_Display Color_Display1(s1,e1);
	Color_Display Color_Display0(s0,e0);

	reg [2:0] db;
	wire [9:0] vx,vy;
	wire [7:0] inx;
	wire [4:0] iny;
	wire [2:0] calcy;
	wire red,green,blue;

    wire [11:0] data;
    wire [16:0] pixel_addr;
    wire [11:0] pixel;
    wire valid;

    assign {vgaRed, vgaGreen, vgaBlue} = (state == 4) ? pixel:{{red,3'b111} , {green,3'b111},{blue,3'b111}};
 

	mem_addr_gen mem_addr_gen_inst(
    	.clk(clk22),
   		.rst(rst),
		.h_cnt(vx),
   		.v_cnt(vy),
    	.pixel_addr(pixel_addr)
    );
    blk_mem_gen_0 blk_mem_gen_0_inst(
      	.clka(clk_vga),
      	.wea(0),
     	.addra(pixel_addr),
      	.dina(data[11:0]),
      	.douta(pixel)
   	);

    vga_controller(clk_vga,rst,Hsync,Vsync,valid,vx,vy);
    assign {red,green,blue} = (valid == 1 ? db : 0);
	assign inx = vx[9:2]; //vertical position
	assign iny = vy[9:5]; //horizontal position
	assign calcy = vy[4:2]; //define the horizontal line

	always @(inx or iny) begin
		case(iny)
			5'h03 :if(inx==`b1||inx==`b2||inx==`b3||inx==`b4||inx==`b5) db <= 3'b000;//define the vertical a white line
			  else if(inx<`b1) db <= 3'b111;
			  else if(inx<= `b5 && calcy == 3'b000) db <= 3'b000;//the horizontal white line
			  else if(inx<`b2) db <= {3{e15[(inx-`b1)*8+calcy]}} ^ (s15[2:0]);//let the number have a different color
			  else if(inx<`b3) db <= {3{e14[(inx-`b2)*8+calcy]}} ^ (s14[2:0]);
			  else if(inx<`b4) db <= {3{e13[(inx-`b3)*8+calcy]}} ^ (s13[2:0]);
			  else if(inx<`b5) db <= {3{e12[(inx-`b4)*8+calcy]}} ^ (s12[2:0]);
			  else db <= 3'b111;
			5'h04 :if(inx==`b1||inx==`b2||inx==`b3||inx==`b4||inx==`b5) db <= 3'b000;              
			  else if(inx<`b1) db <= 3'b111;
			  else if(inx<`b2) db <= {3{e15[(inx+`t)*8+calcy]}} ^ (s15[2:0]);
			  else if(inx<`b3) db <= {3{e14[(inx-`b1)*8+calcy]}} ^ (s14[2:0]);
			  else if(inx<`b4) db <= {3{e13[(inx-`b2)*8+calcy]}} ^ (s13[2:0]);
			  else if(inx<`b5) db <= {3{e12[(inx-`b3)*8+calcy]}} ^ (s12[2:0]);
			  else db <= 3'b111;
			5'h05 :if(inx==`b1||inx==`b2||inx==`b3||inx==`b4||inx==`b5) db <= 3'b000;
			  else if(inx<`b1) db <= 3'b111;
			  else if(inx<=`b5 && calcy == 3'b000) db <= 3'b000;
			  else if(inx<`b2) db <= {3{e11[(inx-`b1)*8+calcy]}} ^ (s11[2:0]);
			  else if(inx<`b3) db <= {3{e10[(inx-`b2)*8+calcy]}} ^ (s10[2:0]);
			  else if(inx<`b4) db <= {3{ e9[(inx-`b3)*8+calcy]}} ^ ( s9[2:0]);
			  else if(inx<`b5) db <= {3{ e8[(inx-`b4)*8+calcy]}} ^ ( s8[2:0]);
			  else db <= 3'b111;
			5'h06 :if(inx==`b1||inx==`b2||inx==`b3||inx==`b4||inx==`b5) db <= 3'b000;
			  else if(inx<`b1) db <= 3'b111;
			  else if(inx<`b2) db <= {3{e11[(inx+`t)*8+calcy]}} ^ (s11[2:0]);
			  else if(inx<`b3) db <= {3{e10[(inx-`b1)*8+calcy]}} ^ (s10[2:0]);
			  else if(inx<`b4) db <= {3{ e9[(inx-`b2)*8+calcy]}} ^ ( s9[2:0]);
			  else if(inx<`b5) db <= {3{ e8[(inx-`b3)*8+calcy]}} ^ ( s8[2:0]);
			  else db <= 3'b111;
			5'h07 :if(inx==`b1||inx==`b2||inx==`b3||inx==`b4||inx==`b5) db <= 3'b000;
			  else if(inx<`b1) db <= 3'b111;
			  else if(inx<=`b5 && calcy == 3'b000) db <= 3'b000;
			  else if(inx<`b2) db <= {3{ e7[(inx-`b1)*8+calcy]}} ^ (s7[2:0]);
			  else if(inx<`b3) db <= {3{ e6[(inx-`b2)*8+calcy]}} ^ (s6[2:0]);
			  else if(inx<`b4) db <= {3{ e5[(inx-`b3)*8+calcy]}} ^ (s5[2:0]);
			  else if(inx<`b5) db <= {3{ e4[(inx-`b4)*8+calcy]}} ^ (s4[2:0]);
			  else db <= 3'b111;
			5'h08 :if(inx==`b1||inx==`b2||inx==`b3||inx==`b4||inx==`b5) db <= 3'b000;
			  else if(inx<`b1) db <= 3'b111;
			  else if(inx<`b2) db <= {3{ e7[(inx+`t)*8+calcy]}} ^ (s7[2:0]);
			  else if(inx<`b3) db <= {3{ e6[(inx-`b1)*8+calcy]}} ^ (s6[2:0]);
			  else if(inx<`b4) db <= {3{ e5[(inx-`b2)*8+calcy]}} ^ (s5[2:0]);
			  else if(inx<`b5) db <= {3{ e4[(inx-`b3)*8+calcy]}} ^ (s4[2:0]);
			  else db <= 3'b111;
			5'h09 :if(inx==`b1||inx==`b2||inx==`b3||inx==`b4||inx==`b5) db <= 3'b000;
			  else if(inx<`b1) db <= 3'b111;
			  else if(inx<=`b5 && calcy == 3'b000) db <= 3'b000;
			  else if(inx<`b2) db <= {3{ e3[(inx-`b1)*8+calcy]}} ^ (s3[2:0]);
			  else if(inx<`b3) db <= {3{ e2[(inx-`b2)*8+calcy]}} ^ (s2[2:0]);
			  else if(inx<`b4) db <= {3{ e1[(inx-`b3)*8+calcy]}} ^ (s1[2:0]);
			  else if(inx<`b5) db <= {3{ e0[(inx-`b4)*8+calcy]}} ^ (s0[2:0]);
			  else db <= 3'b111;
			5'h0a :if(inx==`b1||inx==`b2||inx==`b3||inx==`b4||inx==`b5) db <= 3'b000;
			  else if(inx<`b1) db <= 3'b111;
			  else if(inx<`b2) db <= {3{ e3[(inx+`t)*8+calcy]}} ^ (s3[2:0]);
			  else if(inx<`b3) db <= {3{ e2[(inx-`b1)*8+calcy]}} ^ (s2[2:0]);
			  else if(inx<`b4) db <= {3{ e1[(inx-`b2)*8+calcy]}} ^ (s1[2:0]);
			  else if(inx<`b5) db <= {3{ e0[(inx-`b3)*8+calcy]}} ^ (s0[2:0]);
			  else db <= 3'b111;
			5'h0b :if(inx>=`b1 && inx<=`b5 && calcy == 3'b000) db <= 3'b000;
			  else db <= 3'b111;
			default : db <= 3'b111;//case(iny)
		endcase
	end
endmodule

//Shake
module Shake(rst,clk,din,dout);
	input rst,clk,din;
	output reg dout;
	reg [31:0] fifo;
	
	always @(posedge rst or posedge clk) begin
		if(rst) fifo <= 0;
		else fifo[31:0] <= {fifo[30:0],din};
	end
	
	always @(fifo) begin
		if(fifo == 32'h00000000) dout <= 0;
		else if(fifo == 32'hffffffff) dout <= 1;
		else dout <= dout;
	end
endmodule

//Color
module Color_Display(din,dout);
    input [3:0] din;
    output reg [0:287] dout;
    
    always @(din) begin
        case(din)//show color of digits
			4'h0 : dout <= 288'h00_00_00_00_00_00_00_00_03_02_03_00_00_00_00_00_00_00_00_00_00_00_00_00_00_00_e0_20_e0_00_00_00_00_00_00_00;//0
			4'h1 : dout <= 288'h00_00_00_00_00_00_00_00_02_02_03_00_00_00_00_00_00_00_00_00_00_00_00_00_00_00_e0_a0_a0_00_00_00_00_00_00_00;//2
			4'h2 : dout <= 288'h00_00_00_00_00_00_00_00_03_00_03_00_00_00_00_00_00_00_00_00_00_00_00_00_00_00_80_80_e0_00_00_00_00_00_00_00;//4
			4'h3 : dout <= 288'h00_00_00_00_00_00_00_00_03_02_03_00_00_00_00_00_00_00_00_00_00_00_00_00_00_00_e0_a0_e0_00_00_00_00_00_00_00;//8
			4'h4 : dout <= 288'h00_00_00_00_00_00_00_03_00_00_03_02_02_00_00_00_00_00_00_00_00_00_00_00_00_e0_00_00_e0_a0_e0_00_00_00_00_00;//16
			4'h5 : dout <= 288'h00_00_00_00_00_00_02_02_03_00_02_02_03_00_00_00_00_00_00_00_00_00_00_00_a0_a0_e0_00_e0_a0_a0_00_00_00_00_00;//32
			4'h6 : dout <= 288'h00_00_00_00_00_00_03_02_02_00_03_00_03_00_00_00_00_00_00_00_00_00_00_00_e0_a0_e0_00_80_80_e0_00_00_00_00_00;//64
			4'h7 : dout <= 288'h00_00_00_00_00_03_00_00_02_02_03_00_03_02_03_00_00_00_00_00_00_00_00_e0_00_00_e0_a0_a0_00_e0_a0_e0_00_00_00;//128
			4'h8 : dout <= 288'h00_00_00_00_02_02_03_00_03_02_02_00_03_02_02_00_00_00_00_00_00_00_e0_a0_a0_00_a0_a0_e0_00_e0_a0_e0_00_00_00;//256
			4'h9 : dout <= 288'h00_00_00_00_03_02_02_00_00_03_00_00_02_02_03_00_00_00_00_00_00_00_a0_a0_e0_00_00_e0_00_00_e0_a0_a0_00_00_00;//512
			4'ha : dout <= 288'h00_00_00_03_00_00_03_02_03_00_02_02_03_00_03_00_03_00_00_00_00_e0_00_00_e0_20_e0_00_e0_a0_a0_00_80_80_e0_00;//1024
			4'hb : dout <= 288'h00_00_02_02_03_00_03_02_03_00_03_00_03_00_03_02_03_00_00_00_e0_a0_a0_00_e0_20_e0_00_80_80_e0_00_e0_a0_e0_00;//2048
			4'hc : dout <= 288'h00_00_03_00_03_00_03_02_03_00_03_02_03_00_03_02_02_00_00_00_80_80_e0_00_e0_20_e0_00_a0_a0_e0_00_e0_a0_e0_00;//4096
			4'hd : dout <= 288'h00_00_03_02_03_00_00_03_00_00_03_02_03_00_02_02_03_00_00_00_e0_a0_e0_00_00_e0_00_00_a0_a0_e0_00_e0_a0_a0_00;//8192
			default: dout<=288'hff_80_80_80_80_80_80_80_80_80_80_80_80_80_80_80_80_80_ff_00_00_00_00_00_00_00_00_00_00_00_00_00_00_00_00_00;//none
        endcase
    end
endmodule

//Random
module Random(rst,clk,ran2,ran3,ran4);
    input rst,clk;
    output reg [7:0] ran2;
    output reg [11:0] ran3;
    output reg [15:0] ran4;
  
    always @(posedge rst or posedge clk) begin
		if(rst) begin
		  ran2 <= 8'd1;ran3 <= 12'd1;ran4 <= 16'd1;
		end else begin
		  ran2 <= {ran2[3:0],ran2[7:4]};
		  ran3 <= {ran3[7:0],ran3[11:8]};
		  ran4 <= {ran4[11:0],ran4[15:12]};
		end
	end
endmodule

//Move
module Move(input3,input2,input1,input0,output3,output2,output1,output0);
	input [3:0] input3,input2,input1,input0;
	output reg [3:0] output3,output2,output1,output0;
	
	wire f3,f2,f1,f0;
	assign f3 = (input3!=4'h0);
	assign f2 = (input2!=4'h0);
	assign f1 = (input1!=4'h0);
	assign f0 = (input0!=4'h0);
	
	always @(*) begin
		case({f3,f2,f1,f0})
			4'b0000 : {output3,output2,output1,output0} <= {4'h0,4'h0,4'h0,4'h0};
			4'b0001 : {output3,output2,output1,output0} <= {4'h0,4'h0,4'h0,input0};
			4'b0010 : {output3,output2,output1,output0} <= {4'h0,4'h0,4'h0,input1};
			4'b0011 : if(input1!=input0) {output3,output2,output1,output0} <= {4'h0,4'h0,input1,input0};
			          else {output3,output2,output1,output0} <= {4'h0,4'h0,4'h0,input0+4'h1};
			4'b0100 : {output3,output2,output1,output0} <= {4'h0,4'h0,4'h0,input2};
			4'b0101 : if(input2!=input0) {output3,output2,output1,output0} <= {4'h0,4'h0,input2,input0};
			          else {output3,output2,output1,output0} <= {4'h0,4'h0,4'h0,input0+4'h1};
			4'b0110 : if(input2!=input1) {output3,output2,output1,output0} <= {4'h0,4'h0,input2,input1};
			          else {output3,output2,output1,output0} <= {4'h0,4'h0,4'h0,input1+4'h1};
			4'b0111 : if(input2!=input1 && input1!=input0) {output3,output2,output1,output0} <= {4'h0,input2,input1,input0};
			          else if(input1==input0) {output3,output2,output1,output0} <= {4'h0,4'h0,input2,input0+4'h1};
			          else {output3,output2,output1,output0} <= {4'h0,4'h0,input1+4'h1,input0};
			4'b1000 : {output3,output2,output1,output0} <= {4'h0,4'h0,4'h0,input3};
			4'b1001 : if(input3!=input0) {output3,output2,output1,output0} <= {4'h0,4'h0,input3,input0};
			          else {output3,output2,output1,output0} <= {4'h0,4'h0,4'h0,input0+4'h1};
			4'b1010 : if(input3!=input1) {output3,output2,output1,output0} <= {4'h0,4'h0,input3,input1};
			          else {output3,output2,output1,output0} <= {4'h0,4'h0,4'h0,input1+4'h1};
			4'b1011 : if(input3!=input1 && input1!=input0) {output3,output2,output1,output0} <= {4'h0,input3,input1,input0};
			          else if(input1==input0) {output3,output2,output1,output0} <= {4'h0,4'h0,input3,input0+4'h1};
			          else {output3,output2,output1,output0} <= {4'h0,4'h0,input1+4'h1,input0};
			4'b1100 : if(input3!=input2) {output3,output2,output1,output0} <= {4'h0,4'h0,input3,input2};
			          else {output3,output2,output1,output0} <= {4'h0,4'h0,4'h0,input2+4'h1};
			4'b1101 : if(input3!=input2 && input2!=input0) {output3,output2,output1,output0} <= {4'h0,input3,input2,input0};
			          else if(input2==input0) {output3,output2,output1,output0} <= {4'h0,4'h0,input3,input0+4'h1};
			          else {output3,output2,output1,output0} <= {4'h0,4'h0,input2+4'h1,input0};
			4'b1110 : if(input3!=input2 && input2!=input1) {output3,output2,output1,output0} <= {4'h0,input3,input2,input1};
			          else if(input2==input1) {output3,output2,output1,output0} <= {4'h0,4'h0,input3,input1+4'h1};
			          else {output3,output2,output1,output0} <= {4'h0,4'h0,input2+4'h1,input1};
			4'b1111 : if(input3!=input2 && input2!=input1 && input1!=input0) {output3,output2,output1,output0} <= {input3,input2,input1,input0};
			          else if(input3==input2 && input1==input0) {output3,output2,output1,output0} <= {4'h0,4'h0,input2+4'h1,input0+4'h1};
			          else if(input1==input0) {output3,output2,output1,output0} <= {4'h0,input3,input2,input0+4'h1};
			          else if(input2==input1) {output3,output2,output1,output0} <= {4'h0,input3,input1+4'h1,input0};
			          else {output3,output2,output1,output0} <= {4'h0,input2+4'h1,input1,input0};
			default : {output3,output2,output1,output0} <= {4'h0,4'h0,4'h0,4'h0};
		endcase
	end
endmodule

//Generate
module Generate(rst,clk,input3,input2,input1,input0,output3,output2,output1,output0);
    input rst,clk;
    input [3:0] input3,input2,input1,input0;
    output reg [3:0] output3,output2,output1,output0;
  
	wire f3,f2,f1,f0;
	assign f3=(input3!=4'h0);
	assign f2=(input2!=4'h0);
	assign f1=(input1!=4'h0);
	assign f0=(input0!=4'h0);
	
    wire [7:0] ran2;
    wire [11:0] ran3;
    wire [15:0] ran4;
  
	Random ran(rst,clk,ran2,ran3,ran4);
  
    always @(*) begin
		case({f3,f2,f1,f0})
			4'b0000 : {output3,output2,output1,output0} <= {ran4};
			4'b0001 : {output3,output2,output1,output0} <= {ran3,input0};
			4'b0010 : {output3,output2,output1,output0} <= {ran3[11:8],ran3[7:4],input1,ran3[3:0]};
			4'b0011 : {output3,output2,output1,output0} <= {ran2[7:4],ran2[3:0],input1,input0};
			4'b0100 : {output3,output2,output1,output0} <= {ran3[11:8],input2,ran3[7:4],ran3[3:0]};
			4'b0101 : {output3,output2,output1,output0} <= {ran2[7:4],input2,ran2[3:0],input0};
			4'b0110 : {output3,output2,output1,output0} <= {ran2[7:4],input2,input1,ran2[3:0]};
			4'b0111 : {output3,output2,output1,output0} <= {4'h1,input2,input1,input0};
			4'b1000 : {output3,output2,output1,output0} <= {input3,ran3};
			4'b1001 : {output3,output2,output1,output0} <= {input3,ran2[7:4],ran2[3:0],input0};
			4'b1010 : {output3,output2,output1,output0} <= {input3,ran2[7:4],input1,ran2[3:0]};
			4'b1011 : {output3,output2,output1,output0} <= {input3,4'h1,input1,input0};
			4'b1100 : {output3,output2,output1,output0} <= {input3,input2,ran2[7:4],ran2[3:0]};
			4'b1101 : {output3,output2,output1,output0} <= {input3,input2,4'h1,input0};
			4'b1110 : {output3,output2,output1,output0} <= {input3,input2,input1,4'h1};
			4'b1111 : {output3,output2,output1,output0} <= {input3,input2,input1,input0};
			default : {output3,output2,output1,output0} <= {input3,input2,input1,input0};
		endcase
	end
endmodule

//Clock Divider
module Clock_Divider(rst,clk,clk_game,clk_rand,clk_vga,clk_fpga,clk_lose,clk22);

	reg [25:0] counter;
	output wire clk_game,clk_rand,clk_vga,clk_fpga,clk_lose,clk22;
	input wire clk,rst;

	assign clk_game = counter[10];
	assign clk_rand = counter[15];
	assign clk_vga = counter[1];
	assign clk_fpga = counter[16];
	assign clk_lose =  counter[25];
	assign clk22 = counter[22];

	always @(posedge rst or posedge clk) begin
		if(rst) counter <= 0;
		else counter <= counter + 1;
	end
endmodule

//OnePulse
module OnePulse(output reg single_signal,input wire signal,input clk);
	reg delay;
	always@(posedge clk)begin
		single_signal<=signal&&(!delay);
		delay<=signal;
	end
endmodule

//Debounce
module debounce (output wire debounced,input wire pb,input wire clk);
	reg [3:0] DFF;
	always@(posedge clk)begin
		DFF[3:1]<=DFF[2:0];
		DFF[0]<=pb;
	end
	assign debounced=(DFF==4'b1111)?1'b1:1'b0;
endmodule

//Keyboard_Decoder
module KeyboardDecoder(
	output reg [511:0] key_down,
	output wire [8:0] last_change,
	output reg key_valid,
	inout wire PS2_DATA,
	inout wire PS2_CLK,
	input wire rst,
	input wire clk
    );
    
    parameter [1:0] INIT			= 2'b00;
    parameter [1:0] WAIT_FOR_SIGNAL = 2'b01;
    parameter [1:0] GET_SIGNAL_DOWN = 2'b10;
    parameter [1:0] WAIT_RELEASE    = 2'b11;
    
	parameter [7:0] IS_INIT			= 8'hAA;
    parameter [7:0] IS_EXTEND		= 8'hE0;
    parameter [7:0] IS_BREAK		= 8'hF0;
    
    reg [9:0] key;		// key = {been_extend, been_break, key_in}
    reg [1:0] state;
    reg been_ready, been_extend, been_break;
    
    wire [7:0] key_in;
    wire is_extend;
    wire is_break;
    wire valid;
    wire err;
    
    wire [511:0] key_decode = 1 << last_change;
    assign last_change = {key[9], key[7:0]};
    
    KeyboardCtrl inst (
		.key_in(key_in),
		.is_extend(is_extend),
		.is_break(is_break),
		.valid(valid),
		.err(err),
		.PS2_DATA(PS2_DATA),
		.PS2_CLK(PS2_CLK),
		.rst(rst),
		.clk(clk)
	);
	
	OnePulse op (
		.single_signal(pulse_been_ready),
		.signal(been_ready),
		.clk(clk)
	);
    
    always @ (posedge clk, posedge rst) begin
    	if (rst) begin
    		state <= INIT;
    		been_ready  <= 1'b0;
    		been_extend <= 1'b0;
    		been_break  <= 1'b0;
    		key <= 10'b0_0_0000_0000;
    	end else begin
    		state <= state;
			been_ready  <= been_ready;
			been_extend <= (is_extend) ? 1'b1 : been_extend;
			been_break  <= (is_break ) ? 1'b1 : been_break;
			key <= key;
    		case (state)
    			INIT : begin
    					if (key_in == IS_INIT) begin
    						state <= WAIT_FOR_SIGNAL;
    						been_ready  <= 1'b0;
							been_extend <= 1'b0;
							been_break  <= 1'b0;
							key <= 10'b0_0_0000_0000;
    					end else begin
    						state <= INIT;
    					end
    				end
    			WAIT_FOR_SIGNAL : begin
    					if (valid == 0) begin
    						state <= WAIT_FOR_SIGNAL;
    						been_ready <= 1'b0;
    					end else begin
    						state <= GET_SIGNAL_DOWN;
    					end
    				end
    			GET_SIGNAL_DOWN : begin
						state <= WAIT_RELEASE;
						key <= {been_extend, been_break, key_in};
						been_ready  <= 1'b1;
    				end
    			WAIT_RELEASE : begin
    					if (valid == 1) begin
    						state <= WAIT_RELEASE;
    					end else begin
    						state <= WAIT_FOR_SIGNAL;
    						been_extend <= 1'b0;
    						been_break  <= 1'b0;
    					end
    				end
    			default : begin
    					state <= INIT;
						been_ready  <= 1'b0;
						been_extend <= 1'b0;
						been_break  <= 1'b0;
						key <= 10'b0_0_0000_0000;
    				end
    		endcase
    	end
    end
    
    always @ (posedge clk, posedge rst) begin
    	if (rst) begin
    		key_valid <= 1'b0;
    		key_down <= 511'b0;
    	end else if (key_decode[last_change] && pulse_been_ready) begin
    		key_valid <= 1'b1;
    		if (key[8] == 0) begin
    			key_down <= key_down | key_decode;
    		end else begin
    			key_down <= key_down & (~key_decode);
    		end
    	end else begin
    		key_valid <= 1'b0;
			key_down <= key_down;
    	end
    end
endmodule

module KeyboardCtrl#(
   parameter SYSCLK_FREQUENCY_HZ = 100000000
)(
    output reg [7:0] key_in,
    output reg is_extend,
    output reg is_break,
	output reg valid,
    output err,
    inout PS2_DATA,
    inout PS2_CLK,
    input rst,
    input clk
);
    parameter RESET          = 3'd0;
	parameter SEND_CMD       = 3'd1;
	parameter WAIT_ACK       = 3'd2;
    parameter WAIT_KEYIN     = 3'd3;
	parameter GET_BREAK      = 3'd4;
	parameter GET_EXTEND     = 3'd5;
	parameter RESET_WAIT_BAT = 3'd6;
    
    parameter CMD_RESET           = 8'hFF; 
    parameter CMD_SET_STATUS_LEDS = 8'hED;
	parameter RSP_ACK             = 8'hFA;
	parameter RSP_BAT_PASS        = 8'hAA;
    
    parameter BREAK_CODE  = 8'hF0;
    parameter EXTEND_CODE = 8'hE0;
    parameter CAPS_LOCK   = 8'h58;
    parameter NUM_LOCK    = 8'h77;
    parameter SCR_LOCK    = 8'h7E;
    
    wire [7:0] rx_data;
	wire rx_valid;
	wire busy;
	
	reg [7:0] tx_data;
	reg tx_valid;
	reg [2:0] state;
	reg [2:0] lock_status;
	
	always @ (posedge clk, posedge rst)
	  if(rst)
	    key_in <= 0;
	  else if(rx_valid)
	    key_in <= rx_data;
	  else
	    key_in <= key_in;
	
	always @ (posedge clk, posedge rst)begin
	  if(rst)begin
	    state <= RESET;
        is_extend <= 1'b0;
        is_break <= 1'b1;
		valid <= 1'b0;
		lock_status <= 3'b0;
		tx_data <= 8'h00;
		tx_valid <= 1'b0;
	  end else begin
	    is_extend <= 1'b0;
	    is_break <= 1'b0;
	    valid <= 1'b0;
	    lock_status <= lock_status;
	    tx_data <= tx_data;
	    tx_valid <= 1'b0;
	    case(state)
	      RESET:begin
	          is_extend <= 1'b0;
              is_break <= 1'b1;
		      valid <= 1'b0;
		      lock_status <= 3'b0;
		      tx_data <= CMD_RESET;
		      tx_valid <= 1'b0;
			  state <= SEND_CMD;
	        end
		  
		  SEND_CMD:begin
		      if(busy == 1'b0)begin
			    tx_valid <= 1'b1;
				state <= WAIT_ACK;
			  end else begin
			    tx_valid <= 1'b0;
				state <= SEND_CMD;
		      end
		    end
	      
		  WAIT_ACK:begin
		      if(rx_valid == 1'b1)begin
			    if(rx_data == RSP_ACK && tx_data == CMD_RESET)begin
				  state <= RESET_WAIT_BAT;
				end else if(rx_data == RSP_ACK && tx_data == CMD_SET_STATUS_LEDS)begin
				  tx_data <= {5'b00000, lock_status};
				  state <= SEND_CMD;
				end else begin
				  state <= WAIT_KEYIN;
				end
			  end else if(err == 1'b1)begin
			    state <= RESET;
			  end else begin
			    state <= WAIT_ACK;
			  end
		    end
			
		  WAIT_KEYIN:begin
		      if(rx_valid == 1'b1 && rx_data == BREAK_CODE)begin
			    state <= GET_BREAK;
			  end else if(rx_valid == 1'b1 && rx_data == EXTEND_CODE)begin
			    state <= GET_EXTEND;
			  end else if(rx_valid == 1'b1)begin
			    state <= WAIT_KEYIN;
				valid <= 1'b1;
			  end else if(err == 1'b1)begin
			    state <= RESET;
			  end else begin
			    state <= WAIT_KEYIN;
			  end
		    end
		    
		  GET_BREAK:begin
		      is_extend <= is_extend;
		      if(rx_valid == 1'b1)begin
			    state <= WAIT_KEYIN;
                valid <= 1'b1;
				is_break <= 1'b1;
			  end else if(err == 1'b1)begin
			    state <= RESET;
			  end else begin
			    state <= GET_BREAK;
			  end
		    end
			
		  GET_EXTEND:begin
		      if(rx_valid == 1'b1 && rx_data == BREAK_CODE)begin
		        state <= GET_BREAK;
		        is_extend <= 1'b1;
		      end else if(rx_valid == 1'b1)begin
		        state <= WAIT_KEYIN;
                valid <= 1'b1;
		        is_extend <= 1'b1;
			  end else if(err == 1'b1)begin
			    state <= RESET;
		      end else begin
		        state <= GET_EXTEND;
		      end
		    end
			
		  RESET_WAIT_BAT:begin
		      if(rx_valid == 1'b1 && rx_data == RSP_BAT_PASS)begin
			    state <= WAIT_KEYIN;
			  end else if(rx_valid == 1'b1)begin
			    state <= RESET;
			  end else if(err == 1'b1)begin
			    state <= RESET;
			  end else begin
			    state <= RESET_WAIT_BAT;
			  end
		    end
		  default:begin
		      state <= RESET;
		      valid <= 1'b0;
		    end
		endcase
	  end
	end
	
    Ps2Interface #(
      .SYSCLK_FREQUENCY_HZ(SYSCLK_FREQUENCY_HZ)
    ) Ps2Interface_i(
      .ps2_clk(PS2_CLK),
      .ps2_data(PS2_DATA),
      
      .clk(clk),
      .rst(rst),
      
      .tx_data(tx_data),
      .tx_valid(tx_valid),
      
      .rx_data(rx_data),
      .rx_valid(rx_valid),
      
      .busy(busy),
      .err(err)
    );
        
endmodule
module Ps2Interface#(
    parameter SYSCLK_FREQUENCY_HZ = 100000000
  )(
  ps2_clk,
  ps2_data,

  clk,
  rst,

  tx_data,
  tx_valid,

  rx_data,
  rx_valid,

  busy,
  err
);
  inout ps2_clk, ps2_data;
  input clk, rst;
  input [7:0] tx_data;
  input tx_valid;
  output reg [7:0] rx_data;
  output reg rx_valid;
  output busy;
  output reg err;
  
  parameter CLOCK_CNT_100US = (100*1000) / (1000000000/SYSCLK_FREQUENCY_HZ);
  parameter CLOCK_CNT_20US = (20*1000) / (1000000000/SYSCLK_FREQUENCY_HZ);
  parameter DEBOUNCE_DELAY = 15;
  parameter BITS_NUM = 11;
  
  parameter [0:0] parity_table [0:255] = {    //(odd) parity bit table, used instead of logic because this way speed is far greater
    1'b1,1'b0,1'b0,1'b1,1'b0,1'b1,1'b1,1'b0,
    1'b0,1'b1,1'b1,1'b0,1'b1,1'b0,1'b0,1'b1,
    1'b0,1'b1,1'b1,1'b0,1'b1,1'b0,1'b0,1'b1,
    1'b1,1'b0,1'b0,1'b1,1'b0,1'b1,1'b1,1'b0,
    1'b0,1'b1,1'b1,1'b0,1'b1,1'b0,1'b0,1'b1,
    1'b1,1'b0,1'b0,1'b1,1'b0,1'b1,1'b1,1'b0,
    1'b1,1'b0,1'b0,1'b1,1'b0,1'b1,1'b1,1'b0,
    1'b0,1'b1,1'b1,1'b0,1'b1,1'b0,1'b0,1'b1,
    1'b0,1'b1,1'b1,1'b0,1'b1,1'b0,1'b0,1'b1,
    1'b1,1'b0,1'b0,1'b1,1'b0,1'b1,1'b1,1'b0,
    1'b1,1'b0,1'b0,1'b1,1'b0,1'b1,1'b1,1'b0,
    1'b0,1'b1,1'b1,1'b0,1'b1,1'b0,1'b0,1'b1,
    1'b1,1'b0,1'b0,1'b1,1'b0,1'b1,1'b1,1'b0,
    1'b0,1'b1,1'b1,1'b0,1'b1,1'b0,1'b0,1'b1,
    1'b0,1'b1,1'b1,1'b0,1'b1,1'b0,1'b0,1'b1,
    1'b1,1'b0,1'b0,1'b1,1'b0,1'b1,1'b1,1'b0,
    1'b0,1'b1,1'b1,1'b0,1'b1,1'b0,1'b0,1'b1,
    1'b1,1'b0,1'b0,1'b1,1'b0,1'b1,1'b1,1'b0,
    1'b1,1'b0,1'b0,1'b1,1'b0,1'b1,1'b1,1'b0,
    1'b0,1'b1,1'b1,1'b0,1'b1,1'b0,1'b0,1'b1,
    1'b1,1'b0,1'b0,1'b1,1'b0,1'b1,1'b1,1'b0,
    1'b0,1'b1,1'b1,1'b0,1'b1,1'b0,1'b0,1'b1,
    1'b0,1'b1,1'b1,1'b0,1'b1,1'b0,1'b0,1'b1,
    1'b1,1'b0,1'b0,1'b1,1'b0,1'b1,1'b1,1'b0,
    1'b1,1'b0,1'b0,1'b1,1'b0,1'b1,1'b1,1'b0,
    1'b0,1'b1,1'b1,1'b0,1'b1,1'b0,1'b0,1'b1,
    1'b0,1'b1,1'b1,1'b0,1'b1,1'b0,1'b0,1'b1,
    1'b1,1'b0,1'b0,1'b1,1'b0,1'b1,1'b1,1'b0,
    1'b0,1'b1,1'b1,1'b0,1'b1,1'b0,1'b0,1'b1,
    1'b1,1'b0,1'b0,1'b1,1'b0,1'b1,1'b1,1'b0,
    1'b1,1'b0,1'b0,1'b1,1'b0,1'b1,1'b1,1'b0,
    1'b0,1'b1,1'b1,1'b0,1'b1,1'b0,1'b0,1'b1
  };
  
  parameter IDLE                        = 4'd0;
  parameter RX_NEG_EDGE                 = 4'd1;
  parameter RX_CLK_LOW                  = 4'd2;
  parameter RX_CLK_HIGH                 = 4'd3;
  parameter TX_FORCE_CLK_LOW            = 4'd4;
  parameter TX_BRING_DATA_LOW           = 4'd5;
  parameter TX_RELEASE_CLK              = 4'd6;
  parameter TX_WAIT_FIRTS_NEG_EDGE      = 4'd7;
  parameter TX_CLK_LOW                  = 4'd8;
  parameter TX_WAIT_POS_EDGE            = 4'd9;
  parameter TX_CLK_HIGH                 = 4'd10;
  parameter TX_WAIT_POS_EDGE_BEFORE_ACK = 4'd11;
  parameter TX_WAIT_ACK                 = 4'd12;
  parameter TX_RECEIVED_ACK             = 4'd13;
  parameter TX_ERROR_NO_ACK             = 4'd14;
  
  
  reg [10:0] frame;
  wire rx_parity;
  
  wire ps2_clk_in, ps2_data_in;
  reg clk_inter, ps2_clk_s, data_inter, ps2_data_s;
  reg [3:0] clk_count, data_count;
  
  reg ps2_clk_en, ps2_clk_en_next, ps2_data_en, ps2_data_en_next;
  reg ps2_clk_out, ps2_clk_out_next, ps2_data_out, ps2_data_out_next;
  reg err_next;
  reg [3:0] state, state_next;
  reg rx_finish;
  
  reg [3:0] bits_count;
  
  reg [13:0] counter, counter_next;
  
  IOBUF IOBUF_inst_0(
    .O(ps2_clk_in),
    .IO(ps2_clk),
    .I(ps2_clk_out),
    .T(~ps2_clk_en)
  );
	
  IOBUF IOBUF_inst_1(
    .O(ps2_data_in),
    .IO(ps2_data),
    .I(ps2_data_out),
    .T(~ps2_data_en)
  );
  //assign ps2_clk = (ps2_clk_en)?ps2_clk_out:1'bz;
  //assign ps2_data = (ps2_data_en)?ps2_data_out:1'bz;
  assign busy = (state==IDLE)?1'b0:1'b1;
  
  always @ (posedge clk, posedge rst)begin
    if(rst)begin
	  rx_data <= 0;
	  rx_valid <= 1'b0;
	end else if(rx_finish==1'b1)begin
	  rx_data <= frame[8:1];
	  rx_valid <= 1'b1;
	end else begin
	  rx_data <= rx_data;
	  rx_valid <= 1'b0;
	end
  end
  
  assign rx_parity = parity_table[frame[8:1]];
  assign tx_parity = parity_table[tx_data];
  
  always @ (posedge clk, posedge rst)begin
    if(rst)
	  frame <= 0;
	else if(tx_valid==1'b1 && state==IDLE) begin
	  frame[0] <= 1'b0;              //start bit
	  frame[8:1] <= tx_data;         //data
	  frame[9] <= tx_parity;         //parity bit
	  frame[10] <= 1'b1;             //stop bit
	end else if(state==RX_NEG_EDGE || state==TX_CLK_LOW)
	  frame <= {ps2_data_s, frame[10:1]};
	else
	  frame <= frame;
  end
    
  // Debouncer
  always @ (posedge clk, posedge rst) begin
    if(rst)begin
	  ps2_clk_s <= 1'b1;
	  clk_inter <= 1'b1;
	  clk_count <= 0;
	end else if(ps2_clk_in != clk_inter)begin
	  ps2_clk_s <= ps2_clk_s;
	  clk_inter <= ps2_clk_in;
	  clk_count <= 0;
	end else if(clk_count == DEBOUNCE_DELAY) begin
	  ps2_clk_s <= clk_inter;
	  clk_inter <= clk_inter;
	  clk_count <= clk_count;
	end else begin
	  ps2_clk_s <= ps2_clk_s;
	  clk_inter <= clk_inter;
	  clk_count <= clk_count + 1'b1;
	end
  end
  
  always @ (posedge clk, posedge rst) begin
    if(rst)begin
	  ps2_data_s <= 1'b1;
	  data_inter <= 1'b1;
	  data_count <= 0;
	end else if(ps2_data_in != data_inter)begin
	  ps2_data_s <= ps2_data_s;
	  data_inter <= ps2_data_in;
	  data_count <= 0;
	end else if(data_count == DEBOUNCE_DELAY) begin
	  ps2_data_s <= data_inter;
	  data_inter <= data_inter;
	  data_count <= data_count;
	end else begin
	  ps2_data_s <= ps2_data_s;
	  data_inter <= data_inter;
	  data_count <= data_count + 1'b1;
	end
  end
  
  // FSM
  always @ (posedge clk, posedge rst)begin
    if(rst)begin
	  state <= IDLE;
	  ps2_clk_en <= 1'b0;
	  ps2_clk_out <= 1'b0;
	  ps2_data_en <= 1'b0;
	  ps2_data_out <= 1'b0;
	  err <= 1'b0;
	  counter <= 0;
	end else begin
	  state <= state_next;
	  ps2_clk_en <= ps2_clk_en_next;
	  ps2_clk_out <= ps2_clk_out_next;
	  ps2_data_en <= ps2_data_en_next;
	  ps2_data_out <= ps2_data_out_next;
	  err <= err_next;
	  counter <= counter_next;
	end
  end
  
  always @ * begin
    state_next = IDLE;                                     // default values for these signals
	ps2_clk_en_next = 1'b0;                                // ensures signals are reset to default value
	ps2_clk_out_next = 1'b1;                               // when conditions for their activation are no
	ps2_data_en_next = 1'b0;                               // longer applied (transition to other state,
	ps2_data_out_next = 1'b1;                              // where signal should not be active)
	err_next = 1'b0;                                       // Idle value for ps2_clk and ps2_data is 'Z'
	rx_finish = 1'b0;
	counter_next = 0;
    case(state)
	  IDLE:begin                                           // wait for the device to begin a transmission
	      if(tx_valid == 1'b1)begin                        // by pulling the clock line low and go to state
		    state_next = TX_FORCE_CLK_LOW;                 // RX_NEG_EDGE or, if write is high, the
	      end else if(ps2_clk_s == 1'b0)begin              // client of this interface wants to send a byte
		    state_next = RX_NEG_EDGE;                      // to the device and a transition is made to state
	      end else begin                                   // TX_FORCE_CLK_LOW
		    state_next = IDLE;
		  end
	    end
		
	  RX_NEG_EDGE:begin                                    // data must be read into frame in this state
	      state_next = RX_CLK_LOW;                         // the ps2_clk just transitioned from high to low
	    end
		
	  RX_CLK_LOW:begin                                     // ps2_clk line is low, wait for it to go high
	      if(ps2_clk_s == 1'b1)begin
		    state_next = RX_CLK_HIGH;
		  end else begin
		    state_next = RX_CLK_LOW;
		  end
	    end
		
	  RX_CLK_HIGH:begin                                    // ps2_clk is high, check if all the bits have been read
	      if(bits_count == BITS_NUM)begin                  // if, last bit read, check parity, and if parity ok
		    if(rx_parity != frame[9])begin                 // load received data into rx_data.
			  err_next = 1'b1;                             // else if more bits left, then wait for the ps2_clk to
			  state_next = IDLE;                           // go low
			end else begin
			  rx_finish = 1'b1;
			  state_next = IDLE;
			end
		  end else if(ps2_clk_s == 1'b0)begin
		    state_next = RX_NEG_EDGE;
	      end else begin
		    state_next = RX_CLK_HIGH;
		  end		  
	    end
		
	  TX_FORCE_CLK_LOW:begin                               // the client wishes to transmit a byte to the device
	      ps2_clk_en_next = 1'b1;                          // this is done by holding ps2_clk down for at least 100us
		  ps2_clk_out_next = 1'b0;                         // bringing down ps2_data, wait 20us and then releasing
		  if(counter == CLOCK_CNT_100US)begin              // the ps2_clk.
		    state_next = TX_BRING_DATA_LOW;                // This constitutes a request to send command.
			counter_next = 0;                              // In this state, the ps2_clk line is held down and
		  end else begin                                   // the counter for waiting 100us is enabled.
		    state_next = TX_FORCE_CLK_LOW;                 // when the counter reached upper limit, transition
			counter_next = counter + 1'b1;                 // to TX_BRING_DATA_LOW
		  end                                              
	    end                              

	  TX_BRING_DATA_LOW:begin                              // with the ps2_clk line low bring ps2_data low
	      ps2_clk_en_next = 1'b1;                          // wait for 20us and then go to TX_RELEASE_CLK
		  ps2_clk_out_next = 1'b0;

		  // set data line low
		  // when clock is released in the next state
		  // the device will read bit 0 on data line
		  // and this bit represents the start bit.
		  ps2_data_en_next = 1'b1;
		  ps2_data_out_next = 1'b0;
	      if(counter == CLOCK_CNT_20US)begin
		    state_next = TX_RELEASE_CLK;
			counter_next = 0;
		  end else begin
		    state_next = TX_BRING_DATA_LOW;
			counter_next = counter + 1'b1;
		  end
	    end
		
      TX_RELEASE_CLK:begin                                 // release the ps2_clk line
	      ps2_clk_en_next = 1'b0;                          // keep holding data line low 
		  ps2_data_en_next = 1'b1;
		  ps2_data_out_next = 1'b0;
		  state_next = TX_WAIT_FIRTS_NEG_EDGE;
	    end
		
	  TX_WAIT_FIRTS_NEG_EDGE:begin                         // state is necessary because the clock signal
	      ps2_data_en_next = 1'b1;                         // is not released instantaneously and, because of debounce, 
		  ps2_data_out_next = 1'b0;                        // delay is even greater. 
		  if(counter == 14'd63)begin                       // Wait 63 clock periods for the clock line to release 
		    if(ps2_clk_s == 1'b0)begin                     // then if clock is low then go to tx_clk_l 
			  state_next = TX_CLK_LOW;                     // else wait until ps2_clk goes low. 
			  counter_next = 0;                            
			end else begin
			  state_next = TX_WAIT_FIRTS_NEG_EDGE;
			  counter_next = counter;
			end
		  end else begin
		    state_next = TX_WAIT_FIRTS_NEG_EDGE;
			counter_next = counter + 1'b1;
		  end
	    end
	  
	  TX_CLK_LOW:begin                                     // place the least significant bit from frame 
	      ps2_data_en_next = 1'b1;                         // on the data line
		  ps2_data_out_next = frame[0];                    // During this state the frame is shifted one
		  state_next = TX_WAIT_POS_EDGE;                   // bit to the right
	    end
	  
	  TX_WAIT_POS_EDGE:begin                               // wait for the clock to go high
	      ps2_data_en_next = 1'b1;                         // this is the edge on which the device reads the data
		  ps2_data_out_next = frame[0];                    // on ps2_data.
		  if(bits_count == BITS_NUM-1)begin                // keep holding ps2_data on frame(0) because else
		    ps2_data_en_next = 1'b0;                       // will be released by default value.
			state_next = TX_WAIT_POS_EDGE_BEFORE_ACK;      // Check if sent the last bit and if so, release data line
		  end else if(ps2_clk_s == 1'b1)begin              // and go to state that wait for acknowledge
		    state_next = TX_CLK_HIGH;
		  end else begin
		    state_next = TX_WAIT_POS_EDGE;
		  end
	    end
	
      TX_CLK_HIGH:begin                                    // ps2_clk is released, wait for down edge
	      ps2_data_en_next = 1'b1;                         // and go to tx_clk_l when arrived
		  ps2_data_out_next = frame[0];
		  if(ps2_clk_s == 1'b0)begin
		    state_next = TX_CLK_LOW;
		  end else begin
		    state_next = TX_CLK_HIGH;
		  end
	    end
	  
	  TX_WAIT_POS_EDGE_BEFORE_ACK:begin                    // release ps2_data and wait for rising edge of ps2_clk
	      if(ps2_clk_s == 1'b1)begin                       // once this occurs, transition to tx_wait_ack
		    state_next = TX_WAIT_ACK;
		  end else begin
		    state_next = TX_WAIT_POS_EDGE_BEFORE_ACK;
		  end
	    end
		
	  TX_WAIT_ACK:begin                                    // wait for the falling edge of the clock line
	      if(ps2_clk_s == 1'b0)begin                       // if data line is low when this occurs, the
		    if(ps2_data_s == 1'b0) begin                   // ack is received
			  state_next = TX_RECEIVED_ACK;                // else if data line is high, the device did not
			end else begin                                 // acknowledge the transimission
			  state_next = TX_ERROR_NO_ACK;
			end
		  end else begin
		    state_next = TX_WAIT_ACK;
		  end
	    end
	  
	  TX_RECEIVED_ACK:begin                                // wait for ps2_clk to be released together with ps2_data
	      if(ps2_clk_s == 1'b1 && ps2_clk_s == 1'b1)begin  // (bus to be idle) and go back to idle state
		    state_next = IDLE;
		  end else begin
		    state_next = TX_RECEIVED_ACK;
		  end
	    end
		
	  TX_ERROR_NO_ACK:begin
	      if(ps2_clk_s == 1'b1 && ps2_clk_s == 1'b1)begin  // wait for ps2_clk to be released together with ps2_data
		    err_next = 1'b1;                               // (bus to be idle) and go back to idle state
			state_next = IDLE;                             // signal error for not receiving ack
		  end else begin
		    state_next = TX_ERROR_NO_ACK;
		  end
	    end
	
	  default:begin                                        // if invalid transition occurred, signal error and
	      err_next = 1'b1;                                 // go back to idle state
		  state_next = IDLE;
	    end
		
    endcase
  end
  
  always @ (posedge clk, posedge rst)begin
    if(rst)
	  bits_count <= 0;
	else if(state==IDLE)
	  bits_count <= 0;
	else if(state==RX_NEG_EDGE || state==TX_CLK_LOW)
	  bits_count <= bits_count + 1'b1;
	else
	  bits_count <= bits_count;
  end
	
endmodule


//VGA
module vga_controller(
    input wire pclk,reset,
    output wire hsync,vsync,valid,
    output wire [9:0]h_cnt,
    output wire [9:0]v_cnt
);
    reg [9:0]pixel_cnt;
    reg [9:0]line_cnt;
    reg hsync_i,vsync_i;
    wire hsync_default, vsync_default;
    wire [9:0] HD, HF, HS, HB, HT, VD, VF, VS, VB, VT;

   
    assign HD = 640;
    assign HF = 16;
    assign HS = 96;
    assign HB = 48;
    assign HT = 800; 
    assign VD = 480;
    assign VF = 10;
    assign VS = 2;
    assign VB = 33;
    assign VT = 525;
    assign hsync_default = 1'b1;
    assign vsync_default = 1'b1;
     
    always@(posedge pclk)
        if(reset)
            pixel_cnt <= 0;
        else if(pixel_cnt < (HT - 1))
                pixel_cnt <= pixel_cnt + 1;
             else
                pixel_cnt <= 0;

    always@(posedge pclk)
        if(reset)
            hsync_i <= hsync_default;
        else if((pixel_cnt >= (HD + HF - 1))&&(pixel_cnt < (HD + HF + HS - 1)))
                hsync_i <= ~hsync_default;
            else
                hsync_i <= hsync_default; 
    
    always@(posedge pclk)
        if(reset)
            line_cnt <= 0;
        else if(pixel_cnt == (HT -1))
                if(line_cnt < (VT - 1))
                    line_cnt <= line_cnt + 1;
                else
                    line_cnt <= 0;
                    
    always@(posedge pclk)
        if(reset)
            vsync_i <= vsync_default; 
        else if((line_cnt >= (VD + VF - 1))&&(line_cnt < (VD + VF + VS - 1)))
            vsync_i <= ~vsync_default; 
        else
            vsync_i <= vsync_default; 
                    
    assign hsync = hsync_i;
    assign vsync = vsync_i;
    assign valid = ((pixel_cnt < HD) && (line_cnt < VD));
    
    assign h_cnt = (pixel_cnt < HD) ? pixel_cnt:10'd0;
    assign v_cnt = (line_cnt < VD) ? line_cnt:10'd0;
           
endmodule


module mem_addr_gen(
   input clk,
   input rst,
   input [9:0] h_cnt,
   input [9:0] v_cnt,
   output [16:0] pixel_addr
   );
  
   assign pixel_addr = ((h_cnt>>1)+320*(v_cnt>>1))% 76800;  //640*480 --> 320*240   
    
endmodule

//Speaker
module Speaker(
	input clk,
	input reset,
	input [2:0] state,
	input [13:0] scores,
	output pmod_1,
	output pmod_2,
	output pmod_4
);
reg [31:0] beat_freq;
parameter BEAT_FREQ_1 = 32'd1;	//one beat=1sec
parameter BEAT_FREQ_2 = 32'd2;	//one beat=0.5sec
parameter BEAT_FREQ_3 = 32'd4;	//one beat=0.25sec
parameter BEAT_FREQ_4 = 32'd8;	//one beat=0.125sec
always @(*) begin 
	if(scores < 14'd20) begin
		beat_freq = BEAT_FREQ_1;
	end else if(scores < 14'd40) begin
		beat_freq = BEAT_FREQ_2;
	end else if(scores < 14'd60) begin
		beat_freq = BEAT_FREQ_3;
	end else begin
		beat_freq = BEAT_FREQ_4;
	end
end


parameter DUTY_BEST = 10'd512;	//duty cycle=50%

wire [31:0] freq;
wire [7:0] ibeatNum;
wire beatFreq;

assign pmod_2 = 1'd1;	//no gain(6dB)
assign pmod_4 = 1'd1;	//turn-on

//Generate beat speed
PWM_gen btSpeedGen ( .clk(clk), 
					 .reset(reset),
					 .freq(beat_freq),
					 .duty(DUTY_BEST), 
					 .PWM(beatFreq)
);
	
//manipulate beat
PlayerCtrl playerCtrl_00 ( .clk(beatFreq),
						   .reset(reset),
						   .state(state),
						   .ibeat(ibeatNum)
);	
	
//Generate variant freq. of tones
Music music00 ( .ibeatNum(ibeatNum),
				.state(state),
				.tone(freq)
);

// Generate particular freq. signal
PWM_gen toneGen ( .clk(clk), 
				  .reset(reset), 
				  .freq(freq),
				  .duty(DUTY_BEST), 
				  .PWM(pmod_1)
);
endmodule

module PWM_gen (
    input wire clk,
    input wire reset,
	input [31:0] freq,
    input [9:0] duty,
    output reg PWM
);

wire [31:0] count_max = 100_000_000 / freq;
wire [31:0] count_duty = count_max * duty / 1024;
reg [31:0] count;
    
always @(posedge clk, posedge reset) begin
    if (reset) begin
        count <= 0;
        PWM <= 0;
    end else if (count < count_max) begin
        count <= count + 1;
		if(count < count_duty)
            PWM <= 1;
        else
            PWM <= 0;
    end else begin
        count <= 0;
        PWM <= 0;
    end
end

endmodule

module PlayerCtrl (
	input clk,
	input reset,
	input [2:0] state,
	output reg [7:0] ibeat
);
parameter BEATLENGTH = 42;

always @(posedge clk, posedge reset) begin
	if (reset)
		ibeat <= 0;
	else begin
		if (state < 3'd4) begin
			if(ibeat < BEATLENGTH) 
				ibeat <= ibeat + 1;
			else 
				ibeat <= 1;
		end else begin
			ibeat <= 0;
		end
	end 
end

endmodule

`define NM0 32'd20000 //slience (over freq.)
`define NM1 32'd262 //C4
`define NM2 32'd294 //D4
`define NM3 32'd330 //E4
`define NM4 32'd349 //F4
`define NM5 32'd392 //G4
`define NM6 32'd440 //A4
`define NM7 32'd494 //B4

module Music (
	input [7:0] ibeatNum,
	input [2:0] state,
	output [31:0] tone
);
reg [31:0] tone1, tone2;
assign tone = (state == 3'd3) ? tone2 : tone1;
always @(*) begin
	case (ibeatNum)		// 1/4 beat
		8'd0 : begin
			tone1 = `NM0;
			tone2 = `NM0;
		end
		8'd1 : begin
			tone1 = `NM1;
			tone2 = `NM5;
		end
		8'd2 : begin
			tone1 = `NM1;
			tone2 = `NM0;
		end
		8'd3 : begin
			tone1 = `NM5;
			tone2 = `NM5;
		end
		8'd4 : begin
			tone1 = `NM5;
			tone2 = `NM0;
		end
		8'd5 : begin
			tone1 = `NM6;
			tone2 = `NM5;
		end
		8'd6 : begin
			tone1 = `NM6;
			tone2 = `NM0;
		end
		8'd7 : begin
			tone1 = `NM5;
			tone2 = `NM3;
		end
		8'd8 : begin
			tone1 = `NM4;
			tone2 = `NM3;
		end
		8'd9 : begin
			tone1 = `NM4;
			tone2 = `NM3;
		end
		8'd10 : begin
			tone1 = `NM3;
			tone2 = `NM3;
		end
		8'd11 : begin
			tone1 = `NM3;
			tone2 = `NM3;
		end
		8'd12 : begin
			tone1 = `NM2;
			tone2 = `NM3;
		end
		8'd13 : begin
			tone1 = `NM2;
			tone2 = `NM3;
		end
		8'd14 : begin
			tone1 = `NM1;
			tone2 = `NM0;
		end
		8'd15 : begin
			tone1 = `NM5;
			tone2 = `NM4;
		end
		8'd16 : begin
			tone1 = `NM5;
			tone2 = `NM0;
		end
		8'd17 : begin
			tone1 = `NM4;
			tone2 = `NM4;
		end
		8'd18 : begin
			tone1 = `NM4;
			tone2 = `NM0;
		end
		8'd19 : begin
			tone1 = `NM3;
			tone2 = `NM4;
		end
		8'd20 : begin
			tone1 = `NM3;
			tone2 = `NM0;
		end
		8'd21 : begin
			tone1 = `NM2;
			tone2 = `NM2;
		end
		8'd22 : begin
			tone1 = `NM5;
			tone2 = `NM2;
		end
		8'd23 : begin
			tone1 = `NM5;
			tone2 = `NM2;
		end
		8'd24 : begin
			tone1 = `NM4;
			tone2 = `NM2;
		end
		8'd25 : begin
			tone1 = `NM4;
			tone2 = `NM2;
		end
		8'd26 : begin
			tone1 = `NM3;
			tone2 = `NM2;
		end
		8'd27 : begin
			tone1 = `NM3;
			tone2 = `NM2;
		end
		8'd28 : begin
			tone1 = `NM2;
			tone2 = `NM0;
		end
		8'd29 : begin
			tone1 = `NM1;
			tone2 = `NM0;
		end
		8'd30 : begin
			tone1 = `NM1;
			tone2 = `NM0;
		end
		8'd31 : begin
			tone1 = `NM5;
			tone2 = `NM0;
		end
		8'd32 : begin
			tone1 = `NM5;
			tone2 = `NM0;
		end
		8'd33 : begin
			tone1 = `NM6;
			tone2 = `NM0;
		end
		8'd34 : begin
			tone1 = `NM6;
			tone2 = `NM0;
		end
		8'd35 : begin
			tone1 = `NM5;
			tone2 = `NM0;
		end
		8'd36 : begin
			tone1 = `NM4;
			tone2 = `NM0;
		end
		8'd37 : begin
			tone1 = `NM4;
			tone2 = `NM0;
		end
		8'd38 : begin
			tone1 = `NM3;
			tone2 = `NM0;
		end
		8'd39 : begin
			tone1 = `NM3;
			tone2 = `NM0;
		end
		8'd40 : begin
			tone1 = `NM2;
			tone2 = `NM0;
		end
		8'd41 : begin
			tone1 = `NM2;
			tone2 = `NM0;
		end
		8'd42 : begin
			tone1 = `NM1;
			tone2 = `NM0;
		end
		default : begin
			tone1 = `NM0;
			tone2 = `NM0;
		end
	endcase
end
endmodule