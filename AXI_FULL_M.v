module axi_test_v1_0_M00_AXI #(
		parameter  C_M_TARGET_SLAVE_BASE_ADDR	= 32'h40000000,
		parameter integer C_M_AXI_BURST_LEN	= 16,
		parameter integer C_M_AXI_ID_WIDTH	= 1,
		parameter integer C_M_AXI_ADDR_WIDTH	= 32,
		parameter integer C_M_AXI_DATA_WIDTH	= 32,
		parameter integer C_M_AXI_AWUSER_WIDTH	= 0,
		parameter integer C_M_AXI_ARUSER_WIDTH	= 0,
		parameter integer C_M_AXI_WUSER_WIDTH	= 0,
		parameter integer C_M_AXI_RUSER_WIDTH	= 0,
		parameter integer C_M_AXI_BUSER_WIDTH	= 0
)(
        input wire                                  M_AXI_ACLK          ,
		input wire                                  M_AXI_ARESETN       ,  
        //写地址通道
		output wire [C_M_AXI_ID_WIDTH-1 : 0]        M_AXI_AWID          ,
		output wire [C_M_AXI_ADDR_WIDTH-1 : 0]      M_AXI_AWADDR        ,
		output wire [7 : 0]                         M_AXI_AWLEN         ,
		output wire [2 : 0]                         M_AXI_AWSIZE        ,
		output wire [1 : 0]                         M_AXI_AWBURST       ,
		output wire                                 M_AXI_AWLOCK        ,
		output wire [3 : 0]                         M_AXI_AWCACHE       ,
		output wire [2 : 0]                         M_AXI_AWPROT        ,
		output wire [3 : 0]                         M_AXI_AWQOS         ,
		output wire [C_M_AXI_AWUSER_WIDTH-1 : 0]    M_AXI_AWUSER        ,
		output wire                                 M_AXI_AWVALID       ,
		input wire                                  M_AXI_AWREADY       ,
        //写数据通道
		output wire [C_M_AXI_DATA_WIDTH-1 : 0]      M_AXI_WDATA         ,
		output wire [C_M_AXI_DATA_WIDTH/8-1 : 0]    M_AXI_WSTRB         ,
		output wire                                 M_AXI_WLAST         ,
		output wire [C_M_AXI_WUSER_WIDTH-1 : 0]     M_AXI_WUSER         ,
		output wire                                 M_AXI_WVALID        ,
		input wire                                  M_AXI_WREADY        ,
        //写响应通道
		input wire [C_M_AXI_ID_WIDTH-1 : 0]         M_AXI_BID           ,
		input wire [1 : 0]                          M_AXI_BRESP         ,
		input wire [C_M_AXI_BUSER_WIDTH-1 : 0]      M_AXI_BUSER         ,
		input wire                                  M_AXI_BVALID        ,
		output wire                                 M_AXI_BREADY        ,
        //读地址通道
		output wire [C_M_AXI_ID_WIDTH-1 : 0]        M_AXI_ARID          ,
		output wire [C_M_AXI_ADDR_WIDTH-1 : 0]      M_AXI_ARADDR        ,
		output wire [7 : 0]                         M_AXI_ARLEN         ,
		output wire [2 : 0]                         M_AXI_ARSIZE        ,
		output wire [1 : 0]                         M_AXI_ARBURST       ,
		output wire [3 : 0]                         M_AXI_ARCACHE       ,
		output wire [2 : 0]                         M_AXI_ARPROT        ,
		output wire [3 : 0]                         M_AXI_ARQOS         ,
		output wire [C_M_AXI_ARUSER_WIDTH-1 : 0]    M_AXI_ARUSER        ,
		output wire                                 M_AXI_ARVALID       ,
		input wire                                  M_AXI_ARREADY       ,
        //读数据通道
		input wire [C_M_AXI_ID_WIDTH-1 : 0]         M_AXI_RID           ,
		input wire [C_M_AXI_DATA_WIDTH-1 : 0]       M_AXI_RDATA         ,
		input wire [1 : 0]                          M_AXI_RRESP         ,
		input wire                                  M_AXI_RLAST         ,
		input wire [C_M_AXI_RUSER_WIDTH-1 : 0]      M_AXI_RUSER         ,
		input wire                                  M_AXI_RVALID        ,
		output wire                                 M_AXI_RREADY        

);


//***************************************
//计算二进制数的位宽
function integer clogb2(input integer number);
begin
        for(clogb2 = 0;number>0; clogb2=clogb2+1 )
                number = number>>1 ;
end
endfunction
/**********************参数***********************/
localparam  IDLE = 6'b000_0001,
            WRITE_ADDR   = 6'b000_0010,
            WRITE_DATA   = 6'b000_0100,
            WRITE_RESP   = 6'b000_1000,
            READ_ARRD    = 6'b001_0000,
            READ_DATA    = 6'b010_0000;            
/*********************寄存器**********************/
reg [C_M_AXI_ADDR_WIDTH -1 :0]   m_aw_addr      ;
reg                              m_aw_vaild     ;
reg [C_M_AXI_DATA_WIDTH -1 :0]   m_w_data       ;
reg                              m_w_last       ;
reg                              m_w_valid      ;
reg [C_M_AXI_ADDR_WIDTH -1 :0]   m_ar_addr      ;
reg                              m_ar_vaild     ; 
reg                              m_r_ready      ;     
reg [5:0]                        state          ;    
reg                              write_begin    ;  
reg                              read_begin     ;    
reg [7:0]                        cnt_write_begin;    
reg [clogb2(C_M_AXI_BURST_LEN-1):0]    burst_cnt;                    
/*********************网表型**********************/

/*********************状态机**********************/

always@(posedge M_AXI_ACLK or negedge M_AXI_ARESETN) begin
    if(!M_AXI_ARESETN)                  state <= IDLE;
    else begin
        case(state)
            IDLE    :       if(write_begin)     state <= WRITE_ADDR;
                            else if(read_begin) state <= READ_ARRD;
                            else                state <= IDLE;
            WRITE_ADDR :    if(M_AXI_AWVALID && M_AXI_AWREADY)
                                                state <= WRITE_DATA;
                            else                state <= state;
            WRITE_DATA :    if(M_AXI_WLAST)     state <= WRITE_RESP;
                            else                state <= state;
            WRITE_RESP :    if(M_AXI_BVALID && M_AXI_BREADY)
                                                state <= IDLE;
                            else                state <= state;
            READ_ARRD  :    if(M_AXI_ARVALID && M_AXI_ARREADY)
                                                state <= READ_DATA;
                            else                state <= state;
            READ_DATA  :    if(M_AXI_RLAST)     state <= IDLE;
                            else                state <= state;

        default : state <= state;
        endcase
    end
end


/*********************组合逻辑********************/
assign M_AXI_AWID    = 'd0                              ;
assign M_AXI_AWLEN   = C_M_AXI_BURST_LEN -1             ;
assign M_AXI_AWSIZE  = clogb2(C_M_AXI_DATA_WIDTH/8 - 1) ;
assign M_AXI_AWBURST = 2'b01                            ;           //自增模式
assign M_AXI_AWLOCK  = 'd0                              ;
assign M_AXI_AWCACHE = 4'b0010                          ;
assign M_AXI_AWPROT  = 'd0                              ;
assign M_AXI_AWQOS   = 'd0                              ;
assign M_AXI_AWUSER  = 'd0                              ;
assign M_AXI_AWADDR  = m_aw_addr + C_M_TARGET_SLAVE_BASE_ADDR                       ;
assign M_AXI_AWVALID = m_aw_vaild                       ;

assign M_AXI_WDATA   = m_w_data                         ;
assign M_AXI_WSTRB   = {(C_M_AXI_DATA_WIDTH/8){1'b1}}   ;           //生成一个位宽为C_M_AXI_DATA_WIDTH/8的全1信号         
assign M_AXI_WLAST   = m_w_last                         ;
assign M_AXI_WUSER   = 'd0                              ;
assign M_AXI_WVALID  = m_w_valid                        ;

assign M_AXI_BREADY  = 1'b1                             ;

assign M_AXI_ARID    = 'd0                              ;
assign M_AXI_ARADDR  = m_ar_addr + C_M_TARGET_SLAVE_BASE_ADDR                       ;
assign M_AXI_ARLEN   = C_M_AXI_BURST_LEN -1             ;
assign M_AXI_ARSIZE  = clogb2(C_M_AXI_DATA_WIDTH/8 - 1) ;
assign M_AXI_ARBURST = 2'b01                            ;           //自增模式
assign M_AXI_ARCACHE = 4'b0010                          ;
assign M_AXI_ARPROT  = 'd0                              ;
assign M_AXI_ARQOS   = 'd0                              ;
assign M_AXI_ARUSER  = 'd0                              ;
assign M_AXI_ARVALID = m_ar_vaild                       ;

assign M_AXI_RREADY  = m_r_ready                        ;
/*********************例化************************/

/*********************进程************************/
//延迟计数器，用于产生写开始信号
always@(posedge M_AXI_ACLK or negedge M_AXI_ARESETN) begin
    if(!M_AXI_ARESETN)                        cnt_write_begin <= 'd0;
    else if (cnt_write_begin < 8'd255)        cnt_write_begin <= cnt_write_begin + 1'b1;
    else                                      cnt_write_begin <= cnt_write_begin;
end

always@(posedge M_AXI_ACLK or negedge M_AXI_ARESETN) begin
    if(!M_AXI_ARESETN)                        write_begin <= 1'b0;
    else if (cnt_write_begin == 8'd254)       write_begin <= 1'b1;
    else                                      write_begin <= 1'b0;
end

always@(posedge M_AXI_ACLK or negedge M_AXI_ARESETN) begin
    if(!M_AXI_ARESETN)                         m_aw_vaild <= 1'b0;
    else if (M_AXI_AWVALID && M_AXI_AWREADY)   m_aw_vaild <= 1'b0;
    else if (state == WRITE_ADDR)              m_aw_vaild <= 1'b1;
    else                                       m_aw_vaild <= m_aw_vaild;
end

always@(posedge M_AXI_ACLK or negedge M_AXI_ARESETN) begin
    if(!M_AXI_ARESETN)                          m_aw_addr <= 'd0;
    else if (state == WRITE_ADDR)               m_aw_addr <= 'd100;
    else                                        m_aw_addr <= m_aw_addr;
end

always@(posedge M_AXI_ACLK or negedge M_AXI_ARESETN) begin
    if(!M_AXI_ARESETN)                          m_w_valid <= 1'b0;
    else if (state == WRITE_DATA)               m_w_valid <= 1'b1;
    else                                        m_w_valid <= 1'b0;
end

always@(posedge M_AXI_ACLK or negedge M_AXI_ARESETN) begin
    if(!M_AXI_ARESETN)                          m_w_data <= 'd0;
    else if (M_AXI_WVALID&&M_AXI_WREADY&&(burst_cnt<='d14))
                                                m_w_data <= m_w_data + 1'b1;
    else                                        m_w_data <= 'd0;
end
//计数发送的数据个数，用于产生last信号
always@(posedge M_AXI_ACLK or negedge M_AXI_ARESETN) begin
    if(!M_AXI_ARESETN)                          burst_cnt <= 'd0;
    else if (M_AXI_WVALID&&M_AXI_WREADY)        burst_cnt <= burst_cnt + 1'b1;
    else                                        burst_cnt <= 'd0;
end
always@(posedge M_AXI_ACLK or negedge M_AXI_ARESETN) begin
    if(!M_AXI_ARESETN)                          m_w_last  <= 1'b0;
    else if (burst_cnt=='d14)                   m_w_last  <= 1'b1;
    else                                        m_w_last  <= 1'b0;
end

always@(posedge M_AXI_ACLK or negedge M_AXI_ARESETN) begin
    if(!M_AXI_ARESETN)                          read_begin <= 1'b0;
    else if (M_AXI_BVALID && M_AXI_BREADY)      read_begin <= 1'b1;
    else                                        read_begin <= 1'b0;
end

always@(posedge M_AXI_ACLK or negedge M_AXI_ARESETN) begin
    if(!M_AXI_ARESETN)                          m_ar_addr  <=  'd0;
    else if (state==READ_ARRD)                  m_ar_addr  <=  'd100;
    else                                        m_ar_addr  <=  'd0;
end

always@(posedge M_AXI_ACLK or negedge M_AXI_ARESETN) begin
    if(!M_AXI_ARESETN)                          m_ar_vaild <= 1'b0;
    else if (state == READ_ARRD)                m_ar_vaild <= 1'b1;
    else                                        m_ar_vaild <= 1'b0;
end

always@(posedge M_AXI_ACLK or negedge M_AXI_ARESETN) begin
    if(!M_AXI_ARESETN)                          m_r_ready <= 1'b0;
    else if (M_AXI_ARVALID && M_AXI_ARREADY)    m_r_ready <= 1'b1;
    else if (M_AXI_RLAST)                       m_r_ready <= 1'b0;
    else                                        m_r_ready <= m_r_ready;
end
endmodule