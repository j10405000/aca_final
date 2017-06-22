package object AcaCustom
{
    import Chisel._
    import Node._
    import Sodor.Constants._
    import Common._

    class DCacheInterface()(implicit conf: SodorConfiguration) extends Module
    {
        val io = new Bundle {
            val core_port = (new MemPortIo(data_width=conf.xprlen)).flip
            val mem_port = new MemPortIo(data_width=conf.xprlen)
        }
    }

    class NoDCache()(implicit conf: SodorConfiguration) extends DCacheInterface
    {
        io.mem_port <> io.core_port
    }

    class NoDCache2()(implicit conf: SodorConfiguration) extends DCacheInterface
    {
        printf("\n\n")
        
        // Extract desired data from burst
        val burst_len = 64    // 64 byte per line
        val burst_len_bit = 6 // 2^6 = 64
        val word_len = 4      // 4 byte per word
        val word_len_bit = 2  // 2^2 = 4

        // Rename io.core.port
        val req_valid      = io.core_port.req.valid
        val req_addr       = io.core_port.req.bits.addr
        val req_data       = io.core_port.req.bits.data
        val req_fcn        = io.core_port.req.bits.fcn
        val req_typ        = io.core_port.req.bits.typ

        val req_addr_reg       = Reg(Bits())
        val req_data_reg       = Reg(Bits()) 
        val req_fcn_reg        = Reg(Bits()) 
        val req_typ_reg        = Reg(Bits()) 

        val w_queue_size       = 16
        val w_queue_addr       = Mem(Bits(width=32), w_queue_size)
        val w_queue_data       = Mem(Bits(width=32), w_queue_size)
        val w_queue_fcn        = Mem(Bits(width=32), w_queue_size)
        val w_queue_typ        = Mem(Bits(width=32), w_queue_size)
        val w_queue_is_full    = Reg(init=Bool(false))
        val w_queue_is_empty   = Reg(init=Bool(true))
        val w_header           = Counter(w_queue_size)
        val w_tail             = Counter(w_queue_size)
        val mem_is_busy        = Reg(init=Bool(false))
        val last_reg           = Reg(init=Bits(0,2)) //init=0 enqueue=1 dequeue=2
        //val init_mem           = Reg(init=Bool(false))

        //w_queue_count.inc()
        printf("w_queue_is_full:%x\nw_queue_is_empty:%x\nw_header:%x\nw_tail :%x\n", 
                w_queue_is_full,w_queue_is_empty,w_header.value,w_tail.value)
        
        when(w_header.value===w_tail.value)
        {
            when(last_reg===Bits(1,2))
            {
                w_queue_is_full := Bool(true)
                last_reg := Bits(0,2)
            }
            when(last_reg===Bits(2,2))
            {
                w_queue_is_empty := Bool(true)
                last_reg := Bits(0,2)
            }
        }
        /*
        //enqueue
        when(!w_queue_is_full_2 && io.core_port.req.valid)
        {
            when(!(w_header.value===w_tail.value) || last_reg===Bits(0,2))
            {
                w_header.inc()

                w_queue_addr(w_header.value) := req_addr
                last_reg := Bits(1,2)
                when(w_queue_is_empty)
                {
                    w_queue_is_empty := Bool(false)
                }
            }
        }
       
        //dequeue
        when(!w_queue_is_empty && io.mem_port.resp.valid)
        {
            when(!(w_header.value===w_tail.value) || last_reg===Bits(0,2))
            {
                w_tail.inc()
                val output = w_queue_addr(w_tail.value)
                printf("output: %x\n", output)
                last_reg := Bits(2,2)
                when(w_queue_is_full_2)
                {
                    w_queue_is_full_2 := Bool(false)
                }
            }
        } */ 

        // Generate read data from memory
        val burst_data     = io.mem_port.resp.bits.burst_data
        val word_idx_in_burst = req_addr(burst_len_bit - 1, word_len_bit)
        val word_data_ori = Mux1H(UIntToOH(word_idx_in_burst, width=(burst_len / word_len)), burst_data)
        val byte_idx_in_word = req_addr(word_len_bit - 1, 0)        
        // Original read data
        //val read_data_ori = LoadDataGen(word_data_ori >> (byte_idx_in_word << 3), io.core_port.req.bits.typ)
        
        /* Debug: print burst data
        when(io.mem_port.resp.valid){
            for(i <- 0 until 16)
                printf("burst data %d: %x\n", UInt(i),burst_data(i))
            
            printf("offset:%x ,read data ori: %x\n", word_idx_in_burst, read_data_ori)
        }
        */
        
        /* Debug: print write mask & write data
        printf("write_mask=%x\nwdata=     %x\n", wmask, write_mask,wdata)
        */

        // Define DCache parameter
        val DCACHE_ENTRIES = 64
        val DCACHE_ENTRIES_BIT = log2Up(DCACHE_ENTRIES)
        val DCACHE_TAG_BIT = conf.xprlen - DCACHE_ENTRIES_BIT - burst_len_bit
        val DCACHE_BITS = 1 + DCACHE_TAG_BIT + burst_len*8
        //            valid bit +   tag      +   data bit

        val tag = req_addr(conf.xprlen-1,conf.xprlen-DCACHE_TAG_BIT)
        val index = req_addr(conf.xprlen-DCACHE_TAG_BIT-1,conf.xprlen-DCACHE_TAG_BIT-DCACHE_ENTRIES_BIT)
        val word_offset = req_addr(burst_len_bit-1,2)
        val byte_offset = req_addr(1,0)

        val dcache = Mem(Bits(width=DCACHE_BITS), DCACHE_ENTRIES)
        val dcache_write_data = UInt(width=DCACHE_BITS)
        val dcache_read_out = dcache(index)
        val dcache_read_burst = Vec.fill(16){Bits(width=conf.xprlen)}
        
        val word_data = Bits() 
        val read_data = Bits()
        
        /* 
        when(!init_mem)
        {
            val dcache_write_data_init = UInt(0)
            for(i <- 0 until DCACHE_ENTRIES)
            {
                dcache.write(UInt(i),dcache_write_data_init)
                printf("index:%x, data:%x\n", UInt(i),dcache(UInt(i)))
            }
            init_mem := Bool(true)
        }
        */

        // Generate read data
        for(k <- 0 until 16)
            dcache_read_burst(k) := dcache_read_out(32*k+31,32*k) 
        word_data := Mux1H(UIntToOH(word_offset, width=(burst_len / word_len)),dcache_read_burst)
        read_data := LoadDataGen(word_data >> (byte_offset << 3), req_typ)
        
        // Generate dcache entry write data 
        dcache_write_data := UInt(0)
        dcache_write_data(DCACHE_BITS-1,DCACHE_BITS-1) := Bits(1,1)
        dcache_write_data(DCACHE_BITS-1-1,DCACHE_BITS-1-DCACHE_TAG_BIT) := tag
        for(k <- 0 until 16) 
            dcache_write_data(32*k+31,32*k) := burst_data(k)

        // Generate dcache write data(one word or one byte in a cache line) and mask
        val wdata = StoreDataGen(req_data, req_typ) << UInt(word_offset<<5)
        val bit_shift_amt  = Cat(byte_offset, UInt(0,3))
        val wmask = (StoreMask(req_typ) << bit_shift_amt)(31,0)
        val write_mask = wmask << UInt(word_offset<<5)

        // Wiring
        io.mem_port.req.valid := Bool(false)
        io.mem_port.req.bits.addr := io.core_port.req.bits.addr
        io.mem_port.req.bits.data := io.core_port.req.bits.data
        io.mem_port.req.bits.fcn  := io.core_port.req.bits.fcn
        io.mem_port.req.bits.typ  := io.core_port.req.bits.typ
        io.core_port.resp.valid     := Bool(false)
        io.core_port.resp.bits.data := read_data       

        when( !w_queue_is_empty && !mem_is_busy )
        {
            //dequeue
            when(!(w_header.value===w_tail.value) || last_reg===Bits(0,2))
            {
                w_tail.inc()
                io.mem_port.req.valid     := Bool(true)
                io.mem_port.req.bits.addr := w_queue_addr(w_tail.value)
                io.mem_port.req.bits.data := w_queue_data(w_tail.value)
                io.mem_port.req.bits.fcn  := w_queue_fcn(w_tail.value)
                io.mem_port.req.bits.typ  := w_queue_typ(w_tail.value)
                
                last_reg := Bits(2,2)
                when(w_queue_is_full)
                {
                    w_queue_is_full := Bool(false)
                }
                mem_is_busy := Bool(true)
            }
        }

        when( mem_is_busy && io.mem_port.resp.valid)
        {
            mem_is_busy := Bool(false)
        }

        /* Debug: w_queue & is_busy signal
        printf("wqueue_is_full: %x, mem_is_busy: %x\n", w_queue_is_full, mem_is_busy)
        printf("req_addr_reg: %x\nreq_data_reg: %x \nreq_fcn_reg : %x \nreq_typ_reg : %x \n"
               , req_addr_reg, req_data_reg, req_fcn_reg , req_typ_reg)
        printf("mem.resp.valid: %x\n", io.mem_port.resp.valid)
        */

        // Define state machine
        val s_idle :: s_load :: Nil = Enum(UInt(),2)
        val state = Reg(init = s_idle)
        switch(state)
        {
            is(s_idle)
            {
                printf("state: s_idle\n")
                when ( io.core_port.req.valid )
                {
                    //read access
                    when ( io.core_port.req.bits.fcn === M_XRD )
                    {
                        // Read hit: valid bit = 1 && tag match
                        when(dcache_read_out(DCACHE_BITS-1,DCACHE_BITS-1) === Bits(1,1) 
                             && dcache_read_out(DCACHE_BITS-1-1,DCACHE_BITS-1-DCACHE_TAG_BIT) === tag)
                        {
                            when( w_queue_is_empty && !mem_is_busy )
                            {
                                printf("debug: read hit\n")
                                io.core_port.resp.valid := Bool(true)
                                state := s_idle
                            }
                        }

                        // Read miss, load memory
                        .otherwise
                        {
                            printf("debug: read miss\n")
                            when( w_queue_is_empty && !mem_is_busy )
                            {
                                io.mem_port.req.valid := Bool(true)
                                mem_is_busy := Bool(true)
                                state := s_load
                            }
                        }
                    }

                    //write access
                    when ( io.core_port.req.bits.fcn === M_XWR )
                    {
                        // Write miss, load memory
                        when(!(dcache_read_out(DCACHE_BITS-1,DCACHE_BITS-1) === Bits(1,1) 
                             && dcache_read_out(DCACHE_BITS-1-1,DCACHE_BITS-1-DCACHE_TAG_BIT) === tag))
                        {
                            printf("debug: write miss\n")
                            when( w_queue_is_empty && !mem_is_busy )
                            {
                                io.mem_port.req.valid := Bool(true)
                                mem_is_busy := Bool(true)
                                state := s_load
                            }
                        }

                        // Write hit
                        .otherwise
                        {
                            printf("debug: write hit\n")
                            //write memory

                            when(!w_queue_is_full)
                            {
                                //enqueue
                                when(!(w_header.value===w_tail.value) || last_reg===Bits(0,2))
                                {   
                                    w_header.inc()

                                    w_queue_addr.write(w_header.value,req_addr)
                                    w_queue_data.write(w_header.value,req_data)  
                                    w_queue_fcn.write(w_header.value,req_fcn)
                                    w_queue_typ.write(w_header.value,req_typ)

                                    last_reg := Bits(1,2)
                                    when(w_queue_is_empty)
                                    {   
                                        w_queue_is_empty := Bool(false)
                                    }   
                                    dcache.write(index, wdata, write_mask)
                                    io.core_port.resp.valid := Bool(true)
                                }
                            }
                        }
                    }
                }
            }
            
            //Read miss, load memory
            is(s_load)
            {
                printf("state: s_load\n")
                when ( io.mem_port.resp.valid )
                {   
                    dcache.write(index, dcache_write_data)
                    //printf("Debug: dcache_write_data: %x\n", dcache_write_data)
                    state := s_idle
                }
            }
        }
    }

    type DCache = NoDCache2

    //appropriately mask and sign-extend data for the core                  
    object LoadDataGen                                                      
    {                                                                       
       def apply(data: Bits, typ: Bits) : Bits =                            
       {                                                                    
          val out = Mux(typ === MT_H,  Cat(Fill(16, data(15)),  data(15,0)),
                    Mux(typ === MT_HU, Cat(Fill(16, UInt(0x0)), data(15,0)),
                    Mux(typ === MT_B,  Cat(Fill(24, data(7)),    data(7,0)),
                    Mux(typ === MT_BU, Cat(Fill(24, UInt(0x0)), data(7,0)), 
                                        data(31,0)))))                      
          return out                                                        
       }                                                                    
    }
}
