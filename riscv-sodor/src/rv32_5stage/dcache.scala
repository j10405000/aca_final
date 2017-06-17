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
        // Extract desired data from burst
        val burst_len = 64    // 64 byte per line
        val burst_len_bit = 6 // 2^6 = 64
        val word_len = 4      // 4 byte per word
        val word_len_bit = 2  // 2^2 = 4

        val req_addr = io.core_port.req.bits.addr
        val burst_data = io.mem_port.resp.bits.burst_data

        val word_idx_in_burst = req_addr(burst_len_bit - 1, word_len_bit)
        val word_data = 
            Mux1H(UIntToOH(word_idx_in_burst, width=(burst_len / word_len)), burst_data)

        val byte_idx_in_word = req_addr(word_len_bit - 1, 0)
        val read_data = LoadDataGen(word_data >> (byte_idx_in_word << 3), io.core_port.req.bits.typ)
        
        
        // Wiring
        io.mem_port.req.valid <> io.core_port.req.valid
        io.mem_port.req.ready <> io.core_port.req.ready
        io.mem_port.req.bits.addr <> io.core_port.req.bits.addr
        io.mem_port.req.bits.data <> io.core_port.req.bits.data
        io.mem_port.req.bits.fcn <> io.core_port.req.bits.fcn
        io.mem_port.req.bits.typ <> io.core_port.req.bits.typ

        io.core_port.resp.valid <> io.mem_port.resp.valid
        io.core_port.resp.bits.data := read_data
        io.core_port.resp.bits.burst_data := Bits(0)

        val DCACHE_ENTRIES = 1024
        val DCACHE_ENTRIES_BIT = 10
        val DCACHE_BITS = 1+(conf.xprlen-DCACHE_ENTRIES_BIT-burst_len_bit)+burst_len*8
        //valid bit + tag + data bit
        
        val dcache = Mem(Bits(width=DCACHE_BITS), DCACHE_ENTRIES)
        val dcache_write_addr = UInt(width=DCACHE_ENTRIES_BIT)
        val dcache_write_en = Bool()
        val dcache_write_data = Unit(width=DCACHE_BITS)
        val dcache_read_addr = UInt(width=DCACHE_ENTRIES_BIT)
        dcache_write_addr = UInt(0)
        dcache_write_en = Bool(false)
        dcache_write_data = Unit(0)
        dcache_read_addr = UInt(0)
        when(dcache_write_en){
            dcache(dcache_write_addr) := dcache_write_data
        }
        val dcache_read_out = dcache(dcache_read_addr)
        val btb_read_data = Bits(width=DCACHE_BITS)
        
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
