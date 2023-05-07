// license:BSD-3-Clause
// copyright-holders:Luca Elia, AJR
/***************************************************************************

    TMP68301 basic emulation + Interrupt Handling

    The Toshiba TMP68301 is a 68HC000 + serial I/O, parallel I/O,
    3 timers, address decoder, wait generator, interrupt controller,
    all integrated in a single chip.

    TODO:
    - Interrupt generation: edge detection, input expansion (INT3-INT9)
    - Parallel port: handle timing latency
    - Serial port: not done at all
    - (and many other things)

***************************************************************************/

#include "emu.h"
#include "machine/tmp68301.h"

DEFINE_DEVICE_TYPE(TMP68301, tmp68301_device, "tmp68301", "Toshiba TMP68301")

void tmp68301_device::tmp68301_regs(address_map &map)
{
	map(0x000, 0x3ff).rw(FUNC(tmp68301_device::regs_r), FUNC(tmp68301_device::regs_w));

	map(0x080, 0x093).rw(FUNC(tmp68301_device::icr_r), FUNC(tmp68301_device::icr_w)).umask16(0x00ff);

	map(0x094, 0x095).rw(FUNC(tmp68301_device::imr_r), FUNC(tmp68301_device::imr_w));
	map(0x096, 0x097).rw(FUNC(tmp68301_device::ipr_r), FUNC(tmp68301_device::ipr_w));
	map(0x098, 0x099).rw(FUNC(tmp68301_device::iisr_r), FUNC(tmp68301_device::iisr_w));

	/* Parallel Port */
	map(0x100, 0x101).rw(FUNC(tmp68301_device::pdir_r), FUNC(tmp68301_device::pdir_w));
	map(0x10a, 0x10b).rw(FUNC(tmp68301_device::pdr_r), FUNC(tmp68301_device::pdr_w));

	/* Serial Port */
	map(0x18e, 0x18f).rw(FUNC(tmp68301_device::scr_r), FUNC(tmp68301_device::scr_w));
}

// IRQ Mask register
uint16_t tmp68301_device::imr_r()
{
	return m_imr;
}

void tmp68301_device::imr_w(offs_t offset, uint16_t data, uint16_t mem_mask)
{
	COMBINE_DATA(&m_imr);
	update_ipl();
}

// IRQ Pending Register
uint16_t tmp68301_device::ipr_r()
{
	return m_ipr;
}

void tmp68301_device::ipr_w(offs_t offset, uint16_t data, uint16_t mem_mask)
{
	// software writes only clear bits
	m_ipr &= data | ~mem_mask;
	update_ipl();
}

// IRQ In-Service Register
uint16_t tmp68301_device::iisr_r()
{
	return m_iisr;
}

void tmp68301_device::iisr_w(offs_t offset, uint16_t data, uint16_t mem_mask)
{
	// software writes only clear bits
	m_iisr &= data | ~mem_mask;
}

// Serial Control Register (TODO: 8-bit wide)
uint16_t tmp68301_device::scr_r()
{
	return m_scr;
}

void tmp68301_device::scr_w(offs_t offset, uint16_t data, uint16_t mem_mask)
{
	/*
	    *--- ---- CKSE
	    --*- ---- RES
	    ---- ---* INTM
	*/

	COMBINE_DATA(&m_scr);
	m_scr &= 0xa1;
}

/* Parallel direction: 1 = output, 0 = input */
uint16_t tmp68301_device::pdir_r()
{
	return m_pdir;
}

void tmp68301_device::pdir_w(offs_t offset, uint16_t data, uint16_t mem_mask)
{
	COMBINE_DATA(&m_pdir);
}

uint16_t tmp68301_device::pdr_r()
{
	return (m_in_parallel_cb(0) & ~m_pdir) | (m_pdr & m_pdir);
}

void tmp68301_device::pdr_w(offs_t offset, uint16_t data, uint16_t mem_mask)
{
	uint16_t old = m_pdr;
	COMBINE_DATA(&m_pdr);
	m_pdr = (old & ~m_pdir) | (m_pdr & m_pdir);
	m_out_parallel_cb(0, m_pdr, mem_mask);
}

uint8_t tmp68301_device::icr_r(offs_t offset)
{
	return m_icr[offset];
}

void tmp68301_device::icr_w(offs_t offset, uint8_t data)
{
/*
    --x- ---- Vector number is autogenerated if 1, else use external source
    ---x x--- 00 falling edge, 01 low level, 10 rising edge, 11 high level
    ^ applies only to external irqs (offset 0 to 2)
    ---- -xxx irq level
*/
	m_icr[offset] = data;
}

tmp68301_device::tmp68301_device(const machine_config &mconfig, const char *tag, device_t *owner, uint32_t clock)
	: m68000_device(mconfig, TMP68301, tag, owner, clock),
		m_in_parallel_cb(*this),
		m_out_parallel_cb(*this),
		m_ipl(0),
		m_imr(0),
		m_ipr(0),
		m_iisr(0),
		m_scr(0),
		m_pdir(0),
		m_pdr(0)
{
	memset(m_regs, 0, sizeof(m_regs));
	memset(m_icr, 0, sizeof(m_icr));
	m_cpu_space_config.m_internal_map = address_map_constructor(FUNC(tmp68301_device::internal_vectors_r), this);
}


//-------------------------------------------------
//  device_start - device-specific startup
//-------------------------------------------------

void tmp68301_device::device_start()
{
	m68000_device::device_start();

	for (int i = 0; i < 3; i++)
		m_tmp68301_timer[i] = timer_alloc(FUNC(tmp68301_device::timer_callback), this);

	m_in_parallel_cb.resolve_safe(0);
	m_out_parallel_cb.resolve_safe();

	m_program->install_device(0xfffc00, 0xffffff, *this, &tmp68301_device::tmp68301_regs);

	save_item(NAME(m_regs));
	save_item(NAME(m_icr));
	save_item(NAME(m_ipl));
	save_item(NAME(m_imr));
	save_item(NAME(m_ipr));
	save_item(NAME(m_iisr));
	save_item(NAME(m_scr));
	save_item(NAME(m_pdir));
	save_item(NAME(m_pdr));
}

//-------------------------------------------------
//  device_reset - device-specific reset
//-------------------------------------------------

void tmp68301_device::device_reset()
{
	m68000_device::device_reset();

	m_ipr = 0;
	m_iisr = 0;
	m_imr = 0x7f7; // mask all irqs
	std::fill(std::begin(m_icr), std::end(m_icr), 0x07);
	update_ipl();
}

//**************************************************************************
//  INLINE HELPERS
//**************************************************************************

void tmp68301_device::internal_vectors_r(address_map &map)
{
	map(0xfffff0, 0xffffff).r(FUNC(tmp68301_device::irq_callback)).umask16(0x00ff);
}


uint8_t tmp68301_device::irq_callback(offs_t offset)
{
	uint8_t IVNR = m_regs[0x9a/2] & 0xe0;      // Interrupt Vector Number Register (IVNR)

	for (int src : { 0, 7, 3, 1, 8, 4, 5, 9, 2 })
	{
		// check if the IPL matches
		if (offset == (m_icr[src] & 0x07))
		{
			// check if interrupt is pending and not masked out
			u16 mask = (src > 2 ? 2 : 1) << src;
			if ((m_ipr & mask) != 0 && (m_imr & mask) == 0)
			{
				if (!machine().side_effects_disabled())
				{
					// add cause to interrupt in-service register
					m_iisr |= mask;

					// no longer pending
					m_ipr &= ~mask;
					update_ipl();
				}

				// vary vector number by type
				if (src > 6)
					return IVNR | (src - 3);
				else if (src > 2)
					return IVNR | (src - 1) << 2 | serial_interrupt_cause(src - 3);
				else /*if (BIT(m_icr[src], 5))*/ // TODO: use external vector otherwise
					return IVNR | src;
			}
		}
	}

	// default vector
	return IVNR | 0x1f;
}

TIMER_CALLBACK_MEMBER(tmp68301_device::timer_callback)
{
	int i = param;
	uint16_t TCR  =   m_regs[(0x200 + i * 0x20)/2];

//  logerror("s: callback timer %04X, j = %d\n",machine().describe_context(),i,tcount);

	if (TCR & 0x0004)   // INT
	{
		m_ipr |= 0x100 << i;
		update_ipl();
	}

	if (TCR & 0x0080)   // N/1
	{
		// Repeat
		update_timer(i);
	}
	else
	{
		// One Shot
	}
}

void tmp68301_device::update_timer(int i)
{
	uint16_t TCR  =   m_regs[(0x200 + i * 0x20)/2];
	uint16_t MAX1 =   m_regs[(0x204 + i * 0x20)/2];
	uint16_t MAX2 =   m_regs[(0x206 + i * 0x20)/2];

	int max = 0;
	attotime duration = attotime::zero;

	m_tmp68301_timer[i]->adjust(attotime::never,i);

	// timers 1&2 only
	switch( (TCR & 0x0030)>>4 )                     // MR2..1
	{
	case 1:
		max = MAX1;
		break;
	case 2:
		max = MAX2;
		break;
	}

	switch ( (TCR & 0xc000)>>14 )                   // CK2..1
	{
	case 0: // System clock (CLK)
		if (max)
		{
			int scale = (TCR & 0x3c00)>>10;         // P4..1
			if (scale > 8) scale = 8;
			duration = attotime::from_hz(unscaled_clock()) * ((1 << scale) * max);
		}
		break;
	}

//  logerror("%s: TMP68301 Timer %d, duration %lf, max %04X\n",machine().describe_context(),i,duration,max);

	if (!(TCR & 0x0002))                // CS
	{
		if (duration != attotime::zero)
			m_tmp68301_timer[i]->adjust(duration,i);
		else
			logerror("%s: TMP68301 error, timer %d duration is 0\n",machine().describe_context(),i);
	}
}

/* Update the IRQ state based on all possible causes */
void tmp68301_device::update_ipl()
{
	uint8_t new_ipl = 0;

	for (int src = 0; src < 10; src++)
	{
		u16 mask = (src > 2 ? 2 : 1) << src;
		if ((m_ipr & mask) != 0 && (m_imr & mask) == 0 && new_ipl < (m_icr[src] & 0x07))
			new_ipl = m_icr[src] & 0x07;
	}

	if (new_ipl != m_ipl)
	{
		if (m_ipl != 0)
			set_input_line(m_ipl, CLEAR_LINE);
		if (new_ipl != 0)
			set_input_line(new_ipl, ASSERT_LINE);

		m_ipl = new_ipl;
	}
}

uint8_t tmp68301_device::serial_interrupt_cause(int channel)
{
	/*
	 *  00 error occurred
	 *  01 receive completed
	 *  10 transmit ready
	 *  11 interrupt cause cleared while interrupt pending
	 */
	(void)channel;
	return 3;
}


uint16_t tmp68301_device::regs_r(offs_t offset)
{
	return m_regs[offset];
}

void tmp68301_device::regs_w(offs_t offset, uint16_t data, uint16_t mem_mask)
{
	COMBINE_DATA(&m_regs[offset]);

	if (!ACCESSING_BITS_0_7)    return;

//  logerror("CPU #0 PC %06X: TMP68301 Reg %04X<-%04X & %04X\n", >pc(),offset*2,data,mem_mask^0xffff);

	switch( offset * 2 )
	{
		// Timers
		case 0x200:
		case 0x220:
		case 0x240:
		{
			int i = ((offset*2) >> 5) & 3;

			update_timer( i );
		}
		break;
	}
}

void tmp68301_device::external_interrupt_0()    { m_ipr |= EXT_IRQ0; update_ipl(); }
void tmp68301_device::external_interrupt_1()    { m_ipr |= EXT_IRQ1; update_ipl(); }
void tmp68301_device::external_interrupt_2()    { m_ipr |= EXT_IRQ2; update_ipl(); }
