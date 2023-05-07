
/***************************************************************************
CPS2 Turbo
***************************************************************************/

#include "emu.h"

#include "cps2turbo.h" //copy of cps1.h defined in Mame 0247
#include "cps2comm.h"
#include "cps2crypt.h"

#include "cpu/m68000/m68000.h"
#include "cpu/z80/z80.h"
#include "machine/eepromser.h"
//#include "sound/okim6295.h"
#include "sound/qsound.h"

#include "speaker.h"

#include "sound/samples.h" //Zero800

const char *const samples_id_l[] = //zero800
{
	"*lsample",
	"empty", // 00
	"L01", // 01
	"L02", // 02
	"L03", // 03
	"L04", // 04
	"L05", // 05
	"L06", // 06
	"L07", // 07
	"L08", // 08
	"L09", // 09
	"L0A", // 10
	"L0B", // 11
	"L0C", // 12
	"L0D", // 13
	"L0E", // 14
	"L0F", // 15
	"L10", // 16
	"L11", // 17
	"L12", // 18
	"L13", // 19
	"L14", // 20
	"L15", // 21
	"L16", // 22
	"L17", // 23
	"L18", // 24
	"L19", // 25
	"L1A", // 26
	"L1B", // 27
	"L1C", // 28
	"L1D", // 29
	"L1E", // 30
	"L1F", // 31
	"L20", // 32
	"L21", // 33
	"L22", // 34
	"L23", // 35
	"L24", // 36
	"L25", // 37
	"L26", // 38
	"L27", // 39
	"L28", // 40
	"L29", // 41
	"L2A", // 42
	"L2B", // 43
	"L2C", // 44
	"L2D", // 45
	"L2E", // 46
	"L2F", // 47
	"L30", // 48
	"L31", // 49
	"L32", // 50
	"L33", // 51
	"L34", // 52
	"L35", // 53
	"L36", // 54
	"L37", // 55
	"L38", // 56
	"L39", // 57
	"L3A", // 58
	"L3B", // 59
	"L3C", // 60
	"L3D", // 61
	"L3E", // 62
	"L3F", // 63
	0
};	


const char *const samples_id_r[] = //zero800
{
	"*rsamples",
	"empty", // 00
	"R01", // 01
	"R02", // 02
	"R03", // 03
	"R04", // 04
	"R05", // 05
	"R06", // 06
	"R07", // 07
	"R08", // 08
	"R09", // 09
	"R0A", // 10
	"R0B", // 11
	"R0C", // 12
	"R0D", // 13
	"R0E", // 14
	"R0F", // 15
	"R10", // 16
	"R11", // 17
	"R12", // 18
	"R13", // 19
	"R14", // 20
	"R15", // 21
	"R16", // 22
	"R17", // 23
	"R18", // 24
	"R19", // 25
	"R1A", // 26
	"R1B", // 27
	"R1C", // 28
	"R1D", // 29
	"R1E", // 30
	"R1F", // 31
	"R20", // 32
	"R21", // 33
	"R22", // 34
	"R23", // 35
	"R24", // 36
	"R25", // 37
	"R26", // 38
	"R27", // 39
	"R28", // 40
	"R29", // 41
	"R2A", // 42
	"R2B", // 43
	"R2C", // 44
	"R2D", // 45
	"R2E", // 46
	"R2F", // 47
	"R30", // 48
	"R31", // 49
	"R32", // 50
	"R33", // 51
	"R34", // 52
	"R35", // 53
	"R36", // 54
	"R37", // 55
	"R38", // 56
	"R39", // 57
	"R3A", // 58
	"R3B", // 59
	"R3C", // 60
	"R3D", // 61
	"R3E", // 62
	"R3F", // 63
	0
};	


namespace {

/*************************************
 *
 *  Constants
 *
 *************************************/

// Maximum size of QSound Z80 region
#define QSOUND_SIZE 0x50000
#define QSOUND_TURBO_SIZE 0x80000 //Zero800

// Maximum 68000 code size
#undef  CODE_SIZE
#define CODE_SIZE   0x0400000


class cps2_state : public cps_state
{
public:
	cps2_state(const machine_config &mconfig, device_type type, const char *tag)
		: cps_state(mconfig, type, tag, 2)
		, m_samples_l(*this, "samples_l")   //Zero800
		, m_samples_r(*this, "samples_r")	//Zero800	
		, m_decrypted_opcodes(*this, "decrypted_opcodes")
		, m_region_key(*this, "key")
		, m_qsound(*this, "qsound")
		, m_comm(*this, "comm")
		, m_objram1(*this, "objram1")
		, m_objram2(*this, "objram2")
		, m_output(*this, "output")
		, m_io_in0(*this, "IN0")
		, m_io_in1(*this, "IN1")
		, m_cps2_dial_type(0)
	{ }

	void cps2turbo(machine_config &config); //Zero800	
	
	void init_cps2();
	void init_cps2nc();

private:
	void init_digital_volume();
	void cps2_eeprom_port_w(offs_t offset, uint16_t data, uint16_t mem_mask = ~0);
	uint16_t cps2_qsound_volume_r();
	void qsound_sharedram1_samples_w(offs_t offset, uint16_t data, uint16_t mem_mask = ~0); //Zero800	allows you to write and signal when a new song is played
	uint16_t kludge_r();
	uint16_t joy_or_paddle_r();
	TIMER_DEVICE_CALLBACK_MEMBER(cps2_interrupt);
	TIMER_CALLBACK_MEMBER(cps2_update_digital_volume);

	void cps2_objram_bank_w(offs_t offset, uint16_t data, uint16_t mem_mask = ~0);
	[[maybe_unused]] uint16_t cps2_objram1_r(offs_t offset);
	uint16_t cps2_objram2_r(offs_t offset);
	void cps2_objram1_w(offs_t offset, uint16_t data, uint16_t mem_mask = ~0);
	void cps2_objram2_w(offs_t offset, uint16_t data, uint16_t mem_mask = ~0);

	void unshuffle(uint64_t *buf, int len);
	void cps2_gfx_decode();
	virtual void find_last_sprite() override;
	void cps2_render_sprites(screen_device &screen, bitmap_ind16 &bitmap, const rectangle &cliprect, int *primasks);
	void cps2_set_sprite_priorities();
	void cps2_objram_latch();
	uint16_t *cps2_objbase();
	virtual void render_layers(screen_device &screen, bitmap_ind16 &bitmap, const rectangle &cliprect) override;
	uint32_t screen_update_cps2(screen_device &screen, bitmap_ind16 &bitmap, const rectangle &cliprect);

	DECLARE_MACHINE_START(cps2);
	virtual void video_start() override;

	void cps2turbo_map(address_map &map); //Zero800
	void decrypted_cps2turbo(address_map &map); //Zero800	

	void init_cps2_video();
	void init_cps2crypt();

	optional_device<samples_device> m_samples_l; //Zero800
	optional_device<samples_device> m_samples_r; //Zero800	

	optional_shared_ptr<uint16_t> m_decrypted_opcodes;
	optional_memory_region m_region_key;

	optional_device<qsound_device> m_qsound;
	optional_device<cps2_comm_device> m_comm;

	required_shared_ptr<uint16_t> m_objram1;
	required_shared_ptr<uint16_t> m_objram2;
	required_shared_ptr<uint16_t> m_output;

	optional_ioport m_io_in0;
	optional_ioport m_io_in1;

	std::unique_ptr<uint16_t[]> m_cps2_buffered_obj;

	/* video-related */
	int          m_cps2_last_sprite_offset = 0; /* Offset of the last sprite */
	int          m_pri_ctrl = 0;                /* Sprite layer priorities */
	int          m_objram_bank = 0;
	int          m_cps2_obj_size = 0;

	/* misc */
	int          m_readpaddle = 0;  // pzloop2
	int          m_cps2digitalvolumelevel = 0;
	int          m_cps2disabledigitalvolume = 0;
	emu_timer    *m_digital_volume_timer = nullptr;
	int          m_cps2_dial_type = 0;
};


/*************************************
 *
 *  Video
 *
 *************************************/

#define CPS2_OBJ_BASE   0x00    // Unknown (not base address of objects). Could be bass address of bank used when object swap bit set?
#define CPS2_OBJ_UK1    0x02    // Unknown (nearly always 0x807d, or 0x808e when screen flipped)
#define CPS2_OBJ_PRI    0x04    // Layers priorities
#define CPS2_OBJ_UK2    0x06    // Unknown (usually 0x0000, 0x1101 in ssf2, 0x0001 in 19XX)
#define CPS2_OBJ_XOFFS  0x08    // X offset (usually 0x0040)
#define CPS2_OBJ_YOFFS  0x0a    // Y offset (always 0x0010)


void cps2_state::unshuffle(uint64_t *buf, int len)
{
	if (len == 2)
		return;

	assert(len % 4 == 0);   /* must not happen */

	len /= 2;

	unshuffle(buf, len);
	unshuffle(buf + len, len);

	for (int i = 0; i < len / 2; i++)
	{
		const uint64_t t = buf[len / 2 + i];
		buf[len / 2 + i] = buf[len + i];
		buf[len + i] = t;
	}
}


void cps2_state::cps2_gfx_decode()
{
	const int banksize = 0x200000;
	const auto size = memregion("gfx")->bytes();

	for (int i = 0; i < size; i += banksize)
		unshuffle((uint64_t *)(memregion("gfx")->base() + i), banksize / 8);
}


void cps2_state::video_start()
{
	cps_state::video_start();

	m_cps2_obj_size = 0x2000;
	m_cps2_buffered_obj = make_unique_clear<uint16_t[]>(m_cps2_obj_size / 2);

	memset(m_objram1, 0, m_cps2_obj_size);
	memset(m_objram2, 0, m_cps2_obj_size);

	save_item(NAME(m_cps2_last_sprite_offset));
	save_pointer(NAME(m_cps2_buffered_obj), m_cps2_obj_size / 2);
	save_item(NAME(m_pri_ctrl));
	save_item(NAME(m_objram_bank));
}



void cps2_state::cps2_objram_bank_w(offs_t offset, uint16_t data, uint16_t mem_mask)
{
	if (ACCESSING_BITS_0_7)
		m_objram_bank = data & 1;
}

uint16_t cps2_state::cps2_objram1_r(offs_t offset)
{
	if (m_objram_bank & 1)
		return m_objram2[offset];
	else
		return m_objram1[offset];
}

uint16_t cps2_state::cps2_objram2_r(offs_t offset)
{
	if (m_objram_bank & 1)
		return m_objram1[offset];
	else
		return m_objram2[offset];
}

void cps2_state::cps2_objram1_w(offs_t offset, uint16_t data, uint16_t mem_mask)
{
	if (m_objram_bank & 1)
		COMBINE_DATA(&m_objram2[offset]);
	else
		COMBINE_DATA(&m_objram1[offset]);
}

void cps2_state::cps2_objram2_w(offs_t offset, uint16_t data, uint16_t mem_mask)
{
	if (m_objram_bank & 1)
		COMBINE_DATA(&m_objram1[offset]);
	else
		COMBINE_DATA(&m_objram2[offset]);
}

uint16_t *cps2_state::cps2_objbase()
{
	int baseptr;
	baseptr = 0x7000;

	if (m_objram_bank & 1)
		baseptr ^= 0x0080;

//popmessage("%04x %d", cps2_port(machine, CPS2_OBJ_BASE), m_objram_bank & 1);

	if (baseptr == 0x7000)
		return m_objram1;
	else //if (baseptr == 0x7080)
		return m_objram2;
}


void cps2_state::find_last_sprite()    /* Find the offset of last sprite */
{
	cps_state::find_last_sprite();

	int offset = 0;
	uint16_t *base = m_cps2_buffered_obj.get();

	/* Locate the end of table marker */
	while (offset < m_cps2_obj_size / 2)
	{
		if (base[offset + 1] >= 0x8000 || base[offset + 3] >= 0xff00)
		{
			/* Marker found. This is the last sprite. */
			m_cps2_last_sprite_offset = offset - 4;
			return;
		}
		
		if ((base[offset + 1] & 0x1000)) //Zero800 uses an unused bit to access an extra gfx bank    
		{
			base[offset + 1] = base[offset + 1] + 0x8000;
		}

		offset += 4;
	}
	/* Sprites must use full sprite RAM */
	m_cps2_last_sprite_offset = m_cps2_obj_size / 2 - 4;
}

void cps2_state::cps2_render_sprites( screen_device &screen, bitmap_ind16 &bitmap, const rectangle &cliprect, int *primasks )
{
#define DRAWSPRITE(CODE,COLOR,FLIPX,FLIPY,SX,SY)                                    \
{                                                                                   \
	if (flip_screen())                                                           \
		m_gfxdecode->gfx(2)->prio_transpen(bitmap,\
				cliprect,                                            \
				CODE,                                                               \
				COLOR,                                                              \
				!(FLIPX),!(FLIPY),                                                  \
				512-16-(SX),256-16-(SY), screen.priority(),primasks[priority],15);                 \
	else                                                                            \
		m_gfxdecode->gfx(2)->prio_transpen(bitmap,\
				cliprect,                                            \
				CODE,                                                               \
				COLOR,                                                              \
				FLIPX,FLIPY,                                                        \
				SX,SY, screen.priority(),primasks[priority],15);                 \
}

	int i;
	uint16_t *base = m_cps2_buffered_obj.get();
	int xoffs = 64 - m_output[CPS2_OBJ_XOFFS /2];
	int yoffs = 16 - m_output[CPS2_OBJ_YOFFS /2];

#ifdef MAME_DEBUG
	if (machine().input().code_pressed(KEYCODE_Z) && machine().input().code_pressed(KEYCODE_R))
	{
		return;
	}
#endif

	for (i = m_cps2_last_sprite_offset; i >= 0; i -= 4)
	{
		int x = base[i + 0];
		int y = base[i + 1];
		int priority = (x >> 13) & 0x07;
		int code = base[i + 2] + ((y & 0xE000) << 3); //Zero800 uses an unused bit to access an extra gfx bank 
		int colour = base[i + 3];
		int col = colour & 0x1f;

		if (colour & 0x80)
		{
			x += m_output[CPS2_OBJ_XOFFS /2];  /* fix the offset of some games */
			y += m_output[CPS2_OBJ_YOFFS /2];  /* like Marvel vs. Capcom ending credits */
		}

		if (colour & 0xff00)
		{
			/* handle blocked sprites */
			int nx = (colour & 0x0f00) >> 8;
			int ny = (colour & 0xf000) >> 12;
			int nxs, nys, sx, sy;
			nx++;
			ny++;

			if (colour & 0x40)
			{
				/* Y flip */
				if (colour & 0x20)
				{
					for (nys = 0; nys < ny; nys++)
					{
						for (nxs = 0; nxs < nx; nxs++)
						{
							sx = (x + nxs * 16 + xoffs) & 0x3ff;
							sy = (y + nys * 16 + yoffs) & 0x3ff;
							DRAWSPRITE(
									code + (nx - 1) - nxs + 0x10 * (ny - 1 - nys),
									(col & 0x1f),
									1,1,
									sx,sy);
						}
					}
				}
				else
				{
					for (nys = 0; nys < ny; nys++)
					{
						for (nxs = 0; nxs < nx; nxs++)
						{
							sx = (x + nxs * 16 + xoffs) & 0x3ff;
							sy = (y + nys * 16 + yoffs) & 0x3ff;

							DRAWSPRITE(
									code + nxs + 0x10 * (ny - 1 - nys),
									(col & 0x1f),
									0,1,
									sx,sy);
						}
					}
				}
			}
			else
			{
				if (colour & 0x20)
				{
					for (nys = 0; nys < ny; nys++)
					{
						for (nxs = 0; nxs < nx; nxs++)
						{
							sx = (x + nxs * 16 + xoffs) & 0x3ff;
							sy = (y + nys * 16 + yoffs) & 0x3ff;

							DRAWSPRITE(
									code + (nx - 1) - nxs + 0x10 * nys,
									(col & 0x1f),
									1,0,
									sx,sy);
						}
					}
				}
				else
				{
					for (nys = 0; nys < ny; nys++)
					{
						for (nxs = 0; nxs < nx; nxs++)
						{
							sx = (x + nxs * 16 + xoffs) & 0x3ff;
							sy = (y + nys * 16 + yoffs) & 0x3ff;

							DRAWSPRITE(
//                                      code + nxs + 0x10 * nys,
									(code & ~0xf) + ((code + nxs) & 0xf) + 0x10 * nys,  //  pgear fix
									(col & 0x1f),
									0,0,
									sx,sy);
						}
					}
				}
			}
		}
		else
		{
			/* Simple case... 1 sprite */
			DRAWSPRITE(
					code,
					(col & 0x1f),
					colour&0x20,colour&0x40,
					(x+xoffs) & 0x3ff,(y+yoffs) & 0x3ff);
		}
	}
}


void cps2_state::render_layers(screen_device &screen, bitmap_ind16 &bitmap, const rectangle &cliprect)
{
	/* Draw layers (0 = sprites, 1-3 = tilemaps) */
	int layercontrol = m_cps_b_regs[m_game_config->layer_control / 2];
	int l0 = (layercontrol >> 0x06) & 0x03;
	int l1 = (layercontrol >> 0x08) & 0x03;
	int l2 = (layercontrol >> 0x0a) & 0x03;
	int l3 = (layercontrol >> 0x0c) & 0x03;
	screen.priority().fill(0, cliprect);

	int primasks[8], i;
	int l0pri = (m_pri_ctrl >> 4 * l0) & 0x0f;
	int l1pri = (m_pri_ctrl >> 4 * l1) & 0x0f;
	int l2pri = (m_pri_ctrl >> 4 * l2) & 0x0f;
	int l3pri = (m_pri_ctrl >> 4 * l3) & 0x0f;

#if 0
if (    (m_output[CPS2_OBJ_BASE /2] != 0x7080 && m_output[CPS2_OBJ_BASE /2] != 0x7000) ||
		m_output[CPS2_OBJ_UK1 /2] != 0x807d ||
		(m_output[CPS2_OBJ_UK2 /2] != 0x0000 && m_output[CPS2_OBJ_UK2 /2] != 0x1101 && m_output[CPS2_OBJ_UK2 /2] != 0x0001))
	popmessage("base %04x uk1 %04x uk2 %04x",
			m_output[CPS2_OBJ_BASE /2],
			m_output[CPS2_OBJ_UK1 /2],
			m_output[CPS2_OBJ_UK2 /2]);

if (0 && machine().input().code_pressed(KEYCODE_Z))
	popmessage("order: %d (%d) %d (%d) %d (%d) %d (%d)",l0,l0pri,l1,l1pri,l2,l2pri,l3,l3pri);
#endif

	/* take out the CPS1 sprites layer */
	if (l0 == 0) { l0 = l1; l1 = 0; l0pri = l1pri; }
	if (l1 == 0) { l1 = l2; l2 = 0; l1pri = l2pri; }
	if (l2 == 0) { l2 = l3; l3 = 0; l2pri = l3pri; }

	{
		int mask0 = 0xaa;
		int mask1 = 0xcc;
		if (l0pri > l1pri) mask0 &= ~0x88;
		if (l0pri > l2pri) mask0 &= ~0xa0;
		if (l1pri > l2pri) mask1 &= ~0xc0;

		primasks[0] = 0xff;
		for (i = 1; i < 8; i++)
		{
			if (i <= l0pri && i <= l1pri && i <= l2pri)
			{
				primasks[i] = 0xfe;
				continue;
			}
			primasks[i] = 0;
			if (i <= l0pri) primasks[i] |= mask0;
			if (i <= l1pri) primasks[i] |= mask1;
			if (i <= l2pri) primasks[i] |= 0xf0;
		}
	}

	cps1_render_layer(screen, bitmap, cliprect, l0, 1);
	cps1_render_layer(screen, bitmap, cliprect, l1, 2);
	cps1_render_layer(screen, bitmap, cliprect, l2, 4);
	cps2_render_sprites(screen, bitmap, cliprect, primasks);
}


uint32_t cps2_state::screen_update_cps2(screen_device &screen, bitmap_ind16 &bitmap, const rectangle &cliprect)
{
	cps2_set_sprite_priorities();
	return screen_update_cps1(screen, bitmap, cliprect);
}

void cps2_state::cps2_set_sprite_priorities()
{
	m_pri_ctrl = m_output[CPS2_OBJ_PRI /2];
}

void cps2_state::cps2_objram_latch()
{
	cps2_set_sprite_priorities();
	memcpy(m_cps2_buffered_obj.get(), cps2_objbase(), m_cps2_obj_size);
}



/*************************************
 *
 *  Interrupt generation
 *
 *************************************/

TIMER_DEVICE_CALLBACK_MEMBER(cps2_state::cps2_interrupt)
{
	// Direct irq line connection, IPL1 is vblank, IPL2 is some sort of scanline interrupt.
	if (param == 0)
		m_scancalls = 0;

	if (m_cps_b_regs[0x10 / 2] & 0x8000)
		m_cps_b_regs[0x10 / 2] &= 0x1ff;

	if (m_cps_b_regs[0x12 / 2] & 0x8000)
		m_cps_b_regs[0x12 / 2] &= 0x1ff;

//  popmessage("%04x %04x - %04x %04x",m_scanline1,m_scanline2,m_cps_b_regs[0x10/2],m_cps_b_regs[0x12/2]);

	// Raster effects
	if (m_scanline1 == param || (m_scanline1 < param && !m_scancalls))
	{
		m_cps_b_regs[0x10/2] = 0;
		m_maincpu->set_input_line(2, HOLD_LINE);
		m_screen->update_partial(param);
		m_scancalls++;
//      popmessage("IRQ4 scancounter = %04i", param);
	}

	// Raster effects
	if(m_scanline2 == param || (m_scanline2 < param && !m_scancalls))
	{
		m_cps_b_regs[0x12 / 2] = 0;
		m_maincpu->set_input_line(2, HOLD_LINE);
		m_screen->update_partial(param);
		m_scancalls++;
//      popmessage("IRQ4 scancounter = %04i", param);
	}

	if (param == 240)  // VBlank
	{
		m_cps_b_regs[0x10 / 2] = m_scanline1;
		m_cps_b_regs[0x12 / 2] = m_scanline2;
		m_maincpu->set_input_line(1, HOLD_LINE);
		cps2_objram_latch();
	}
//  popmessage("Raster calls = %i", m_scancalls);
}


/*************************************
 *
 *  EEPROM
 *
 *************************************/

void cps2_state::cps2_eeprom_port_w(offs_t offset, uint16_t data, uint16_t mem_mask)
{
	if (ACCESSING_BITS_8_15)
	{
		/* bit 0 - Unused */
		/* bit 1 - Unused */
		/* bit 2 - Unused */
		/* bit 3 - Unused? */
		/* bit 4 - Eeprom data  */
		/* bit 5 - Eeprom clock */
		/* bit 6 - */
		/* bit 7 - */

		// EEPROM
		ioport("EEPROMOUT")->write(data, 0xffff);	
	}

	if (ACCESSING_BITS_0_7)
	{
		/* bit 0 - coin counter 1 */
		/* bit 0 - coin counter 2 */
		/* bit 2 - Unused */
		/* bit 3 - Allows access to Z80 address space (Z80 reset) */
		/* bit 4 - lock 1  */
		/* bit 5 - lock 2  */
		/* bit 6 - */
		/* bit 7 - */

		// Z80 Reset
		if (m_audiocpu != nullptr)
			m_audiocpu->set_input_line(INPUT_LINE_RESET, (data & 0x0008) ? CLEAR_LINE : ASSERT_LINE);

		machine().bookkeeping().coin_counter_w(0, data & 0x0001);
		if (m_cps2_dial_type == 1) // pzloop2
		{
			// Puzz Loop 2 uses coin counter 2 input to switch between stick and paddle controls
			m_readpaddle = data & 0x0002;
		}
		else
		{
			machine().bookkeeping().coin_counter_w(1, data & 0x0002);
		}

		if (strncmp(machine().system().name, "mmatrix", 7) == 0) // Mars Matrix seems to require the coin lockout bit to be reversed
		{
			machine().bookkeeping().coin_lockout_w(0, data & 0x0010);
			machine().bookkeeping().coin_lockout_w(1, data & 0x0020);
			machine().bookkeeping().coin_lockout_w(2, data & 0x0040);
			machine().bookkeeping().coin_lockout_w(3, data & 0x0080);
		}
		else
		{
			machine().bookkeeping().coin_lockout_w(0, ~data & 0x0010);
			machine().bookkeeping().coin_lockout_w(1, ~data & 0x0020);
			machine().bookkeeping().coin_lockout_w(2, ~data & 0x0040);
			machine().bookkeeping().coin_lockout_w(3, ~data & 0x0080);
		}

		/*
		output().set_led_value(0, data & 0x01);
		output().set_led_value(1, data & 0x10);
		output().set_led_value(2, data & 0x20);
		*/
	}
}


/*************************************
 *
 *  Sound ?
 *
 *************************************/

TIMER_CALLBACK_MEMBER(cps2_state::cps2_update_digital_volume)
{
	int vol_button_state = ioport("DIGITALVOL")->read();

	if (vol_button_state & 0x01) m_cps2digitalvolumelevel -= 1;
	if (vol_button_state & 0x02) m_cps2digitalvolumelevel += 1;

	if (m_cps2digitalvolumelevel > 39) m_cps2digitalvolumelevel = 39;
	if (m_cps2digitalvolumelevel < 0) m_cps2digitalvolumelevel = 0;

	m_qsound->set_output_gain(0, m_cps2digitalvolumelevel / 39.0);
	m_qsound->set_output_gain(1, m_cps2digitalvolumelevel / 39.0);
}

uint16_t cps2_state::cps2_qsound_volume_r()
{
	static const uint16_t cps2_vol_states[40] =
	{
		0xf010, 0xf008, 0xf004, 0xf002, 0xf001, 0xe810, 0xe808, 0xe804, 0xe802, 0xe801,
		0xe410, 0xe408, 0xe404, 0xe402, 0xe401, 0xe210, 0xe208, 0xe204, 0xe202, 0xe201,
		0xe110, 0xe108, 0xe104, 0xe102, 0xe101, 0xe090, 0xe088, 0xe084, 0xe082, 0xe081,
		0xe050, 0xe048, 0xe044, 0xe042, 0xe041, 0xe030, 0xe028, 0xe024, 0xe022, 0xe021
	};

	uint16_t result;

	result = cps2_vol_states[m_cps2digitalvolumelevel];

	// Extra adapter memory (0x660000-0x663fff) available when bit 14 = 0
	// Network adapter (ssf2tb) present when bit 15 = 0
	// Only game known to use both these so far is SSF2TB

	if (m_comm && m_comm->comm_enabled())
		return 0x2021; // SSF2TB doesn't have a digital slider in the test screen
	else
		if (m_cps2disabledigitalvolume)
			return 0xd000; // Digital display isn't shown in test mode
		else
			return result;
}

void cps2_state::qsound_sharedram1_samples_w(offs_t offset, uint16_t data, uint16_t mem_mask) //Zero800 allows you to write and signal when a new song is played
{

		if (ACCESSING_BITS_0_7)
			{										
				
				if (offset == 0x0001 && (data & 0xff) == 0x00 && m_qsound_sharedram1[0x00] == 0xff) //stop samples sound
					{
					m_samples_l->start(0, 0, false);	
					m_samples_r->start(0, 0, false);				
					//printf("%s Stop: %02x to %x\n", machine().describe_context().c_str(), offset, offset);		
					} 	
					
				if (offset == 0xffe)
					{		
						if (m_qsound_sharedram1[0x80] == 0x77) //stop music condition in audiocpu = C080 (z80 hacked)
						{
							m_samples_l->start(0, 0, false); //init L samples	
							m_samples_r->start(0, 0, false); //init R samples
							m_qsound_sharedram2[0x27] = 0xc9; //Enable Qsound Volume	
							m_qsound_sharedram1[0x80] = 0xff; //Reset music id flag							
							m_qsound_sharedram1[0x81] = 0xff; //Reset music id flag							
							//printf("%s Clear: %02x to %x\n", machine().describe_context().c_str(), data, offset);	
						} else
							
						if (m_qsound_sharedram1[0x81] > 0x00 && m_qsound_sharedram1[0x81] < 0x40) //play music condition in audiocpu = C081 (z80 hacked)
						{
							int data_byte = m_qsound_sharedram1[0x81];
							m_qsound_sharedram1[0x80] = 0xff; //Reset music id flag	
							m_qsound_sharedram1[0x81] = 0xff; //Reset music id flag	
							//printf("%s Music: %02x to %x\n", machine().describe_context().c_str(), data_byte, offset);			
						
							m_samples_l->start(0, data_byte, true);					
							if (m_samples_l->playing(0) == true)
								{				
								m_qsound_sharedram2[0x27] = 0x00; //Qsound music volume = 0				
								} else
									m_samples_l->start(0, 0, false); //init L samples							
							
							m_samples_r->start(0, data_byte, true);					
							if (m_samples_r->playing(0)  == true) 
								{
								m_qsound_sharedram2[0x27] = 0x00; //Qsound music volume = 0				
								} else										
									m_samples_r->start(0, 0, false); //init R samples	
						} else						
						
						if (m_qsound_sharedram2[0x27] == 0x00 && m_samples_l->playing(0) != true) //if loadstate and qsound volume = 0 && samples stopped
						{
							m_samples_l->start(0, 0, false); //init L samples	
							m_samples_r->start(0, 0, false); //init R samples
							m_qsound_sharedram2[0x27] = 0xc9; //Enable Qsound Volume	
							m_qsound_sharedram1[0x80] = 0xff; //Reset music id flag							
							m_qsound_sharedram1[0x81] = 0xff; //Reset music id flag							
							//printf("%s Volume0: %02x to %x\n", machine().describe_context().c_str(), data, offset);	
						} else
							
						if (m_qsound_sharedram2[0x27] != 0x00 && m_samples_l->playing(0) == true) //if loadstate and qsound volume != 0 && samples playing
						{
							m_samples_l->start(0, 0, false); //init L samples
							m_samples_r->start(0, 0, false); //init R samples
							m_qsound_sharedram2[0x27] = 0xc9; //Enable Qsound Volume		
							m_qsound_sharedram1[0x80] = 0xff; //Reset music id flag							
							m_qsound_sharedram1[0x81] = 0xff; //Reset music id flag							
							//printf("%s Volume0: %02x to %x\n", machine().describe_context().c_str(), data, offset);	
						} else 
						
						if ((data & 0xff) == 0x55) //init samples in rom start
						{
							m_samples_l->start(0, 0, false);	
							m_samples_r->start(0, 0, false);	
						} 
						
					}	

			m_qsound_sharedram1[offset] = data;	//Play data Qsound		

			}	
		
}

/*************************************
 *
 *  Read handlers
 *
 *************************************/

uint16_t cps2_state::kludge_r()
{
	return 0xffff;
}


/*************************************
 *
 *  Memory map
 *
 *************************************/

void cps2_state::cps2turbo_map(address_map &map) //Zero800
{
	map(0x000000, 0x5fffff).rom(); //+200000 extra                                                  												   // 68000 ROM
	map(0x618000, 0x619fff).rw(FUNC(cps2_state::qsound_sharedram1_r), FUNC(cps2_state::qsound_sharedram1_samples_w));                 // Q RAM + Play samples condition
	map(0x660000, 0x663fff).ram();                                                                                                    // When bit 14 of 0x804030 equals 0 this space is available. Many games store highscores and other info here if available.
	map(0x664000, 0x664001).ram();                                                                                                    // Unknown - Only used if 0x660000-0x663fff available (could be RAM enable?)
	map(0x665000, 0x66500b).ram().share("output"); //moved from 0x400000  																   // CPS2 object output			
	map(0x700000, 0x701fff).w(FUNC(cps2_state::cps2_objram1_w)).share("objram1");                                                     // Object RAM, no game seems to use it directly
	map(0x708000, 0x709fff).mirror(0x006000).rw(FUNC(cps2_state::cps2_objram2_r), FUNC(cps2_state::cps2_objram2_w)).share("objram2"); // Object RAM
	map(0x800100, 0x80013f).w(FUNC(cps2_state::cps1_cps_a_w)).share("cps_a_regs");                                                    // Mirror (sfa)
	map(0x800140, 0x80017f).rw(FUNC(cps2_state::cps1_cps_b_r), FUNC(cps2_state::cps1_cps_b_w)).share("cps_b_regs");                   // Mirror (sfa)
	map(0x804000, 0x804001).portr("IN0");                                                                                             // IN0
	map(0x804010, 0x804011).portr("IN1");                                                                                             // IN1
	map(0x804020, 0x804021).portr("IN2");                                                                                             // IN2 + EEPROM
	map(0x804030, 0x804031).r(FUNC(cps2_state::cps2_qsound_volume_r));                                                                // Master volume. Also when bit 14=0 addon memory is present, when bit 15=0 network adapter present.
	map(0x804040, 0x804041).w(FUNC(cps2_state::cps2_eeprom_port_w));                                                                  // EEPROM
	map(0x8040a0, 0x8040a1).nopw();                                                                                                   // Unknown (reset once on startup)
	map(0x8040b0, 0x8040b3).r(FUNC(cps2_state::kludge_r));                                                                            // Unknown (xmcotaj hangs if this is 0)
	map(0x8040e0, 0x8040e1).w(FUNC(cps2_state::cps2_objram_bank_w));                                                                  // bit 0 = Object ram bank swap
	map(0x804100, 0x80413f).w(FUNC(cps2_state::cps1_cps_a_w)).share("cps_a_regs");                                                    // CPS-A custom
	map(0x804140, 0x80417f).rw(FUNC(cps2_state::cps1_cps_b_r), FUNC(cps2_state::cps1_cps_b_w));                                       // CPS-B custom
	map(0x900000, 0x92ffff).ram().w(FUNC(cps2_state::cps1_gfxram_w)).share("gfxram");                                                 // Video RAM
	map(0xff0000, 0xffffef).ram();                                                                                                    // RAM
	map(0xfffff0, 0xfffffb).ram().share("output");                                                                                    // CPS2 output
	map(0xfffffc, 0xffffff).ram();
}

void cps2_state::decrypted_cps2turbo(address_map &map) //Zero800
{
	map(0x000000, 0x5fffff).rom().share("decrypted_opcodes"); // 68000 ROM
}

/*************************************
 *
 *  Game-specific port definitions
 *
 *************************************/
 
// 4 players and 4 buttons
static INPUT_PORTS_START( cps2_4p4b )
	PORT_START("IN0")      // (0x00)
	PORT_BIT( 0x0001, IP_ACTIVE_LOW, IPT_JOYSTICK_RIGHT ) PORT_8WAY PORT_PLAYER(1)
	PORT_BIT( 0x0002, IP_ACTIVE_LOW, IPT_JOYSTICK_LEFT ) PORT_8WAY PORT_PLAYER(1)
	PORT_BIT( 0x0004, IP_ACTIVE_LOW, IPT_JOYSTICK_DOWN ) PORT_8WAY PORT_PLAYER(1)
	PORT_BIT( 0x0008, IP_ACTIVE_LOW, IPT_JOYSTICK_UP ) PORT_8WAY PORT_PLAYER(1)
	PORT_BIT( 0x0010, IP_ACTIVE_LOW, IPT_BUTTON1 ) PORT_PLAYER(1)
	PORT_BIT( 0x0020, IP_ACTIVE_LOW, IPT_BUTTON2 ) PORT_PLAYER(1)
	PORT_BIT( 0x0040, IP_ACTIVE_LOW, IPT_BUTTON3 ) PORT_PLAYER(1)
	PORT_BIT( 0x0080, IP_ACTIVE_LOW, IPT_BUTTON4 ) PORT_PLAYER(1)
	PORT_BIT( 0x0100, IP_ACTIVE_LOW, IPT_JOYSTICK_RIGHT ) PORT_8WAY PORT_PLAYER(2)
	PORT_BIT( 0x0200, IP_ACTIVE_LOW, IPT_JOYSTICK_LEFT ) PORT_8WAY PORT_PLAYER(2)
	PORT_BIT( 0x0400, IP_ACTIVE_LOW, IPT_JOYSTICK_DOWN ) PORT_8WAY PORT_PLAYER(2)
	PORT_BIT( 0x0800, IP_ACTIVE_LOW, IPT_JOYSTICK_UP ) PORT_8WAY PORT_PLAYER(2)
	PORT_BIT( 0x1000, IP_ACTIVE_LOW, IPT_BUTTON1 ) PORT_PLAYER(2)
	PORT_BIT( 0x2000, IP_ACTIVE_LOW, IPT_BUTTON2 ) PORT_PLAYER(2)
	PORT_BIT( 0x4000, IP_ACTIVE_LOW, IPT_BUTTON3 ) PORT_PLAYER(2)
	PORT_BIT( 0x8000, IP_ACTIVE_LOW, IPT_BUTTON4 ) PORT_PLAYER(2)

	PORT_START("IN1")      // (0x10)
	PORT_BIT( 0x0001, IP_ACTIVE_LOW, IPT_JOYSTICK_RIGHT ) PORT_8WAY PORT_PLAYER(3)
	PORT_BIT( 0x0002, IP_ACTIVE_LOW, IPT_JOYSTICK_LEFT ) PORT_8WAY PORT_PLAYER(3)
	PORT_BIT( 0x0004, IP_ACTIVE_LOW, IPT_JOYSTICK_DOWN ) PORT_8WAY PORT_PLAYER(3)
	PORT_BIT( 0x0008, IP_ACTIVE_LOW, IPT_JOYSTICK_UP ) PORT_8WAY PORT_PLAYER(3)
	PORT_BIT( 0x0010, IP_ACTIVE_LOW, IPT_BUTTON1 ) PORT_PLAYER(3)
	PORT_BIT( 0x0020, IP_ACTIVE_LOW, IPT_BUTTON2 ) PORT_PLAYER(3)
	PORT_BIT( 0x0040, IP_ACTIVE_LOW, IPT_BUTTON3 ) PORT_PLAYER(3)
	PORT_BIT( 0x0080, IP_ACTIVE_LOW, IPT_BUTTON4 ) PORT_PLAYER(3)
	PORT_BIT( 0x0100, IP_ACTIVE_LOW, IPT_JOYSTICK_RIGHT ) PORT_8WAY PORT_PLAYER(4)
	PORT_BIT( 0x0200, IP_ACTIVE_LOW, IPT_JOYSTICK_LEFT ) PORT_8WAY PORT_PLAYER(4)
	PORT_BIT( 0x0400, IP_ACTIVE_LOW, IPT_JOYSTICK_DOWN ) PORT_8WAY PORT_PLAYER(4)
	PORT_BIT( 0x0800, IP_ACTIVE_LOW, IPT_JOYSTICK_UP ) PORT_8WAY PORT_PLAYER(4)
	PORT_BIT( 0x1000, IP_ACTIVE_LOW, IPT_BUTTON1 ) PORT_PLAYER(4)
	PORT_BIT( 0x2000, IP_ACTIVE_LOW, IPT_BUTTON2 ) PORT_PLAYER(4)
	PORT_BIT( 0x4000, IP_ACTIVE_LOW, IPT_BUTTON3 ) PORT_PLAYER(4)
	PORT_BIT( 0x8000, IP_ACTIVE_LOW, IPT_BUTTON4 ) PORT_PLAYER(4)

	PORT_START("IN2")      // (0x20)
	PORT_BIT( 0x0001, IP_ACTIVE_HIGH, IPT_CUSTOM ) PORT_READ_LINE_DEVICE_MEMBER("eeprom", eeprom_serial_93cxx_device, do_read)
	PORT_SERVICE_NO_TOGGLE( 0x0002, IP_ACTIVE_LOW )
	PORT_BIT( 0x0004, IP_ACTIVE_LOW, IPT_SERVICE1 )
	PORT_BIT( 0x00f8, IP_ACTIVE_LOW, IPT_UNKNOWN )
	PORT_BIT( 0x0100, IP_ACTIVE_LOW, IPT_START1 )
	PORT_BIT( 0x0200, IP_ACTIVE_LOW, IPT_START2 )
	PORT_BIT( 0x0400, IP_ACTIVE_LOW, IPT_START3 )
	PORT_BIT( 0x0800, IP_ACTIVE_LOW, IPT_START4 )
	PORT_BIT( 0x1000, IP_ACTIVE_LOW, IPT_COIN1 )
	PORT_BIT( 0x2000, IP_ACTIVE_LOW, IPT_COIN2 )
	PORT_BIT( 0x4000, IP_ACTIVE_LOW, IPT_COIN3 )
	PORT_BIT( 0x8000, IP_ACTIVE_LOW, IPT_COIN4 )

	PORT_START( "EEPROMOUT" )
	PORT_BIT( 0x1000, IP_ACTIVE_HIGH, IPT_OUTPUT ) PORT_WRITE_LINE_DEVICE_MEMBER("eeprom", eeprom_serial_93cxx_device, di_write)
	PORT_BIT( 0x2000, IP_ACTIVE_HIGH, IPT_OUTPUT ) PORT_WRITE_LINE_DEVICE_MEMBER("eeprom", eeprom_serial_93cxx_device, clk_write)
	PORT_BIT( 0x4000, IP_ACTIVE_HIGH, IPT_OUTPUT ) PORT_WRITE_LINE_DEVICE_MEMBER("eeprom", eeprom_serial_93cxx_device, cs_write)

	// Fake inputs for digital volume buttons
	PORT_START( "DIGITALVOL" )
	PORT_BIT( 0x0001, IP_ACTIVE_HIGH, IPT_VOLUME_DOWN )
	PORT_BIT( 0x0002, IP_ACTIVE_HIGH, IPT_VOLUME_UP )
INPUT_PORTS_END 
 
static INPUT_PORTS_START( cps2_3p4b )
	PORT_INCLUDE(cps2_4p4b)

	PORT_MODIFY("IN1")
	PORT_BIT( 0xff00, IP_ACTIVE_LOW, IPT_UNUSED ) // PORT_PLAYER(4) inputs

	PORT_MODIFY("IN2")
	PORT_BIT( 0x0800, IP_ACTIVE_LOW, IPT_UNUSED ) // START4
	PORT_BIT( 0x8000, IP_ACTIVE_LOW, IPT_UNUSED ) // COIN4
INPUT_PORTS_END 
 
/* 2 players and 4 buttons */
static INPUT_PORTS_START( cps2_2p4b )
	PORT_INCLUDE(cps2_3p4b)

	PORT_MODIFY("IN1")
	PORT_BIT( 0x00ff, IP_ACTIVE_LOW, IPT_UNUSED ) // PORT_PLAYER(3) inputs

	PORT_MODIFY("IN2")
	PORT_BIT( 0x0400, IP_ACTIVE_LOW, IPT_UNUSED ) // START3
	PORT_BIT( 0x4000, IP_ACTIVE_LOW, IPT_UNUSED ) // COIN3
INPUT_PORTS_END 
 
// 2 players and 3 buttons
static INPUT_PORTS_START( cps2_2p3b )
	PORT_INCLUDE(cps2_2p4b)

	PORT_MODIFY("IN0")
	PORT_BIT( 0x0080, IP_ACTIVE_LOW, IPT_UNUSED ) // BUTTON4 PORT_PLAYER(1)
	PORT_BIT( 0x8000, IP_ACTIVE_LOW, IPT_UNUSED ) // BUTTON4 PORT_PLAYER(2)
INPUT_PORTS_END

// 2 players and 6 buttons (2 rows of 3 buttons)
static INPUT_PORTS_START( cps2_2p6b )
	PORT_INCLUDE(cps2_2p3b)

	PORT_MODIFY("IN1")
	PORT_BIT( 0x0001, IP_ACTIVE_LOW, IPT_BUTTON4 ) PORT_PLAYER(1)
	PORT_BIT( 0x0002, IP_ACTIVE_LOW, IPT_BUTTON5 ) PORT_PLAYER(1)
	PORT_BIT( 0x0004, IP_ACTIVE_LOW, IPT_BUTTON6 ) PORT_PLAYER(1)
	PORT_BIT( 0x0010, IP_ACTIVE_LOW, IPT_BUTTON4 ) PORT_PLAYER(2)
	PORT_BIT( 0x0020, IP_ACTIVE_LOW, IPT_BUTTON5 ) PORT_PLAYER(2)

	PORT_MODIFY("IN2")
	PORT_BIT( 0x4000, IP_ACTIVE_LOW, IPT_BUTTON6 ) PORT_PLAYER(2)
INPUT_PORTS_END

/*************************************
 *
 *  Machine driver
 *
 *************************************/

MACHINE_START_MEMBER(cps2_state,cps2)
{
		membank("bank1")->configure_entries(0, (QSOUND_SIZE - 0x10000) / 0x4000, memregion("audiocpu")->base() + 0x10000, 0x4000); //Zero800
}


void cps2_state::cps2turbo(machine_config &config) //Zero800
{
	// Basic machine hardware
	M68000(config, m_maincpu, XTAL(32'000'000)); //+ 100%
	m_maincpu->set_addrmap(AS_PROGRAM, &cps2_state::cps2turbo_map);
	m_maincpu->set_addrmap(AS_OPCODES, &cps2_state::decrypted_cps2turbo);
	m_maincpu->disable_interrupt_mixer();

	TIMER(config, "scantimer").configure_scanline(FUNC(cps2_state::cps2_interrupt), "screen", 0, 1);

	Z80(config, m_audiocpu, XTAL(16'000'000)); //+ 100%
	m_audiocpu->set_addrmap(AS_PROGRAM, &cps2_state::qsound_sub_map);
	m_audiocpu->set_periodic_int(FUNC(cps2_state::irq0_line_hold), attotime::from_hz(250)); // measured

	MCFG_MACHINE_START_OVERRIDE(cps2_state, cps2)

	EEPROM_93C46_16BIT(config, "eeprom");

	// Video hardware
	SCREEN(config, m_screen, SCREEN_TYPE_RASTER);
	m_screen->set_video_attributes(VIDEO_UPDATE_BEFORE_VBLANK);
	m_screen->set_raw(CPS_PIXEL_CLOCK, CPS_HTOTAL, 48, 464, CPS_VTOTAL, 12, 246);
	m_screen->set_screen_update(FUNC(cps2_state::screen_update_cps2));
	m_screen->screen_vblank().set(FUNC(cps2_state::screen_vblank_cps1));
	m_screen->set_palette(m_palette);

	GFXDECODE(config, m_gfxdecode, m_palette, gfx_cps1);
	PALETTE(config, m_palette, palette_device::BLACK).set_entries(0xc00);

	// Sound hardware
	SPEAKER(config, "lspeaker").front_left();
	SPEAKER(config, "rspeaker").front_right();

	QSOUND(config, m_qsound);
	m_qsound->add_route(0, "lspeaker", 1.0);
	m_qsound->add_route(1, "rspeaker", 1.0);

	//Zero800
	SAMPLES(config, m_samples_l);
	m_samples_l->set_channels(1);
	m_samples_l->set_samples_names(samples_id_l);
	m_samples_l->add_route(0, "lspeaker", 0.1);
	
	SAMPLES(config, m_samples_r);
	m_samples_r->set_channels(1);
	m_samples_r->set_samples_names(samples_id_r);
	m_samples_r->add_route(0, "rspeaker", 0.1); 

}


/*************************************
 *
 *  Games initialization
 *
 *************************************/

void cps2_state::init_digital_volume()
{
	m_cps2digitalvolumelevel = 39; // maximum
	m_cps2disabledigitalvolume = 0;

	// create a timer to update our volume state from the fake switches - read it every 6 frames or so to enable some granularity
	m_digital_volume_timer = timer_alloc(FUNC(cps2_state::cps2_update_digital_volume), this);
	m_digital_volume_timer->adjust(attotime::from_msec(100), 0, attotime::from_msec(100));
}


void cps2_state::init_cps2_video()
{
	cps2_gfx_decode();

	m_scanline1 = 262;
	m_scanline2 = 262;
	m_scancalls = 0;
	m_last_sprite_offset = 0;
	m_cps2_last_sprite_offset = 0;
	m_pri_ctrl = 0;
	m_objram_bank = 0;
}


void cps2_state::init_cps2crypt()
{
	if (m_region_key)
	{
		unsigned short decoded[10] = { 0 };
		for (int b = 0; b < 10 * 16; b++)
		{
			int bit = (317 - b) % 160;
			if ((m_region_key->base()[bit / 8] >> ((bit ^ 7) % 8)) & 1)
			{
				decoded[b / 16] |= (0x8000 >> (b % 16));
			}
		}

		uint32_t key[2] = { ((uint32_t)decoded[0] << 16) | decoded[1], ((uint32_t)decoded[2] << 16) | decoded[3] };
		// decoded[4] == watchdog instruction third word
		// decoded[5] == watchdog instruction second word
		// decoded[6] == watchdog instruction first word
		// decoded[7] == 0x4000 (bits 8 to 23 of CPS2 object output address)
		// decoded[8] == 0x0900

		uint32_t lower, upper;
		if (decoded[9] == 0xffff)
		{
			// On a dead board, the only encrypted range is actually FF0000-FFFFFF.
			// It doesn't start from 0, and it's the upper half of a 128kB bank.
			upper = 0xffffff;
			lower = 0xff0000;
		}
		else
		{
			upper = (((~decoded[9] & 0x3ff) << 14) | 0x3fff) + 1;
			lower = 0;
		}

		logerror("cps2 decrypt 0x%08x,0x%08x,0x%08x,0x%08x\n", key[0], key[1], lower, upper);

		// we have a proper key so use it to decrypt
		cps2_decrypt(machine(), (uint16_t *)memregion("maincpu")->base(), m_decrypted_opcodes, memregion("maincpu")->bytes(), key, lower / 2, upper / 2);
	}
}



void cps2_state::init_cps2()
{
	// Decrypt the game - see machine/cps2crypt.cpp
	init_cps2crypt();
	init_cps2nc();
}

void cps2_state::init_cps2nc()
{
	// Initialize some video elements
	init_cps2_video();

	init_digital_volume();

	m_maincpu->set_clock_scale(0.7375f); // RAM access waitstates etc. aren't emulated - slow the CPU to compensate
}

/*
 ------------------------
 Phoenix bootleg sets
 ------------------------

 The Phoenix sets were created by Razoola as a method of allowing the games to run on
 CPS2 boards where the battery had died.  When this happens the boards run non-encrypted
 code, but the memory mapping is changed.  As the original games have encrypted code
 mixed with decrypted data the program roms must be carefully modified in order to
 correctly contain only decrypted code and data, as well as modification to compensate
 for the memory map changes that occur on the dead boards.  Due nature of this process
 there were sometimes errors introduced into the 'Phoenix' sets.

 Unfortunately the 'Phoenix' sets also ended up forming the basis of a mass cps2
 bootlegging operation whereby cheap CPS2 B boards were purchased, the encryption keys
 killed, and the boards converted to more desirable games.  These started off as single
 game bootlegs of in-demand titles, but soon started also forming the basis of xx-in-1
 bootlegs running on heavily customized B-boards.  These are not legitimate Capcom
 products despite appearing to be so.

 These bootlegs are often sold as 'Phoenix Edition' after Razoola's name, 'xx-in-1', or
 simply 'Suicide-Free' to further artificially inflate the price. Buyer Beware!

 All sets are marked as bootleg because they're unauthorized modifications of the
 original Capcom rom data, and were used for bootleg conversions.

 This may not be a complete list of sets, it was taken from MamePlus.  Other sets, and
 further customized bootlegs boards are known to exist.

*/

ROM_START( sfz3mix ) //Zero800
	ROM_REGION( 0x600000, "maincpu", 0 ) // 68000 code
	ROM_LOAD( "maincpu.bin", 0x000000, 0x600000, NO_DUMP )

	ROM_REGION16_BE( CODE_SIZE, "user1", 0 )
	ROM_FILL( 0x000000, 0x100000, 0x00 )

	ROM_REGION( 0x4000000, "gfx", 0 )
	ROM_LOAD( "gfx.bin",   0x0000000, 0x2000000, NO_DUMP )
	ROM_LOAD( "gfx2.bin",  0x2000000, 0x2000000, NO_DUMP )

	ROM_REGION( QSOUND_TURBO_SIZE, "audiocpu", 0 ) // 64k for the audio CPU (+banks)
	ROM_LOAD( "audiocpu.bin",   0x00000, 0x80000, NO_DUMP )

	ROM_REGION( 0x1000000, "qsound", 0 ) // QSound samples
	ROM_LOAD( "qsound.bin",   0x000000, 0x1000000, NO_DUMP )

	ROM_REGION( 0x20, "key", 0 )
	ROM_LOAD( "phoenix.key",  0x000000, 0x000014, NO_DUMP )
ROM_END

} // anonymous namespace

/*************************************
 *
 *  Game drivers
 *
 *************************************/
 
GAME( 2023, sfz3mix,    0,     	  cps2turbo,     cps2_2p6b, cps2_state, init_cps2,     ROT0,   "bootleg", "Street Fighter Zero 3 Mix (CPS2Turbo)",                         							    MACHINE_SUPPORTS_SAVE ) //Zero800