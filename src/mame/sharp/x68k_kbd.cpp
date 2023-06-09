// license:BSD-3-Clause
// copyright-holders:Barry Rodewald,Vas Crabb
#include "emu.h"
#include "x68k_kbd.h"

#include "machine/keyboard.ipp"


x68k_keyboard_device::x68k_keyboard_device(const machine_config& mconfig, const char* tag, device_t* owner, uint32_t clock)
	: buffered_rs232_device(mconfig, X68K_KEYBOARD, tag, owner, 0)
	, device_matrix_keyboard_interface(mconfig, *this, "LINE0", "LINE1", "LINE2", "LINE3", "LINE4", "LINE5", "LINE6", "LINE7", "LINE8", "LINE9", "LINEA", "LINEB", "LINEC", "LINED", "LINEE")
	, m_led_kana(*this, "key_led_kana")
	, m_led_romaji(*this, "key_led_romaji")
	, m_led_code(*this, "key_led_code")
	, m_led_caps(*this, "key_led_caps")
	, m_led_insert(*this, "key_led_insert")
	, m_led_hiragana(*this, "key_led_hiragana")
	, m_led_fullsize(*this, "key_led_fullsize")
{
}


void x68k_keyboard_device::received_byte(uint8_t data)
{
	/* Keyboard control commands:
	   00xxxxxx - TV Control
	              Not of much use as yet

	   01000xxy - y = Mouse control signal

	   01001xxy - y = Keyboard enable

	   010100xy - y = Sharp X1 display compatibility mode

	   010101xx - xx = LED brightness (00 = bright, 11 = dark)

	   010110xy - y = Display control enable

	   010111xy - y = Display control via the Opt. 2 key enable

	   0110xxxx - xxxx = Key delay (default 500ms)
	                     100 * (delay time) + 200ms

	   0111xxxx - xxxx = Key repeat rate  (default 110ms)
	                     (repeat rate)^2*5 + 30ms

	   1xxxxxxx - xxxxxxx = keyboard LED status
	              b6 = fullwidth
	              b5 = hiragana
	              b4 = insert
	              b3 = caps
	              b2 = code input
	              b1 = romaji
	              b0 = kana
	*/

	if (data & 0x80)  // LED status
	{
		m_led_kana = BIT(data, 0);
		m_led_romaji = BIT(data, 1);
		m_led_code = BIT(data, 2);
		m_led_caps = BIT(data, 3);
		m_led_insert = BIT(data, 4);
		m_led_hiragana = BIT(data, 5);
		m_led_fullsize = BIT(data, 6);
		logerror("KB: LED status set to %02x\n", data & 0x7f);
	}

	if ((data & 0xc0) == 0)  // TV control
	{
		// nothing for now
	}

	if ((data & 0xf8) == 0x48)  // Keyboard enable
	{
		m_enabled = data & 0x01;
		if (m_enabled) start_processing(attotime::from_hz(2'400));
		else stop_processing();
		logerror("KB: Keyboard enable bit = %i\n", m_enabled);
	}

	if ((data & 0xf0) == 0x60)  // Key delay time
	{
		m_delay = ((data & 0x0f) * 100) + 200;
		logerror("KB: Keypress delay time is now %ims\n", m_delay);
	}

	if ((data & 0xf0) == 0x70)  // Key repeat rate
	{
		m_repeat = (((data & 0x0f)^2) * 5) + 30;
		logerror("KB: Keypress repeat rate is now %ims\n", m_repeat);
	}
}

void x68k_keyboard_device::key_make(uint8_t row, uint8_t column)
{
	// TODO: work out which keys actually repeat (this assumes it's anything other than ctrl/opt/shift)
	if (row != 0x0eU)
		typematic_start(row, column, attotime::from_msec(m_delay), attotime::from_msec(m_repeat));
	else
		typematic_restart(attotime::from_msec(m_delay), attotime::from_msec(m_repeat));

	transmit_byte((row << 3) | column);
}

void x68k_keyboard_device::key_repeat(uint8_t row, uint8_t column)
{
	transmit_byte((row << 3) | column);
}

void x68k_keyboard_device::key_break(uint8_t row, uint8_t column)
{
	device_matrix_keyboard_interface::key_break(row, column);

	transmit_byte(0x80U | (row << 3) | column);
}

static INPUT_PORTS_START( x68k_keyboard )

	PORT_START("LINE0")
	PORT_BIT( 0x01, IP_ACTIVE_HIGH, IPT_UNUSED) // unused
	PORT_BIT( 0x02, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"ESC")                   PORT_CODE(KEYCODE_ESC)        PORT_CHAR(27)                       // Escape
	PORT_BIT( 0x04, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"1  !  \u306c")          PORT_CODE(KEYCODE_1)          PORT_CHAR('1') PORT_CHAR('!')       // 1 ! ぬ (nu)
	PORT_BIT( 0x08, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"2  \"  \u3075")         PORT_CODE(KEYCODE_2)          PORT_CHAR('2') PORT_CHAR('\"')      // 2 " ふ (fu)
	PORT_BIT( 0x10, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"3  #  \u3042  \u3041")  PORT_CODE(KEYCODE_3)          PORT_CHAR('3') PORT_CHAR('#')       // 3 # あ ぁ (a)
	PORT_BIT( 0x20, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"4  $  \u3046  \u3045")  PORT_CODE(KEYCODE_4)          PORT_CHAR('4') PORT_CHAR('$')       // 4 $ う ぅ (u)
	PORT_BIT( 0x40, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"5  %  \u3048  \u3047")  PORT_CODE(KEYCODE_5)          PORT_CHAR('5') PORT_CHAR('%')       // 5 % え ぇ (e)
	PORT_BIT( 0x80, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"6  &  \u304a  \u3049")  PORT_CODE(KEYCODE_6)          PORT_CHAR('6') PORT_CHAR('&')       // 6 & お ぉ (o)

	PORT_START("LINE1")
	PORT_BIT( 0x01, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"7  \'  \u3084  \u3083") PORT_CODE(KEYCODE_7)          PORT_CHAR('7') PORT_CHAR('\'')      // 7 ' や ゃ (ya)
	PORT_BIT( 0x02, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"8  (  \u3086  \u3085")  PORT_CODE(KEYCODE_8)          PORT_CHAR('8') PORT_CHAR('(')       // 8 ( ゆ ゅ (yu)
	PORT_BIT( 0x04, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"9  )  \u3088  \u3087")  PORT_CODE(KEYCODE_9)          PORT_CHAR('9') PORT_CHAR(')')       // 9 ) よ ょ (yo)
	PORT_BIT( 0x08, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"0  \u308f  \u3092")     PORT_CODE(KEYCODE_0)          PORT_CHAR('0')                      // 0 わ を (wa wo)
	PORT_BIT( 0x10, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"-  =  \u307b")          PORT_CODE(KEYCODE_MINUS)      PORT_CHAR('-') PORT_CHAR('=')       // - = ほ (ho)
	PORT_BIT( 0x20, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"^  \u3078")             PORT_CODE(KEYCODE_EQUALS)     PORT_CHAR('^')                      // ^ へ (he)
	PORT_BIT( 0x40, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"¥  |  \u30fc")          PORT_CODE(KEYCODE_TILDE)      PORT_CHAR('\\') PORT_CHAR('|')      // Yen | ー (sound extension)
	PORT_BIT( 0x80, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"BS")                    PORT_CODE(KEYCODE_BACKSPACE)  PORT_CHAR(8)                        // BS (backspace)

	PORT_START("LINE2")
	PORT_BIT( 0x01, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"TAB")                   PORT_CODE(KEYCODE_TAB)        PORT_CHAR(9)                        // TAB
	PORT_BIT( 0x02, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Q  \u305f")             PORT_CODE(KEYCODE_Q)          PORT_CHAR('q') PORT_CHAR('Q')       // Q た (ta)
	PORT_BIT( 0x04, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"W  \u3066")             PORT_CODE(KEYCODE_W)          PORT_CHAR('w') PORT_CHAR('W')       // W て (te)
	PORT_BIT( 0x08, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"E  \u3044  \u3043")     PORT_CODE(KEYCODE_E)          PORT_CHAR('e') PORT_CHAR('E')       // E い ぃ (i)
	PORT_BIT( 0x10, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"R  \u3059")             PORT_CODE(KEYCODE_R)          PORT_CHAR('r') PORT_CHAR('R')       // R す (su)
	PORT_BIT( 0x20, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"T  \u304b")             PORT_CODE(KEYCODE_T)          PORT_CHAR('t') PORT_CHAR('T')       // T か (ka)
	PORT_BIT( 0x40, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Y  \u3093")             PORT_CODE(KEYCODE_Y)          PORT_CHAR('y') PORT_CHAR('Y')       // Y ん (n)
	PORT_BIT( 0x80, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"U  \u306a")             PORT_CODE(KEYCODE_U)          PORT_CHAR('u') PORT_CHAR('U')       // U な (na)

	PORT_START("LINE3")
	PORT_BIT( 0x01, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"I  \u306b")             PORT_CODE(KEYCODE_I)          PORT_CHAR('i') PORT_CHAR('I')       // I に (ni)
	PORT_BIT( 0x02, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"O  \u3089")             PORT_CODE(KEYCODE_O)          PORT_CHAR('o') PORT_CHAR('O')       // O ら (ra)
	PORT_BIT( 0x04, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"P  \u305b")             PORT_CODE(KEYCODE_P)          PORT_CHAR('p') PORT_CHAR('P')       // P せ (se)
	PORT_BIT( 0x08, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"@  `  \u309b")          PORT_CODE(KEYCODE_OPENBRACE)  PORT_CHAR('@') PORT_CHAR('`')       // @ ` ゛ (dakuten)
	PORT_BIT( 0x10, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"[  {  \u309c \u300c")   PORT_CODE(KEYCODE_CLOSEBRACE) PORT_CHAR('[') PORT_CHAR('{')       // [ { ゜ 「 (handakuten kagikakko)
	PORT_BIT( 0x20, IP_ACTIVE_HIGH, IPT_KEYBOARD )                                       PORT_CODE(KEYCODE_ENTER)      PORT_CHAR(13)                       // Return
	PORT_BIT( 0x40, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"A  \u3061")             PORT_CODE(KEYCODE_A)          PORT_CHAR('a') PORT_CHAR('A')       // A ち (chi)
	PORT_BIT( 0x80, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"S  \u3068")             PORT_CODE(KEYCODE_S)          PORT_CHAR('s') PORT_CHAR('S')       // S と (to)

	PORT_START("LINE4")
	PORT_BIT( 0x01, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"D  \u3057")             PORT_CODE(KEYCODE_D)          PORT_CHAR('d') PORT_CHAR('D')       // D し (shi)
	PORT_BIT( 0x02, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"F  \u306f")             PORT_CODE(KEYCODE_F)          PORT_CHAR('f') PORT_CHAR('F')       // F は (ha)
	PORT_BIT( 0x04, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"G  \u304d")             PORT_CODE(KEYCODE_G)          PORT_CHAR('g') PORT_CHAR('G')       // G き (ki)
	PORT_BIT( 0x08, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"H  \u304f")             PORT_CODE(KEYCODE_H)          PORT_CHAR('h') PORT_CHAR('H')       // H く (ku)
	PORT_BIT( 0x10, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"J  \u307e")             PORT_CODE(KEYCODE_J)          PORT_CHAR('j') PORT_CHAR('J')       // J ま (ma)
	PORT_BIT( 0x20, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"K  \u306e")             PORT_CODE(KEYCODE_K)          PORT_CHAR('k') PORT_CHAR('K')       // K の (no)
	PORT_BIT( 0x40, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"L  \u308a")             PORT_CODE(KEYCODE_L)          PORT_CHAR('l') PORT_CHAR('L')       // L り (ri)
	PORT_BIT( 0x80, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8";  +  \u308c")          PORT_CODE(KEYCODE_COLON)      PORT_CHAR(';') PORT_CHAR('+')       // ; + れ (re)

	PORT_START("LINE5")
	PORT_BIT( 0x01, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8":  *  \u3051")          PORT_CODE(KEYCODE_QUOTE)      PORT_CHAR(':') PORT_CHAR('*')       // : *　け (ke)
	PORT_BIT( 0x02, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"]  }  \u3080  \u300d")  PORT_CODE(KEYCODE_BACKSLASH)  PORT_CHAR(']') PORT_CHAR('}')       // ] } む 」 (mu kagikakko)
	PORT_BIT( 0x04, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Z  \u3064  \u3063")     PORT_CODE(KEYCODE_Z)          PORT_CHAR('z') PORT_CHAR('Z')       // Z つ っ (tsu)
	PORT_BIT( 0x08, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"X  \u3055")             PORT_CODE(KEYCODE_X)          PORT_CHAR('x') PORT_CHAR('X')       // X さ (sa)
	PORT_BIT( 0x10, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"C  \u305d")             PORT_CODE(KEYCODE_C)          PORT_CHAR('c') PORT_CHAR('C')       // C そ (so)
	PORT_BIT( 0x20, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"V  \u3072")             PORT_CODE(KEYCODE_V)          PORT_CHAR('v') PORT_CHAR('V')       // V ひ (hi)
	PORT_BIT( 0x40, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"B  \u3053")             PORT_CODE(KEYCODE_B)          PORT_CHAR('b') PORT_CHAR('B')       // B こ (ko)
	PORT_BIT( 0x80, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"N  \u307f")             PORT_CODE(KEYCODE_N)          PORT_CHAR('n') PORT_CHAR('N')       // N み (mi)

	PORT_START("LINE6")
	PORT_BIT( 0x01, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"M  \u3082")             PORT_CODE(KEYCODE_M)          PORT_CHAR('m') PORT_CHAR('M')       // M も (mo)
	PORT_BIT( 0x02, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8",  <  \u306d  \u3001")  PORT_CODE(KEYCODE_COMMA)      PORT_CHAR(',') PORT_CHAR('<')       // , < ね 、 (ne comma)
	PORT_BIT( 0x04, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8".  >  \u308b  \u3002")  PORT_CODE(KEYCODE_STOP)       PORT_CHAR('.') PORT_CHAR('>')       // . > る 。 (ru stop)
	PORT_BIT( 0x08, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"/  ?  \u3081  \u30fb")  PORT_CODE(KEYCODE_SLASH)      PORT_CHAR('/') PORT_CHAR('?')       // / ? め ・ (me interpunct)
	PORT_BIT( 0x10, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"_  \u308d")                                           PORT_CHAR('_')                      // Underscore (shifted only?) ろ (ro)
	PORT_BIT( 0x20, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Space")                 PORT_CODE(KEYCODE_SPACE)      PORT_CHAR(' ')                      // Space
	PORT_BIT( 0x40, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"HOME")                  PORT_CODE(KEYCODE_HOME)       PORT_CHAR(UCHAR_MAMEKEY(HOME))      // HOME
	PORT_BIT( 0x80, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"DEL")                   PORT_CODE(KEYCODE_DEL)        PORT_CHAR(UCHAR_MAMEKEY(DEL))       // DEL

	PORT_START("LINE7")
	PORT_BIT( 0x01, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"ROLL UP")               PORT_CODE(KEYCODE_PGUP)       PORT_CHAR(UCHAR_MAMEKEY(PGUP))      // ROLL UP
	PORT_BIT( 0x02, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"ROLL DOWN")             PORT_CODE(KEYCODE_PGDN)       PORT_CHAR(UCHAR_MAMEKEY(PGDN))      // ROLL DOWN
	PORT_BIT( 0x04, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"UNDO")                  PORT_CODE(KEYCODE_END)        PORT_CHAR(UCHAR_MAMEKEY(END))       // UNDO
	PORT_BIT( 0x08, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Cursor Left")           PORT_CODE(KEYCODE_LEFT)       PORT_CHAR(UCHAR_MAMEKEY(LEFT))      // Left
	PORT_BIT( 0x10, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Cursor Up")             PORT_CODE(KEYCODE_UP)         PORT_CHAR(UCHAR_MAMEKEY(UP))        // Up
	PORT_BIT( 0x20, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Cursor Right")          PORT_CODE(KEYCODE_RIGHT)      PORT_CHAR(UCHAR_MAMEKEY(RIGHT))     // Right
	PORT_BIT( 0x40, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Cursor Down")           PORT_CODE(KEYCODE_DOWN)       PORT_CHAR(UCHAR_MAMEKEY(DOWN))      // Down
	PORT_BIT( 0x80, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey CLR")            PORT_CODE(KEYCODE_NUMLOCK)    PORT_CHAR(UCHAR_MAMEKEY(NUMLOCK))   // CLR (clear)

	PORT_START("LINE8")
	PORT_BIT( 0x01, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey /")              PORT_CODE(KEYCODE_SLASH_PAD)  PORT_CHAR(UCHAR_MAMEKEY(SLASH_PAD)) // / (numpad)
	PORT_BIT( 0x02, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey *")              PORT_CODE(KEYCODE_ASTERISK)   PORT_CHAR(UCHAR_MAMEKEY(ASTERISK))  // * (numpad)
	PORT_BIT( 0x04, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey -")              PORT_CODE(KEYCODE_MINUS_PAD)  PORT_CHAR(UCHAR_MAMEKEY(MINUS_PAD)) // - (numpad)
	PORT_BIT( 0x08, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey 7")              PORT_CODE(KEYCODE_7_PAD)      PORT_CHAR(UCHAR_MAMEKEY(7_PAD))     // 7 (numpad)
	PORT_BIT( 0x10, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey 8")              PORT_CODE(KEYCODE_8_PAD)      PORT_CHAR(UCHAR_MAMEKEY(8_PAD))     // 8 (numpad)
	PORT_BIT( 0x20, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey 9")              PORT_CODE(KEYCODE_9_PAD)      PORT_CHAR(UCHAR_MAMEKEY(9_PAD))     // 9 (numpad)
	PORT_BIT( 0x40, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey +")              PORT_CODE(KEYCODE_PLUS_PAD)   PORT_CHAR(UCHAR_MAMEKEY(PLUS_PAD))  // + (numpad)
	PORT_BIT( 0x80, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey 4")              PORT_CODE(KEYCODE_4_PAD)      PORT_CHAR(UCHAR_MAMEKEY(4_PAD))     // 4 (numpad)

	PORT_START("LINE9")
	PORT_BIT( 0x01, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey 5")              PORT_CODE(KEYCODE_5_PAD)      PORT_CHAR(UCHAR_MAMEKEY(5_PAD))     // 5 (numpad)
	PORT_BIT( 0x02, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey 6")              PORT_CODE(KEYCODE_6_PAD)      PORT_CHAR(UCHAR_MAMEKEY(6_PAD))     // 6 (numpad)
	PORT_BIT( 0x04, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey =")                                                                                // = (numpad)
	PORT_BIT( 0x08, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey 1")              PORT_CODE(KEYCODE_1_PAD)      PORT_CHAR(UCHAR_MAMEKEY(1_PAD))     // 1 (numpad)
	PORT_BIT( 0x10, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey 2")              PORT_CODE(KEYCODE_2_PAD)      PORT_CHAR(UCHAR_MAMEKEY(2_PAD))     // 2 (numpad)
	PORT_BIT( 0x20, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey 3")              PORT_CODE(KEYCODE_3_PAD)      PORT_CHAR(UCHAR_MAMEKEY(3_PAD))     // 3 (numpad)
	PORT_BIT( 0x40, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey ENTER")          PORT_CODE(KEYCODE_ENTER_PAD)  PORT_CHAR(UCHAR_MAMEKEY(ENTER_PAD)) // Enter (numpad)
	PORT_BIT( 0x80, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey 0")              PORT_CODE(KEYCODE_0_PAD)      PORT_CHAR(UCHAR_MAMEKEY(0_PAD))     // 0 (numpad)

	PORT_START("LINEA")
	PORT_BIT( 0x01, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey ,")                                                                                // , (numpad)
	PORT_BIT( 0x02, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Tenkey .")              PORT_CODE(KEYCODE_DEL_PAD)    PORT_CHAR(UCHAR_MAMEKEY(DEL_PAD))   // . (numpad)
	PORT_BIT( 0x04, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"\u8a18\u53f7\u5165\u529b (Symbol input)")                                                 // 記号入力 (Kigou nyuuryoku - Symbol input)
	PORT_BIT( 0x08, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"\u767b\u9332 (Register)")                                                                 // 登録 (Touroku - Register)
	PORT_BIT( 0x10, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Help")                                                                                    // Help
	PORT_BIT( 0x20, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"XF1")                   PORT_CODE(KEYCODE_F11)        PORT_CHAR(UCHAR_MAMEKEY(F11))       // XF1
	PORT_BIT( 0x40, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"XF2")                   PORT_CODE(KEYCODE_F12)        PORT_CHAR(UCHAR_MAMEKEY(F12))       // XF2
	PORT_BIT( 0x80, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"XF3")                                                                                     // XF3

	PORT_START("LINEB")
	PORT_BIT( 0x01, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"XF4")                                                                                     // XF4
	PORT_BIT( 0x02, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"XF5")                   PORT_CODE(KEYCODE_RALT)                                           // XF5
	PORT_BIT( 0x04, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"\u304b\u306a (Kana)")                                                                     // かな (Kana)
	PORT_BIT( 0x08, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"\u30ed\u30fc\u30de\u5b57 (Romaji)")                                                       // ローマ字 (Romaji)
	PORT_BIT( 0x10, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"\u30b3\u30fc\u30c9\u5165\u529b (Code input)")                                             // コード入力 (Code nyuuryoku - Code input)
	PORT_BIT( 0x20, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"CAPS")                  PORT_CODE(KEYCODE_CAPSLOCK)  PORT_CHAR(UCHAR_MAMEKEY(CAPSLOCK))   // CAPS (Caps lock)
	PORT_BIT( 0x40, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"INS")                   PORT_CODE(KEYCODE_INSERT)    PORT_CHAR(UCHAR_MAMEKEY(INSERT))     // INS (Insert)
	PORT_BIT( 0x80, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"\u3072\u3089\u304c\u306a (Hiragana)") PORT_CODE(KEYCODE_LALT)                             // ひらがな (Hiragana)

	PORT_START("LINEC")
	PORT_BIT( 0x01, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"\u5168\u89d2 (Fullwidth)") PORT_CODE(KEYCODE_RCONTROL)                                    // 全角 (Zenkaku - Fullwidth)
	PORT_BIT( 0x02, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Break")                                                                                   // Break
	PORT_BIT( 0x04, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"Copy")                                                                                    // Copy
	PORT_BIT( 0x08, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"F1")                    PORT_CODE(KEYCODE_F1)         PORT_CHAR(UCHAR_MAMEKEY(F1))        // F1
	PORT_BIT( 0x10, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"F2")                    PORT_CODE(KEYCODE_F2)         PORT_CHAR(UCHAR_MAMEKEY(F2))        // F2
	PORT_BIT( 0x20, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"F3")                    PORT_CODE(KEYCODE_F3)         PORT_CHAR(UCHAR_MAMEKEY(F3))        // F3
	PORT_BIT( 0x40, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"F4")                    PORT_CODE(KEYCODE_F4)         PORT_CHAR(UCHAR_MAMEKEY(F4))        // F4
	PORT_BIT( 0x80, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"F5")                    PORT_CODE(KEYCODE_F5)         PORT_CHAR(UCHAR_MAMEKEY(F5))        // F5

	PORT_START("LINED")
	PORT_BIT( 0x01, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"F6")                    PORT_CODE(KEYCODE_F6)         PORT_CHAR(UCHAR_MAMEKEY(F6))        // F6
	PORT_BIT( 0x02, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"F7")                    PORT_CODE(KEYCODE_F7)         PORT_CHAR(UCHAR_MAMEKEY(F7))        // F7
	PORT_BIT( 0x04, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"F8")                    PORT_CODE(KEYCODE_F8)         PORT_CHAR(UCHAR_MAMEKEY(F8))        // F8
	PORT_BIT( 0x08, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"F9")                    PORT_CODE(KEYCODE_F9)         PORT_CHAR(UCHAR_MAMEKEY(F9))        // F9
	PORT_BIT( 0x10, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"F10")                   PORT_CODE(KEYCODE_F10)        PORT_CHAR(UCHAR_MAMEKEY(F10))       // F10
	// 0x6d reserved
	// 0x6e reserved
	// 0x6f reserved

	PORT_START("LINEE")
	PORT_BIT( 0x01, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"SHIFT")  PORT_CODE(KEYCODE_LSHIFT) PORT_CODE(KEYCODE_RSHIFT)  PORT_CHAR(UCHAR_SHIFT_1)    // Shift
	PORT_BIT( 0x02, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"CTRL")                  PORT_CODE(KEYCODE_LCONTROL)                                       // Control
	PORT_BIT( 0x04, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"OPT.1")                 PORT_CODE(KEYCODE_PRTSCR)                                         // Opt1
	PORT_BIT( 0x08, IP_ACTIVE_HIGH, IPT_KEYBOARD )  PORT_NAME(u8"OPT.2")                 PORT_CODE(KEYCODE_PAUSE)                                          // Opt2
INPUT_PORTS_END

ioport_constructor x68k_keyboard_device::device_input_ports() const
{
	return INPUT_PORTS_NAME(x68k_keyboard);
}

void x68k_keyboard_device::device_start()
{
	buffered_rs232_device::device_start();

	m_led_kana.resolve();
	m_led_romaji.resolve();
	m_led_code.resolve();
	m_led_caps.resolve();
	m_led_insert.resolve();
	m_led_hiragana.resolve();
	m_led_fullsize.resolve();

	save_item(NAME(m_delay));
	save_item(NAME(m_repeat));
	save_item(NAME(m_enabled));
}

void x68k_keyboard_device::device_reset()
{
	buffered_rs232_device::device_reset();

	set_data_frame(1, 8, PARITY_NONE, STOP_BITS_1);
	set_rate(2400);
	receive_register_reset();
	transmit_register_reset();

	m_enabled = 0;
	m_delay = 500;  // 3*100+200
	m_repeat = 110;  // 4^2*5+30

	stop_processing();
	reset_key_state();
	typematic_stop();
	clear_fifo();

	output_dcd(0);
	output_dsr(0);
	output_cts(0);
	output_rxd(1);

	m_led_kana = 1;
	m_led_romaji = 1;
	m_led_code = 1;
	m_led_caps = 1;
	m_led_insert = 1;
	m_led_hiragana = 1;
	m_led_fullsize = 1;
}


DEFINE_DEVICE_TYPE(X68K_KEYBOARD, x68k_keyboard_device, "x68k_keyboard", "Sharp X68000 Keyboard")

#if 0

void x68k_state::x68k_keyboard_push_scancode(unsigned char code)
{
	m_keynum++;
	if(m_keynum >= 1)
	{
		// keyboard buffer full
		if(m_enabled != 0)
		{
			//m_mfp.rsr |= 0x80;  // Buffer full
			//if(ioport("options")->read() & 0x01)
			//{
			//  m_current_vector[6] = 0x4c;
			//  m_maincpu->set_input_line(6,ASSERT_LINE);
			//  logerror("MFP: Receive buffer full IRQ sent\n");
			//}
		}
	}
	m_buffer[m_headpos++] = code;
	if(m_headpos > 15)
	{
		m_headpos = 0;
		m_current_vector[6] = 0x4b;
	}
}

TIMER_CALLBACK_MEMBER(x68k_state::x68k_keyboard_poll)
{
	int x;
	static const char *const keynames[] = { "key1", "key2", "key3", "key4" };

	for(x=0;x<0x80;x++)
	{
		// adjust delay/repeat timers
		if(m_keytime[x] > 0)
		{
			m_keytime[x] -= 5;
		}
		if(!(ioport(keynames[x / 32])->read() & (1 << (x % 32))))
		{
			if(m_keyon[x] != 0)
			{
				x68k_keyboard_push_scancode(0x80 + x);
				m_keytime[x] = 0;
				m_keyon[x] = 0;
				m_last_pressed = 0;
				logerror("KB: Released key 0x%02x\n",x);
			}
		}
		// check to see if a key is being held
		if(m_keyon[x] != 0 && m_keytime[x] == 0 && m_last_pressed == x)
		{
			if(ioport(keynames[m_last_pressed / 32])->read() & (1 << (m_last_pressed % 32)))
			{
				x68k_keyboard_push_scancode(m_last_pressed);
				m_keytime[m_last_pressed] = (m_repeat^2)*5+30;
				logerror("KB: Holding key 0x%02x\n",m_last_pressed);
			}
		}
		if((ioport(keynames[x / 32])->read() & (1 << (x % 32))))
		{
			if(m_keyon[x] == 0)
			{
				x68k_keyboard_push_scancode(x);
				m_keytime[x] = m_delay * 100 + 200;
				m_keyon[x] = 1;
				m_last_pressed = x;
				logerror("KB: Pushed key 0x%02x\n",x);
			}
		}
	}
}

	struct
	{
		unsigned char led_status;  // keyboard LED status
		unsigned char buffer[16];
		int headpos;  // scancodes are added here
		int tailpos;  // scancodes are read from here
		int keynum;  // number of scancodes in buffer
		int keytime[0x80];  // time until next keypress
		int keyon[0x80];  // is 1 if key is pressed, used to determine if the key state has changed from 1 to 0
		int last_pressed;  // last key pressed, for repeat key handling
	} m_keyboard;
	TIMER_CALLBACK_MEMBER(x68k_led_callback);
	TIMER_CALLBACK_MEMBER(x68k_keyboard_poll);
	void x68k_keyboard_ctrl_w(int data);
	int x68k_keyboard_pop_scancode();
	void x68k_keyboard_push_scancode(unsigned char code);

#endif
