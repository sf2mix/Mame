// license:BSD-3-Clause
// copyright-holders:Curt Coder, hap
/**********************************************************************

    Mouse emulation (Commodore 1351 or compatible)

**********************************************************************/

#ifndef MAME_BUS_VCS_CTRL_MOUSE_H
#define MAME_BUS_VCS_CTRL_MOUSE_H

#pragma once

#include "ctrl.h"



//**************************************************************************
//  TYPE DEFINITIONS
//**************************************************************************

// ======================> vcs_mouse_device

class vcs_mouse_device : public device_t,
						public device_vcs_control_port_interface
{
public:
	// construction/destruction
	vcs_mouse_device(const machine_config &mconfig, const char *tag, device_t *owner, uint32_t clock);

	// device_vcs_control_port_interface overrides
	virtual uint8_t vcs_joy_r() override;
	virtual uint8_t vcs_pot_x_r() override;
	virtual uint8_t vcs_pot_y_r() override;

	virtual bool has_pot_x() override { return true; }
	virtual bool has_pot_y() override { return true; }

protected:
	// device-level overrides
	virtual void device_start() override;

	// optional information overrides
	virtual ioport_constructor device_input_ports() const override;

private:
	required_ioport m_joy;
	required_ioport m_potx;
	required_ioport m_poty;
};


// device type definition
DECLARE_DEVICE_TYPE(VCS_MOUSE, vcs_mouse_device)

#endif // MAME_BUS_VCS_CTRL_MOUSE_H
