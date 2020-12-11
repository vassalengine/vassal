/*
 * Copyright (c) 2020 by The VASSAL Development Team
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.configure;

import VASSAL.i18n.Resources;
import VASSAL.tools.icon.IconFactory;
import java.awt.Font;
import java.awt.Insets;
import javax.swing.JButton;

/**
 * A button with no insets to be as small as possible
 */
public class NoInsetButton extends JButton {
  private static final long serialVersionUID = 1L;
  final Insets NO_INSETS = new Insets(0, 0, 0, 0);
  final Font ITALIC = new Font(Font.DIALOG, Font.ITALIC, 12);

  public NoInsetButton(String icon, int size) {
    this(icon, size, null);
  }

  public NoInsetButton(String icon, int size, String toolTipKey) {
    super(IconFactory.getIcon(icon, size));
    setFont(ITALIC);
    setMargin(NO_INSETS);
    if (toolTipKey != null && !toolTipKey.isEmpty()) {
      setToolTipText(Resources.getString(toolTipKey));
    }
  }
}
