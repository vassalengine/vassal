/*
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
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
package VASSAL.launch.install;

import java.awt.Component;
import javax.swing.JLabel;

/**
 * @author rkinney
 */
public class SuccessScreen implements Screen {
  private JLabel label;
  public SuccessScreen(String msg) {
    label = new JLabel(msg);
  }
  public Component getControls() {
    return label;
  }
  public void next(InstallWizard w) {
    System.exit(0);
  }
}