/*
 * $Id$
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
package VASSAL.launch;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.IOException;

import javax.swing.JFrame;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.tools.ArchiveWriter;

public class CreateModuleAction extends GameModuleAction {
  private static final long serialVersionUID = 1L;

  public CreateModuleAction(Component comp) {
    super(Resources.getString("Main.new_module"), comp);
  }

  public void performAction(ActionEvent e) throws IOException {
    GameModule.init(new BasicModule(new ArchiveWriter((String) null)));
    JFrame frame = GameModule.getGameModule().getFrame();
    frame.setVisible(true);
    ModuleEditorWindow w = new ModuleEditorWindow(GameModule.getGameModule());
    w.setLocation(0, frame.getY() + frame.getHeight());
    w.setSize(Info.getScreenBounds(frame).width/2,w.getHeight());
    w.setVisible(true);
  }
}
