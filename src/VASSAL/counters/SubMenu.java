/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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
package VASSAL.counters;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.JPanel;
import javax.swing.KeyStroke;

import net.miginfocom.swing.MigLayout;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.SequenceEncoder;

/** A trait that groups menu items of other traits into a sub-menu */
public class SubMenu extends Decorator implements TranslatablePiece {
  public static final String ID = "submenu;";
  private String subMenu;
  private KeyCommandSubMenu keyCommandSubMenu;
  private final KeyCommand[] keyCommands = new KeyCommand[1];

  public SubMenu() {
    this(ID+"Sub-Menu;",null);
  }

  public SubMenu(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public String getDescription() {
    if ("Sub-Menu".equals(getMenuName())) {
      return "Sub-Menu";
    }
    else {
      return "Sub-Menu:  "+getMenuName();
    }
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("SubMenu.htm");
  }

  public PieceEditor getEditor() {
    return new Editor(this);
  }

  public void mySetType(String type) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type,';');
    st.nextToken();
    subMenu = st.nextToken();
    keyCommandSubMenu = new KeyCommandSubMenu(subMenu, this, this);
    keyCommandSubMenu.setCommands(
      StringArrayConfigurer.stringToArray(st.nextToken()));

    keyCommands[0] = keyCommandSubMenu;
  }

  protected KeyCommand[] myGetKeyCommands() {
    return keyCommands;
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(getMenuName()).append(
      StringArrayConfigurer.arrayToString(getSubcommands()));
    return ID+se.getValue();
  }

  public String[] getSubcommands() {
    final ArrayList<String> l = new ArrayList<String>();
    for (Iterator<String> i = keyCommandSubMenu.getCommands(); i.hasNext(); ) {
      l.add(i.next());
    }
    return l.toArray(new String[l.size()]);
  }

  public String getMenuName() {
    return subMenu;
  }

  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  public void mySetState(String newState) {
  }

  public Rectangle boundingBox() {
    return getInner().boundingBox();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    getInner().draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return getInner().getName();
  }

  public Shape getShape() {
    return getInner().getShape();
  }

  public PieceI18nData getI18nData() {
    return getI18nData(getMenuName(), "Sub Menu Name");
  }

  public static class Editor implements PieceEditor {
    private StringConfigurer nameConfig;
    private StringArrayConfigurer commandsConfig;
    private JPanel panel = new JPanel();

    public Editor(SubMenu p) {
      nameConfig = new StringConfigurer(null, "Menu name:  ", p.getMenuName());
      commandsConfig = new StringArrayConfigurer(
        null, "Sub-commands", p.getSubcommands()
      );

      panel.setLayout(new MigLayout("fill", "[]rel[]"));
      panel.add(nameConfig.getControls(), "growx,wrap");
      panel.add(commandsConfig.getControls(), "grow,push");
    }

    public Component getControls() {
      return panel;
    }

    public String getState() {
      return "";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(nameConfig.getValueString()).append(commandsConfig.getValueString());
      return ID+se.getValue();
    }
  }
}
