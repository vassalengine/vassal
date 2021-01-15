/*
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

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

import javax.swing.KeyStroke;

/** A trait that groups menu items of other traits into a sub-menu */
public class SubMenu extends Decorator implements TranslatablePiece {
  public static final String ID = "submenu;"; // NON-NLS
  private String subMenu;
  private KeyCommandSubMenu keyCommandSubMenu;
  private final KeyCommand[] keyCommands = new KeyCommand[1];
  private static final String DEFAULT_MENU_NAME = Resources.getString("Editor.SubMenu.default_menu_name");
  private String description = "";

  public SubMenu() {
    this(ID + Resources.getString("Editor.SubMenu.default_menu_name") + ";", null);
  }

  public SubMenu(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.SubMenu.trait_description", DEFAULT_MENU_NAME.equals(subMenu) ? "" : subMenu, description);
  }

  public void setDescription(String description) {
    this.description = description;
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("SubMenu.html"); // NON-NLS
  }

  @Override
  public PieceEditor getEditor() {
    return new Editor(this);
  }

  @Override
  public void mySetType(String type) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    subMenu = st.nextToken();
    keyCommandSubMenu = new KeyCommandSubMenu(subMenu, this, this);
    keyCommandSubMenu.setCommands(StringArrayConfigurer.stringToArray(st.nextToken()));
    description = st.nextToken("");

    keyCommands[0] = keyCommandSubMenu;
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return keyCommands;
  }

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(getMenuName())
      .append(StringArrayConfigurer.arrayToString(getSubcommands()))
      .append(description);
    return ID + se.getValue();
  }

  public String[] getSubcommands() {
    final ArrayList<String> l = new ArrayList<>();
    for (final Iterator<String> i = keyCommandSubMenu.getCommands(); i.hasNext(); ) {
      l.add(i.next());
    }
    return l.toArray(new String[0]);
  }

  public String getMenuName() {
    return subMenu;
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  @Override
  public void mySetState(String newState) {
  }

  @Override
  public Rectangle boundingBox() {
    return getInner().boundingBox();
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    getInner().draw(g, x, y, obs, zoom);
  }

  @Override
  public String getName() {
    return getInner().getName();
  }

  @Override
  public Shape getShape() {
    return getInner().getShape();
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(getMenuName(), Resources.getString("Editor.SubMenu.sub_menu.name"));
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof SubMenu)) return false;
    final SubMenu c = (SubMenu) o;
    if (! Objects.equals(subMenu, c.subMenu)) return false;
    return Objects.equals(StringArrayConfigurer.arrayToString(getSubcommands()), StringArrayConfigurer.arrayToString(c.getSubcommands()));
  }

  public static class Editor implements PieceEditor {
    private final StringConfigurer nameConfig;
    private final StringArrayConfigurer commandsConfig;
    private final TraitConfigPanel panel = new TraitConfigPanel();
    private final StringConfigurer descConfig;

    public Editor(SubMenu p) {

      descConfig = new StringConfigurer(p.description);
      descConfig.setHintKey("Editor.description_hint");
      panel.add("Editor.description_label", descConfig);

      nameConfig = new StringConfigurer(p.getMenuName());
      nameConfig.setHintKey("Editor.menu_command_hint");
      panel.add("Editor.SubMenu.menu_name", nameConfig);

      commandsConfig = new StringArrayConfigurer(p.getSubcommands());
      commandsConfig.setHintKey("Editor.menu_command_hint");
      panel.add("Editor.SubMenu.sub_commands", commandsConfig);
    }

    @Override
    public Component getControls() {
      return panel;
    }

    @Override
    public String getState() {
      return "";
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(nameConfig.getValueString()).append(commandsConfig.getValueString()).append(descConfig.getValueString());
      return ID + se.getValue();
    }
  }

  /**
   * @return a list of any Menu Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(subMenu);
  }
}
