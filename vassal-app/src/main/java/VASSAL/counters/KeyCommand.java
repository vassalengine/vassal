/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;

public class KeyCommand extends AbstractAction {
  private static final long serialVersionUID = 1L;

  public static final KeyCommand[] NONE = new KeyCommand[0];

  private final String name;
  protected String untranslatedName;
  protected String localizedMenuText;
  private final KeyStroke stroke;
  private final GamePiece target;
  private boolean global;
  private boolean enabled = true;

  protected TranslatablePiece i18nPiece;
  protected NamedKeyStroke namedKeyStroke;

  public KeyCommand(String name, KeyStroke key, GamePiece target) {
    this(name, key, target, null);
  }

  public KeyCommand(String name, NamedKeyStroke key, GamePiece target) {
    this(name, key, target, null);
  }

  public KeyCommand(String name, KeyStroke key, GamePiece target, TranslatablePiece i18nPiece) {
    super(makeMenuText(key, name));
    this.target = target;
    this.name = name;
    this.stroke = key;
    this.i18nPiece = i18nPiece;
  }

  public KeyCommand(String name, NamedKeyStroke key, GamePiece target, TranslatablePiece i18nPiece) {
    this(name, key == null ? null : key.getKeyStroke(), target, i18nPiece);
    namedKeyStroke = key == null ? NamedKeyStroke.NULL_KEYSTROKE : key;
  }

  public KeyCommand(String name, NamedKeyStroke key, GamePiece target, boolean enabled) {
    this(name, key, target, null, enabled);
  }

  public KeyCommand(String name, NamedKeyStroke key, GamePiece target, TranslatablePiece i18nPiece, boolean enabled) {
    this(name, key == null ? null : key.getKeyStroke(), target, i18nPiece, enabled);
    namedKeyStroke = key == null ? NamedKeyStroke.NULL_KEYSTROKE : key;
  }

  public KeyCommand(String name, KeyStroke key, GamePiece target, boolean enabled) {
    this(name, key, target, null, enabled);
  }

  public KeyCommand(String name, KeyStroke key, GamePiece target, TranslatablePiece i18nPiece, boolean enabled) {
    this(name, key, target, i18nPiece);
    setEnabled(enabled);
  }

  public KeyCommand(KeyCommand command) {
    this(command.name, command.stroke, command.target, command.i18nPiece, command.isEnabled());
  }

  public String getName() {
    return name;
  }

  public boolean matches(KeyStroke key) {
    return isEnabled() && key != null && key.equals(stroke);
  }

  public KeyStroke getKeyStroke() {
    return stroke;
  }

  public NamedKeyStroke getNamedKeyStroke() {
    return namedKeyStroke;
  }

  public GamePiece getTarget() {
    return target;
  }

  @Override
  public boolean isEnabled() {
    return enabled;
  }

  @Override
  public void setEnabled(boolean b) {
    enabled = b;
  }

  /**
   * If true, then this action will apply to all selected pieces
   * @return global
   */
  public boolean isGlobal() {
    return global;
  }

  /**
   * If true, then this action will apply to all selected pieces
   * @param global value to set
   */
  public void setGlobal(boolean global) {
    this.global = global;
  }

  @Override
  public void actionPerformed(ActionEvent evt) {
    if (stroke != null) {
      if (global) {
        GameModule.getGameModule().sendAndLog(KeyBuffer.getBuffer().keyCommand(stroke));
      }
      else {
        final BoundsTracker t = new BoundsTracker();
        final GamePiece outer = Decorator.getOutermost(target);
        t.addPiece(outer);
        outer.setProperty(Properties.SNAPSHOT, ((PropertyExporter) outer).getProperties()); // save state prior to command
        final Command c = outer.keyEvent(stroke);
        if (target.getId() != null) {
          GameModule.getGameModule().sendAndLog(c);
        }
        t.addPiece(outer);
        t.repaint();
      }
    }
  }
  
  // Returns true if this command exists simply to produce a menu separator
  public boolean isMenuSeparator() {
    return MenuSeparator.SEPARATOR_NAME.equals(name);
  }

  /**
   * The human-readable text that will appear in the right-click menu, translated to the user's Locale
   * @return Localized text
   */
  public String getLocalizedMenuText() {
    if (localizedMenuText == null && name != null) {
      String localizedName = name;
      if (i18nPiece != null && GameModule.getGameModule().isLocalizationEnabled()) {
        localizedName = i18nPiece.getI18nData().translate(name);
      }
      localizedMenuText = makeMenuText(stroke, localizedName);
    }
    return localizedMenuText;
  }

  private static String makeMenuText(KeyStroke ks, String text) {
    return (ks != null && text != null && !text.isBlank() ?
      text + "  " + NamedHotKeyConfigurer.getString(ks) : text).intern();
  }
}
