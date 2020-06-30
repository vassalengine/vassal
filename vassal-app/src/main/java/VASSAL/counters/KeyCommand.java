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
import VASSAL.i18n.Localization;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;

public class KeyCommand extends AbstractAction {
  private static final long serialVersionUID = 1L;

  private String name;
  protected String untranslatedName;
  protected String localizedMenuText;
  private KeyStroke stroke;
  private GamePiece target;
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
    super(key == null ? name : name + "  " + NamedHotKeyConfigurer.getString(key));
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
    this(name, key == null ? null : key.getKeyStroke(), target, enabled);
    namedKeyStroke = key == null ? NamedKeyStroke.NULL_KEYSTROKE : key;
  }

  public KeyCommand(String name, KeyStroke key, GamePiece target, boolean enabled) {
    this(name, key, target, null);
    setEnabled(enabled);
  }

  public KeyCommand(KeyCommand command) {
    this(command.name, command.stroke, command.target, command.isEnabled());
    this.i18nPiece = command.i18nPiece;
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
   * @return
   */
  public boolean isGlobal() {
    return global;
  }

  /**
   * If true, then this action will apply to all selected pieces
   * @param global
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
        BoundsTracker t = new BoundsTracker();
        GamePiece outer = Decorator.getOutermost(target);
        t.addPiece(outer);
        outer.setProperty(Properties.SNAPSHOT, PieceCloner.getInstance().clonePiece(outer)); // save state prior to command
        Command c = outer.keyEvent(stroke);
        if (target.getId() != null) {
          GameModule.getGameModule().sendAndLog(c);
        }
        t.addPiece(outer);
        t.repaint();
      }
    }
  }

  /**
   * The human-readable text that will appear in the right-click menu, translated to the user's Locale
   * @return
   */
  public String getLocalizedMenuText() {
    if (localizedMenuText == null && name != null) {
      String localizedName = name;
      if (i18nPiece != null && GameModule.getGameModule().isLocalizationEnabled()) {
        String key = null;
        for (PieceI18nData.Property p : i18nPiece.getI18nData().getProperties()) {
          if (p.getName().equals(name)) {
            key = TranslatablePiece.PREFIX + p.getName();
          }
        }
        if (key != null) {
          localizedName = Localization.getInstance().translate(key, name);
        }
      }
      localizedMenuText = stroke == null ? localizedName : localizedName + "  " + NamedHotKeyConfigurer.getString(stroke);
    }
    return localizedMenuText;
  }
}


