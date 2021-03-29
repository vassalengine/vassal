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
package VASSAL.tools;

import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.configure.Configurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;

/**
 * A JButton for placing into a VASSAL component's toolbar.
 * Handles configuration of a hotkey shortcut, maintains appropriate
 * tooltip text, etc.
 */
public class LaunchButton extends JButton {
  private static final long serialVersionUID = 1L;
  public static final String UNTRANSLATED_TEXT = "unTranslatedText"; //$NON-NLS-1$
  protected String tooltipAtt;
  protected String nameAtt;
  protected String keyAtt;
  protected String iconAtt;
  protected IconConfigurer iconConfig;
  protected String toolTipText;
  protected NamedKeyStrokeListener keyListener;
  protected Configurer nameConfig, keyConfig;

  public LaunchButton(String text, String textAttribute,
                      String hotkeyAttribute, ActionListener al) {
    this(text, textAttribute, hotkeyAttribute, null, al);
  }

  public LaunchButton(String text, String tooltipAttribute,
                      String textAttribute, String hotkeyAttribute,
                      String iconAttribute, final ActionListener al) {
    this(text, textAttribute, hotkeyAttribute, iconAttribute, al);
    tooltipAtt = tooltipAttribute;
  }

  public LaunchButton(String text, String textAttribute, String hotkeyAttribute, String iconAttribute, final ActionListener al) {
    super(text);
    nameAtt = textAttribute;
    keyAtt = hotkeyAttribute;
    iconAtt = iconAttribute;
    iconConfig = new IconConfigurer(iconAtt, null, null);
    setAlignmentY(0.0F);
    keyListener = new NamedKeyStrokeListener(e -> {
      if (isEnabled() && (GlobalOptions.getInstance().isHotKeysOnClosedWindows() || (getParent() != null && getParent().isShowing()))) {
        al.actionPerformed(e);
      }
    });
    if (al != null) {
      GameModule.getGameModule().addKeyStrokeListener(keyListener);
      addActionListener(al);
    }
    setFocusable(false);
    checkVisibility();
  }

  @Override
  public int getBaseline(int width, int height) {
    // Without this, buttons with no text, buttons with single line HTML text,
    // and buttons with multiline HTML text end up having different baselines.
    // LaunchButtons go into toolbars only, so there should be no alignment
    // issues caused by indicating that the baseline is non-applicable.
    return -1;
  }

  public String getNameAttribute() {
    return nameAtt;
  }

  public String getHotkeyAttribute() {
    return keyAtt;
  }

  public String getIconAttribute() {
    return iconAtt;
  }

  public String getAttributeValueString(String key) {
    if (key.equals(nameAtt)) {
      return getText();
    }
    else if (key.equals(keyAtt)) {
      return NamedHotKeyConfigurer.encode(keyListener.getNamedKeyStroke());
    }
    else if (key.equals(iconAtt)) {
      return iconConfig.getValueString();
    }
    else if (key.equals(tooltipAtt)) {
      return toolTipText;
    }
    else {
      return null;
    }
  }

  public void setAttribute(String key, Object value) {
    if (key != null) {
      if (key.equals(nameAtt)) {
        if (Localization.getInstance().isTranslationInProgress()) {
          putClientProperty(UNTRANSLATED_TEXT, getText());
        }
        setText((String) value);
        checkVisibility();
      }
      else if (key.equals(keyAtt)) {
        if (value instanceof String) {
          value = NamedHotKeyConfigurer.decode((String) value);
        }
        if (value instanceof NamedKeyStroke) {
          keyListener.setKeyStroke((NamedKeyStroke) value);
        }
        else {
          keyListener.setKeyStroke((KeyStroke) value); // Compatibility - custom code
        }
        setToolTipText(toolTipText);
      }
      else if (key.equals(tooltipAtt)) {
        toolTipText = (String) value;
        setToolTipText(toolTipText);
      }
      else if (key.equals(iconAtt)) {
        if (value instanceof String) {
          iconConfig.setValue((String) value);
          setIcon(iconConfig.getIconValue());
        }
        checkVisibility();
      }
    }
  }

  @Override
  public void setToolTipText(String text) {
    toolTipText = text;
    if (keyListener.getKeyStroke() != null) {
      text = (text == null ? "" : text + " "); //$NON-NLS-1$ //$NON-NLS-2$
      if (!keyListener.getNamedKeyStroke().isNamed()) {
        text += "[" + NamedHotKeyConfigurer.getString(keyListener.getKeyStroke()) + "]"; //$NON-NLS-1$ //$NON-NLS-2$
      }
    }
    super.setToolTipText(text);
  }

  public Configurer getNameConfigurer() {
    if (nameConfig == null && nameAtt != null) {
      nameConfig = new StringConfigurer(nameAtt, Resources.getString("Editor.button_text_label"), getText()); //$NON-NLS-1$
    }
    return nameConfig;
  }

  public Configurer getHotkeyConfigurer() {
    if (keyConfig == null && keyAtt != null) {
      keyConfig = new NamedHotKeyConfigurer(keyAtt, Resources.getString("Editor.hotkey_label"), keyListener.getNamedKeyStroke()); //$NON-NLS-1$
    }
    return keyConfig;
  }

  protected void checkVisibility() {
    setVisible((getText() != null && getText().length() > 0) ||
                getIcon() != null);
  }
}
