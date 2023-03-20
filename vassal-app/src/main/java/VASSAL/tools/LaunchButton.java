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

import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.configure.Configurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.Auditable;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.KeyStroke;
import java.awt.event.ActionListener;

/**
 * A JButton for placing into a VASSAL component's toolbar.
 * Handles configuration of a hotkey shortcut, maintains appropriate
 * tooltip text, etc.
 */
public class LaunchButton extends JButton implements Auditable {
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
  protected boolean alwaysAcceptKeystroke;
  protected boolean forceVisible = false;
  protected boolean allowExpression;
  protected boolean usesExpression = false;
  protected FormattedString formatted = new FormattedString("");

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
    this(text, textAttribute, hotkeyAttribute, iconAttribute, al, false);
  }

  public LaunchButton(String text, String tooltipAttribute, String textAttribute, String hotkeyAttribute, String iconAttribute, final ActionListener al, boolean allowExpression) {
    this(text, textAttribute, hotkeyAttribute, iconAttribute, al, allowExpression);
    tooltipAtt = tooltipAttribute;
  }

  public LaunchButton(String text, String textAttribute, String hotkeyAttribute, String iconAttribute, final ActionListener al, boolean allowExpression) {
    super(text);

    this.allowExpression = allowExpression;
    if (allowExpression) {
      setFormat(text);
      updateText();
    }

    alwaysAcceptKeystroke = false;
    nameAtt = textAttribute;
    keyAtt = hotkeyAttribute;
    iconAtt = iconAttribute;
    iconConfig = new IconConfigurer(iconAtt, null, null);
    setAlignmentY(0.0F);
    keyListener = new NamedKeyStrokeListener(e -> {
      if ((isEnabled() || isAlwaysAcceptKeystroke()) && (GlobalOptions.getInstance().isHotKeysOnClosedWindows() || (getParent() != null && getParent().isShowing()))) {
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

  public boolean isUsesExpression() {
    return usesExpression;
  }

  private void setFormat(String text) {
    formatted.setFormat(text);
    usesExpression = formatted.getFormat().contains("$") || formatted.getFormat().contains("{");
    if (usesExpression) {
      GameModule.getGameModule().setMutableButtonSupport(true); // We're gonna need a bigger updater...
    }
  }

  public void updateText() {
    if (usesExpression) {
      setText(formatted.getText(GameModule.getGameModule(), this, "Editor.DrawPile.count_express"));
    }
  }

  /**
   * @return Our current icon (used when we're a ToolbarMenu that's a submenu of another ToolbarMenu so that we keep our icon)
   */
  public Icon getLaunchIcon() {
    return iconConfig.getIconValue();
  }

  public boolean isAlwaysAcceptKeystroke() {
    return alwaysAcceptKeystroke;
  }

  public void setAlwaysAcceptKeystroke(boolean always) {
    alwaysAcceptKeystroke = always;
  }

  public boolean isForcevisible() {
    return forceVisible;
  }

  public void setForceVisible(boolean fv) {
    forceVisible = fv;
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

        if (allowExpression) {
          setFormat((String)value);
          updateText();
        }
        else {
          setText((String) value);
        }
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

  public boolean isNonBlank() {
    return (getText() != null && getText().length() > 0) || getIcon() != null;
  }

  protected void checkVisibility() {
    setVisible(isNonBlank() || forceVisible);
  }

  @Override
  public String getComponentTypeName() {
    return "Launch Button";
  };

  /**
   * Return the name of the trait or Component an Auditable is
   * @return Component name
   */
  @Override
  public String getComponentName() {
    return "Launch Button";
  };
}
