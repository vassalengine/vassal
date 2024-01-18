/*
 *
 * Copyright (c) 2008-2023 by Rodney Kinney, The VASSAL Development Team
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
import VASSAL.tools.NamedKeyManager;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.icon.IconFactory;
import VASSAL.tools.icon.IconFamily;
import VASSAL.tools.swing.SwingUtils;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.SystemUtils;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JLayer;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.plaf.LayerUI;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

/**
 * A configurer for Configuring Key Strokes. It allows the entry of either
 * a standard keystroke, or a Named command.
 *
 * It contains two separate Text fields, one for the Name and one for the keystroke.
 * A user can fill in one or the other. Filling in one, clears the other.
 *
 * This Configurer has a limited undo function. Whenever one of the two fields gains focus,
 * the current state of the Configurer is saved and the Undo button enabled.
 * The undo button will return to the state when that field gained focus.
 * This provides a one-step undo if a user accidentally types in one of the fields and
 * wipes out data in the other field.
 */
public class NamedHotKeyConfigurer extends Configurer implements FocusListener {
  private static final String STROKE_HINT = Resources.getString("Editor.NamedHotKeyConfigurer.keystroke");
  private static final String NAME_HINT = Resources.getString("Editor.NamedHotKeyConfigurer.command");
  private HintTextField keyStroke;
  private HintTextField keyName;
  private JPanel controls;
  private String lastValue;
  private JButton undoButton;
  private final int defaultFieldLength;

  private HintTextField getKeyStroke() {
    if (keyStroke == null) {
      keyStroke = new HintTextField(defaultFieldLength, STROKE_HINT);
    }
    return keyStroke;
  }

  private HintTextField getKeyName() {
    if (keyName == null) {
      keyName = new HintTextField(defaultFieldLength, NAME_HINT);
    }
    return keyName;
  }

  public static String getFancyString(NamedKeyStroke k) {
    String s = getString(k);
    if (s.length() > 0) {
      s = "[" + s + "]";
    }
    return s;
  }

  /**
   * Return a String representation of a NamedKeyStroke
   * @param k NamedKeyStroke
   * @return String representation
   */
  public static String getString(NamedKeyStroke k) {
    return (k == null || k.isNull()) ? "" : getString(k.getStroke());
  }

  /**
   * Return a string representation of a KeyStroke
   * @param k KeyStroke
   * @return String representation
   */
  public static String getString(KeyStroke k) {
    return NamedKeyManager.isNamed(k) ? "" : HotKeyConfigurer.getString(k);
  }

  public NamedHotKeyConfigurer(String key, String name, NamedKeyStroke val, int defaultFieldLength) {
    super(key, name, val);
    this.defaultFieldLength = defaultFieldLength;
  }

  public NamedHotKeyConfigurer(String key, String name, NamedKeyStroke val) {
    this(key, name, val, StringConfigurer.DEFAULT_LENGTH);
  }

  public NamedHotKeyConfigurer(String key, String name) {
    this(key, name, NamedKeyStroke.NULL_KEYSTROKE);
  }

  public NamedHotKeyConfigurer(NamedKeyStroke val, int defaultFieldLength) {
    this(null, null, val, defaultFieldLength);
  }

  public NamedHotKeyConfigurer(NamedKeyStroke val) {
    this(null, null, val);
  }

  public NamedHotKeyConfigurer() {
    this(null);
  }

  @Override
  public String getValueString() {
    return encode((NamedKeyStroke) getValue());
  }

  public NamedKeyStroke getValueNamedKeyStroke() {
    return (NamedKeyStroke) value;
  }

  public boolean isNamed() {
    return value != null && ((NamedKeyStroke) value).isNamed();
  }

  @Override
  public void requestFocus() {
    if (keyName != null) {
      keyName.requestFocus();
    }
  }

  @Override
  public void setValue(Object o) {
    setFrozen(true); // Prevent changes to the input fields triggering further updates
    if (controls != null && !noUpdate) {
      final NamedKeyStroke stroke = (NamedKeyStroke) o;
      if (stroke != null && stroke.isNamed()) {
        keyName.setText(stroke.getName());
        keyStroke.setText("");
      }
      else {
        keyName.setText("");
        keyStroke.setText(getString(stroke));
      }
      updateVisibility();
    }
    setFrozen(false);
    super.setValue(o);
  }

  protected void updateVisibility() {
    getKeyName().setFocusOnly(isNonNullValue());
    getKeyStroke().setFocusOnly(isNonNullValue());
  }

  private boolean isNonNullValue() {
    return value != null && !((NamedKeyStroke) value).isNull();
  }

  @Override
  public void setValue(String s) {
    setValue(s == null ? null : decode(s));
  }

  private void updateValueFromKeyName() {
    if (! isFrozen()) {
      final String key = keyName.getText();
      if (key.isEmpty()) {
        setValue(NamedKeyStroke.NULL_KEYSTROKE);
      }
      else {
        setValue(NamedKeyStroke.of(key));
      }
    }
  }

  @Override
  public Component getControls() {
    if (controls == null) {
      controls = new ConfigurerPanel(getName(), "[fill,grow]", "[][fill,grow]"); // NON-NLS
      keyStroke = getKeyStroke();
      keyStroke.setMaximumSize(new Dimension(keyStroke.getMaximumSize().width, keyStroke.getPreferredSize().height));
      keyStroke.setText(keyToString());
      keyStroke.addKeyListener(new KeyStrokeAdapter());
      ((AbstractDocument) keyStroke.getDocument()).setDocumentFilter(new KeyStrokeFilter());
      keyStroke.addFocusListener(this);
      // Turn off Swing-level paste in the keystroke field
      keyStroke.setTransferHandler(null);

      keyName = getKeyName();
      keyName.setMaximumSize(new Dimension(keyName.getMaximumSize().width, keyName.getPreferredSize().height));
      keyName.setText(getValueNamedKeyStroke() == null ? null : getValueNamedKeyStroke().getName());
      ((AbstractDocument) keyName.getDocument()).setDocumentFilter(new KeyNameFilter());
      keyName.addFocusListener(this);

      // Edit box selects all text when first focused
      keyName.addFocusListener(new java.awt.event.FocusAdapter() {
        @Override
        public void focusGained(FocusEvent evt) {
          SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
              keyName.selectAll();
            }
          });
        }
      });

      final JPanel panel = new JPanel(new MigLayout("ins 0", "[fill,grow]0[]0[fill,grow]0[]")); // NON-NLS

      final LayerUI<JTextField> layerUI = new ConfigLayerUI(this);
      final JLayer<JTextField> nameLayer = new JLayer<>(keyName, layerUI);
      panel.add(nameLayer, "grow"); // NON-NLS

      panel.add(new JLabel("-")); // NON-NLS

      final JLayer<JTextField> keyLayer = new JLayer<>(keyStroke, layerUI);
      panel.add(keyLayer, "grow"); // NON-NLS

      undoButton = new JButton(IconFactory.getIcon("edit-undo", IconFamily.XSMALL)); // NON-NLS
      final int size = (int) keyName.getPreferredSize().getHeight();
      undoButton.setPreferredSize(new Dimension(size, size));
      undoButton.setMaximumSize(new Dimension(size, size));
      undoButton.addActionListener(e -> undo());
      undoButton.setEnabled(false);
      undoButton.setToolTipText(Resources.getString("Editor.undo"));
      panel.add(undoButton);

      controls.add(panel, "grow"); // NON-NLS
      updateVisibility();
    }
    return controls;
  }

  private void undo() {
    if (lastValue != null) {
      setValue(lastValue);
      lastValue = null;
      undoButton.setEnabled(false);
      keyName.requestFocus();
    }
  }

  @Override
  public void focusGained(FocusEvent e) {
    lastValue = getValueString();
    undoButton.setEnabled(true);
  }

  @Override
  public void focusLost(FocusEvent e) {

  }

  public String keyToString() {
    return getString((NamedKeyStroke) getValue());
  }

  protected boolean isPrintableAscii(char c) {
    return isPrintableAscii((int) c);
  }

  protected boolean isPrintableAscii(int i) {
    return i >= ' ' && i <= '~';
  }

  /**
   * Decode a String into a NamedKeyStroke
   */
  public static NamedKeyStroke decode(String s) {
    if (s == null) {
      return NamedKeyStroke.NULL_KEYSTROKE;
    }
    final String[] parts = s.split(",");
    if (parts.length < 2) {
      return NamedKeyStroke.NULL_KEYSTROKE;
    }

    try {
      final KeyStroke stroke = KeyStroke.getKeyStroke(
        Integer.parseInt(parts[0]),
        Integer.parseInt(parts[1])
      );
      String name = null;
      if (parts.length > 2) {
        name = parts[2];
      }
      return NamedKeyStroke.of(stroke, name);
    }
    catch (Exception e) {
      return NamedKeyStroke.NULL_KEYSTROKE;
    }
  }

  /**
   * Encode a NamedKeyStroke into a String
   */
  public static String encode(NamedKeyStroke stroke) {
    if (stroke == null) {
      return "";
    }
    final KeyStroke key = stroke.getStroke();
    if (key == null) {
      return "";
    }
    String s = key.getKeyCode() + "," + key.getModifiers();
    if (stroke.isNamed()) {
      s += "," + stroke.getName();
    }
    return s;
  }

  @Override
  public void setHighlighted(boolean highlighted) {
    super.setHighlighted(highlighted);
    getKeyStroke().setBackground(highlighted ? LIST_ENTRY_HIGHLIGHT_COLOR : Color.white);
    getKeyName().setBackground(highlighted ? LIST_ENTRY_HIGHLIGHT_COLOR : Color.white);
    getKeyStroke().repaint();
    getKeyName().repaint();
  }

  @Override
  public void addFocusListener(FocusListener listener) {
    super.addFocusListener(listener);
    getKeyStroke().addFocusListener(listener);
    getKeyName().addFocusListener(listener);
  }

  @Override
  public void removeFocusListener(FocusListener listener) {
    super.removeFocusListener(listener);
    getKeyStroke().removeFocusListener(listener);
    getKeyName().removeFocusListener(listener);
  }

  // Use JLayer to outline the fields in Red as the Unix LaF ignores TextField background colours
  private static class ConfigLayerUI extends LayerUI<JTextField> {
    private static final long serialVersionUID = 1L;

    private final Configurer parent;

    public ConfigLayerUI(Configurer parent) {
      this.parent = parent;
    }

    @Override
    public void paint(Graphics g, JComponent c) {
      super.paint(g, c);
      final Component cc = ((JLayer) c).getView();
      if (parent.isHighlighted()) {
        final Dimension d = cc.getSize();
        g.setColor(Color.red);
        g.drawRect(0, 0, d.width - 2, d.height - 2);
      }
    }
  }

  private class KeyStrokeAdapter extends KeyAdapter {
    @Override
    public void keyPressed(KeyEvent e) {
      // reportKeyEvent("KEY_PRESSED", e); // NON-NLS
      switch (e.getKeyCode()) {
      case KeyEvent.VK_DELETE:
      case KeyEvent.VK_BACK_SPACE:
        // Allow mapping of Delete
        if (getValue().equals(NamedKeyStroke.NULL_KEYSTROKE) || e.isShiftDown() || e.isControlDown() || e.isMetaDown() || e.isAltDown()) {
          setValue(NamedKeyStroke.of(SwingUtils.convertKeyEvent(e)));
        }
        else {
          setValue(NamedKeyStroke.NULL_KEYSTROKE);
        }
        break;
      case KeyEvent.VK_SHIFT:
      case KeyEvent.VK_CONTROL:
      case KeyEvent.VK_META:
      case KeyEvent.VK_ALT:
      case KeyEvent.VK_ALT_GRAPH:
      case KeyEvent.VK_UNDEFINED:
        break;

      //BR// Consume ESC & Enter events so that we don't close a piece editor dialog while editing a keystroke
      case KeyEvent.VK_ESCAPE:
      case KeyEvent.VK_ENTER:
        e.consume();
        setValue(NamedKeyStroke.of(SwingUtils.convertKeyEvent(e)));
        break;

      default:
        setValue(NamedKeyStroke.of(SwingUtils.convertKeyEvent(e)));
      }
    }

    // Repeat the Key handling for each Key of interest on release.
    // Caters for the bizarre KeyEvent sequences created on MacOS.
    // ALSO, it turns out, makes alphanumeric keys record properly on Windows
    @Override
    public void keyReleased(KeyEvent e) {
      // reportKeyEvent("KEY_RELEASED", e); // NON-NLS
      switch (e.getKeyCode()) {
      case KeyEvent.VK_DELETE:
      case KeyEvent.VK_BACK_SPACE:
        if (SystemUtils.IS_OS_MAC) {
          // Allow mapping of Delete
          if (getValue().equals(NamedKeyStroke.NULL_KEYSTROKE) || e.isShiftDown() || e.isControlDown() || e.isMetaDown() || e.isAltDown()) {
            setValue(NamedKeyStroke.of(SwingUtils.convertKeyEvent(e)));
          }
          else {
            setValue(NamedKeyStroke.NULL_KEYSTROKE);
          }
        }
        break;
      case KeyEvent.VK_SHIFT:
      case KeyEvent.VK_CONTROL:
      case KeyEvent.VK_META:
      case KeyEvent.VK_ALT:
      case KeyEvent.VK_ALT_GRAPH:
      case KeyEvent.VK_UNDEFINED:
        break;
      default:
        setValue(NamedKeyStroke.of(SwingUtils.convertKeyEvent(e)));
      }
    }
  }

  private class KeyNameFilter extends DocumentFilter {
    @Override
    public void remove(FilterBypass fb, int offset, int length) throws BadLocationException {
      super.remove(fb, offset, length);
      updateValueFromKeyName();
      keyName.setCaretPosition(offset);
    }

    @Override
    public void insertString(FilterBypass fb, int offset, String string, AttributeSet attr) throws BadLocationException {
      super.insertString(fb, offset, string, attr);
      updateValueFromKeyName();
      keyName.setCaretPosition(offset + (string == null ? 0 : string.length()));
    }

    @Override
    public void replace(FilterBypass fb, int offset, int length, String text, AttributeSet attrs) throws BadLocationException {
      super.replace(fb, offset, length, text, attrs);
      updateValueFromKeyName();
      keyName.setCaretPosition(offset + (text == null ? 0 : text.length()));
    }
  }

  private class KeyStrokeFilter extends DocumentFilter {
    @Override
    public void replace(FilterBypass fb, int offset, int length, String text, AttributeSet attrs) throws BadLocationException {
      super.replace(fb, 0, keyStroke.getText().length(), text, attrs);
    }
  }
}
