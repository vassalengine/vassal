/*
 *
 * Copyright (c) 2020 by Vassal development team
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

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import javax.swing.JTextField;

import VASSAL.tools.swing.SwingUtils;

/**
 * A class extending a JTextField that can display a 'ghost' text prompt
 * in the field when it is empty.
 * Hint display by default is whenever the field has no text, but
 * hint display can be limited to only when the field has focus.
 */
public class HintTextField extends JTextField implements FocusListener {
  private static final long serialVersionUID = 1L;

  private String hint;
  private Color hintColor;
  private Font hintFont;
  private boolean focusOnly;

  /**
   * Create a new HintTextField with a length and specified hint
   *
   * @param length Field Length
   * @param hint Hint text
   */
  public HintTextField(int length, String hint) {
    super(length);
    setHint(hint);
    setFocusOnly(false);
    addFocusListener(this);
  }

  /**
   * Create a new HintTextField with a specified hint
   *
   * @param hint Hint text
   */
  public HintTextField(String hint) {
    super();
    setHint(hint);
    setFocusOnly(false);
    addFocusListener(this);
  }

  @Override
  public void paint(Graphics g) {
    super.paint(g);
    if (getText().isEmpty() && focusCheck() && hint != null && ! hint.isEmpty()) {
      final int h = getHeight();

      ((Graphics2D) g).addRenderingHints(SwingUtils.FONT_HINTS);
      final Insets ins = getInsets();
      final FontMetrics fm = g.getFontMetrics();
      if (hintColor == null) {
        final int c0 = getBackground().getRGB();
        final int c1 = getForeground().getRGB();
        final int m = 0xfefefefe;
        final int c2 = ((c0 & m) >>> 1) + ((c1 & m) >>> 1);
        hintColor = new Color(c2, true);
        hintFont = new Font(getFont().getFontName(), Font.ITALIC, getFont().getSize());
      }
      g.setColor(hintColor);
      g.setFont(hintFont);
      g.drawString(hint, ins.left, h / 2 + fm.getAscent() / 2 - 2);
    }
  }

  protected boolean focusCheck() {
    return !isFocusOnly() || hasFocus();
  }

  @Override
  public void focusGained(FocusEvent e) {
    repaint();
  }

  @Override
  public void focusLost(FocusEvent e) {
    repaint();
  }

  /**
   * Does this field only display a hint when it has the focus?
   *
   * @return Display hint only with focus
   */
  public boolean isFocusOnly() {
    return focusOnly;
  }

  /**
   * Set whether or not the hint should be displayed if the field does not have focus
   *
   * @param focusOnly If true, hint will only be displayed if field has focus
   */
  public void setFocusOnly(boolean focusOnly) {
    this.focusOnly = focusOnly;
    repaint();
  }

  /**
   * Return the current hint text
   *
   * @return Hint text
   */
  public String getHint() {
    return hint;
  }

  /**
   * Set the Hint text
   *
   * @param hint Hint text
   */
  public void setHint(String hint) {
    this.hint = hint;
    repaint();
  }
}
