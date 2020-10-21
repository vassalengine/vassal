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
import java.awt.RenderingHints;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import javax.swing.JTextField;

/**
 * A class extending a JTexfield that can display a 'ghost' text prompt
 * in the field when it is empty and has focus
 */
public class HintTextField extends JTextField implements FocusListener {

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
      ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
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

  public boolean isFocusOnly() {
    return focusOnly;
  }

  public void setFocusOnly(boolean focusOnly) {
    this.focusOnly = focusOnly;
    repaint();
  }

  public String getHint() {
    return hint;
  }

  public void setHint(String hint) {
    this.hint = hint;
    repaint();
  }
}
