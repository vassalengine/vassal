/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
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

package VASSAL.tools.swing;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;

import org.apache.commons.lang3.SystemUtils;

import VASSAL.i18n.Resources;

/**
 * An About box that follows the platform's conventions.
 *
 * <ul>
 * <li>macOS: a modeless, untitled panel whose only control is the window's
 * close button (no OK button); Escape and Cmd-W also close it. Choosing About
 * again brings the open panel to the front.</li>
 * <li>Windows: a modal dialog titled "About &lt;name&gt;" with an OK button.</li>
 * <li>Linux and others: a modeless dialog titled "About &lt;name&gt;" with a
 * Close button.</li>
 * </ul>
 *
 * The image is shown above the name (in bold) and the version lines. Images
 * larger than {@link #MAX_IMAGE_WIDTH} x {@link #MAX_IMAGE_HEIGHT} are scaled
 * down to fit; smaller ones are shown at their own size.
 *
 * @since 3.8.3
 */
public class AboutDialog extends JDialog {
  private static final long serialVersionUID = 1L;

  public static final int MAX_IMAGE_WIDTH = 640;
  public static final int MAX_IMAGE_HEIGHT = 480;

  private static AboutDialog open;

  /**
   * Shows the About box, or brings an already open one for the same name to
   * the front.
   */
  public static void show(Window owner, String name, Image image, List<String> lines) {
    if (open != null && open.isDisplayable() && name.equals(open.name)) {
      open.toFront();
      return;
    }
    if (open != null) {
      open.dispose();
    }
    open = new AboutDialog(owner, name, image, lines);
    open.setVisible(true);
  }

  private final String name;

  public AboutDialog(Window owner, String name, Image image, List<String> lines) {
    super(
      owner,
      SystemUtils.IS_OS_MAC ? "" : Resources.getString("AboutScreen.about_module_name", name), //NON-NLS
      SystemUtils.IS_OS_WINDOWS ? ModalityType.APPLICATION_MODAL : ModalityType.MODELESS
    );
    this.name = name;

    setDefaultCloseOperation(DISPOSE_ON_CLOSE);
    setResizable(false);

    final JPanel content = new JPanel(new BorderLayout(0, 12));
    content.setBorder(BorderFactory.createEmptyBorder(20, 24, 16, 24));

    if (image != null) {
      final JLabel pic = new JLabel(new ImageIcon(fit(image)));
      pic.setHorizontalAlignment(SwingConstants.CENTER);
      content.add(pic, BorderLayout.NORTH);
    }

    final JPanel text = new JPanel();
    text.setLayout(new BoxLayout(text, BoxLayout.Y_AXIS));

    final JLabel title = new JLabel(name);
    title.setFont(title.getFont().deriveFont(Font.BOLD, title.getFont().getSize2D() + 6f));
    title.setAlignmentX(CENTER_ALIGNMENT);
    text.add(title);
    text.add(Box.createVerticalStrut(6));

    for (final String line : lines) {
      final JLabel l = new JLabel(line);
      l.setAlignmentX(CENTER_ALIGNMENT);
      text.add(l);
    }
    content.add(text, BorderLayout.CENTER);

    final AbstractAction close = new AbstractAction() {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        dispose();
      }
    };

    if (!SystemUtils.IS_OS_MAC) {
      final JButton button = new JButton(close);
      button.setText(Resources.getString(SystemUtils.IS_OS_WINDOWS ? "General.ok" : "General.close")); //NON-NLS
      final JPanel buttons = new JPanel(new FlowLayout(FlowLayout.RIGHT, 0, 0));
      buttons.add(button);
      content.add(buttons, BorderLayout.SOUTH);
      getRootPane().setDefaultButton(button);
    }

    final JComponent root = getRootPane();
    root.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
      .put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "close"); //NON-NLS
    if (SystemUtils.IS_OS_MAC) {
      root.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(
        KeyStroke.getKeyStroke(KeyEvent.VK_W, Toolkit.getDefaultToolkit().getMenuShortcutKeyMaskEx()),
        "close"); //NON-NLS
    }
    root.getActionMap().put("close", close); //NON-NLS

    setContentPane(content);
    pack();
    setLocationRelativeTo(owner);
  }

  private static Image fit(Image img) {
    final int w = img.getWidth(null);
    final int h = img.getHeight(null);
    if (w <= MAX_IMAGE_WIDTH && h <= MAX_IMAGE_HEIGHT) {
      return img;
    }
    final double scale = Math.min((double) MAX_IMAGE_WIDTH / w, (double) MAX_IMAGE_HEIGHT / h);
    return img.getScaledInstance(
      (int) Math.round(w * scale), (int) Math.round(h * scale), Image.SCALE_SMOOTH);
  }
}
