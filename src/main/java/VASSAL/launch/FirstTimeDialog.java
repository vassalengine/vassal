/*
 * $Id$
 *
 * Copyright (c) 2000-2008 by Rodney Kinney, Joel Uckelman
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
package VASSAL.launch;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.net.MalformedURLException;
import java.util.Locale;

import javax.swing.DefaultListCellRenderer;
import javax.swing.GroupLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.LayoutStyle;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.module.Documentation;
import VASSAL.configure.ShowHelpAction;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.image.ImageIOException;

/**
 * A dialog for first-time users.
 *
 * @since 3.1.0
 */
public class FirstTimeDialog extends JDialog {
  private static final long serialVersionUID = 1L;

  private static final Logger logger =
    LoggerFactory.getLogger(FirstTimeDialog.class);

  public FirstTimeDialog(Frame parent) {
    super(parent, true);

    setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
    addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent e) {
        System.exit(0);
      }
    });

    final JLabel about = new JLabel();

    final JLabel welcome = new JLabel();
    welcome.setFont(new Font("SansSerif", 1, 40));  //$NON-NLS-1$
    welcome.setText(Resources.getString("Main.welcome"));  //$NON-NLS-1$
    welcome.setForeground(Color.black);

    final JButton tour = new JButton(new LaunchTourAction(parent));
    final JButton jump =
      new JButton(Resources.getString("Main.jump_right_in"));  //$NON-NLS-1$
    final JButton help = new JButton(Resources.getString(Resources.HELP));

    final ActionListener closer = new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent evt) {
        FirstTimeDialog.this.dispose();
      }
    };

    tour.addActionListener(closer);
    jump.addActionListener(closer);

    try {
      final File readme =
        new File (Documentation.getDocumentationBaseDir(), "README.html");
      help.addActionListener(new ShowHelpAction(readme.toURI().toURL(), null));
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    final JLabel lang = new JLabel(Resources.getString("Prefs.language") + ":");
    final JComboBox langbox =
      new JComboBox(Resources.getSupportedLocales().toArray());
    langbox.setRenderer(new DefaultListCellRenderer() {
      private static final long serialVersionUID = 1L;

      @Override
      public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
        super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        setText(((Locale) value).getDisplayName(Resources.getLocale()));
        return this;
      }
    });

    langbox.setSelectedItem(Resources.getLocale());
    langbox.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        Resources.setLocale((Locale) langbox.getSelectedItem());

        // update the text for the new locale
        welcome.setText(Resources.getString("Main.welcome"));  //$NON-NLS-1$
        tour.setText(Resources.getString("Main.tour"));  //$NON-NLS-1$
        jump.setText(Resources.getString("Main.jump_right_in"));  //$NON-NLS-1$
        help.setText(Resources.getString(Resources.HELP));
        lang.setText(Resources.getString("Prefs.language") + ":");
        FirstTimeDialog.this.pack();
        // langbox picks up the new locale automatically from getDisplayName()
      }
    });

    final JPanel panel = new JPanel();
    final GroupLayout layout = new GroupLayout(panel);
    panel.setLayout(layout);

    layout.setAutoCreateGaps(true);
    layout.setAutoCreateContainerGaps(true);

    layout.setHorizontalGroup(
      layout.createParallelGroup(GroupLayout.Alignment.CENTER, true)
        .addComponent(about)
        .addComponent(welcome)
        .addGroup(layout.createSequentialGroup()
          .addComponent(tour)
          .addComponent(jump)
          .addComponent(help))
        .addGroup(layout.createSequentialGroup()
          .addGap(0, 0, Integer.MAX_VALUE)
          .addComponent(lang)
          .addComponent(langbox)
          .addGap(0, 0, Integer.MAX_VALUE)));

    layout.setVerticalGroup(
      layout.createSequentialGroup()
        .addComponent(about)
        .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED,
                         GroupLayout.DEFAULT_SIZE, Integer.MAX_VALUE)
        .addComponent(welcome)
        .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED,
                         GroupLayout.DEFAULT_SIZE, Integer.MAX_VALUE)
        .addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE, false)
          .addComponent(tour)
          .addComponent(jump)
          .addComponent(help))
        .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED,
                         GroupLayout.DEFAULT_SIZE, Integer.MAX_VALUE)
        .addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE, false)
          .addComponent(lang)
          .addComponent(langbox)));

    layout.linkSize(new Component[]{tour, jump, help});

    add(panel);
    pack();

    // load the splash image
    BufferedImage img = null;
    try {
      img = ImageUtils.getImageResource("/images/Splash.png");
    }
    catch (ImageIOException e) {
      logger.error("", e);
    }

    if (img != null) {
      // ensure that the dialog fits on the screen
      final Rectangle screen = GraphicsEnvironment.getLocalGraphicsEnvironment()
                                                  .getMaximumWindowBounds();
      final Dimension dsize = getSize();
      final Dimension remainder = new Dimension(
        Math.max(screen.width - dsize.width, 0),
        Math.max(screen.height - dsize.height, 0)
      );

      if (remainder.width == 0 || remainder.height == 0) {
        // no room for the image, do nothing
      }
      else if (remainder.width >= img.getWidth() &&
               remainder.height >= img.getHeight()) {
        // the whole image fits, use it as-is
        about.setIcon(new ImageIcon(img));
      }
      else {
        // downscale the image to fit
        final double scale = Math.min(
          remainder.width  / (double) img.getWidth(),
          remainder.height / (double) img.getHeight()
        );
        about.setIcon(new ImageIcon(ImageUtils.transform(img, scale, 0.0)));;
      }

      pack();
    }

    setMinimumSize(getSize());
    setLocationRelativeTo(null);
  }
}
