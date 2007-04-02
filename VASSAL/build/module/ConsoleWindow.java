/*
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
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
package VASSAL.build.module;

import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import VASSAL.configure.StringConfigurer;

/**
 * VASSAL main window for when no game is in progress. A user-friendly dialog with basic controls for starting a game
 * on- or off-line
 * 
 * @author rkinney
 */
public class ConsoleWindow {
  protected Image backgroundImage;
  protected JComponent background;
  protected Box buttonBox;
  protected Box contentBox;
  protected JPanel controls;
  protected JFrame theFrame;
  protected JButton myName;
  protected JButton playLocal;
  protected JButton playOnline;
  protected JComponent myNameControls;
  protected JComponent playLocalControls;
  protected JComponent playOnlineControls;
  protected CardLayout cardLayout = new CardLayout();
  protected Color backgroundColor = Color.black;
  protected Color textColor = Color.white;
  protected String moduleVersionNumber="5.0";
  protected String moduleName="VASL";

  public ConsoleWindow() {
  }

  protected void initComponents() {
    theFrame = new JFrame();
    theFrame.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
      System.exit(0);
      }
    });
    theFrame.getContentPane().setBackground(backgroundColor);
    theFrame.setTitle(moduleName+" version "+moduleVersionNumber);

    controls = new JPanel();
    controls.setLayout(cardLayout);

    background = Box.createVerticalBox();
    JLabel moduleTitle = new JLabel(moduleName);
    moduleTitle.setFont(new Font("SansSerif",Font.BOLD,60));
    moduleTitle.setAlignmentX(0.5f);
    moduleTitle.setForeground(textColor);
    background.add(moduleTitle);
    JLabel moduleVersion = new JLabel("version "+moduleVersionNumber);
    moduleVersion.setFont(new Font("SansSerif",Font.BOLD,12));
    moduleVersion.setAlignmentX(0.5f);
    moduleVersion.setForeground(textColor);
    background.add(moduleVersion);

    backgroundImage = Toolkit.getDefaultToolkit().getImage("images/Splash.gif");
    JLabel backgroundLabel = new JLabel(new ImageIcon(backgroundImage));
    backgroundLabel.setAlignmentX(0.5f);
    background.add(backgroundLabel);

    buttonBox = Box.createVerticalBox();
    myName = addButton("My Name", createMyNameControls());
    playLocal = addButton("Play Offline", createPlayOfflineControls());
    playOnline = addButton("Play Online", createPlayOnlineControls());
    JPanel blank = new JPanel();
    blank.setOpaque(false);
    controls.add(blank,"");
    controls.setOpaque(false);
    cardLayout.show(controls,"");
    buttonBox.add(Box.createVerticalGlue());
    contentBox = Box.createHorizontalBox();
    contentBox.add(buttonBox);
    contentBox.add(controls);
    contentBox.setBounds(new Rectangle(new Point(),background.getPreferredSize()));
    theFrame.getLayeredPane().add(contentBox,new Integer(1));
    theFrame.getContentPane().add(background);
    theFrame.pack();
    theFrame.setLocationRelativeTo(null);
  }

  protected JComponent createPlayOnlineControls() {
    Box b = Box.createVerticalBox();
    b.add(Box.createVerticalGlue());
    b.add(new JButton("Connect"));
    b.add(Box.createVerticalGlue());
    return b;
  }

  protected JComponent createPlayOfflineControls() {
    Box b = Box.createVerticalBox();
    b.add(Box.createVerticalGlue());
    b.add(new JButton("New Game"));
    b.add(new JButton("Load Saved Game"));
    b.add(Box.createVerticalGlue());
    return b;
  }

  protected JComponent createMyNameControls() {
    Box b = Box.createVerticalBox();
    b.add(Box.createVerticalGlue());
    b.add(new StringConfigurer(null,"Real Name:  ").getControls());
    b.add(new StringConfigurer(null,"Password:  ").getControls());
    b.add(Box.createVerticalGlue());
    return b;
  }

  protected JButton addButton(final String label, JComponent buttonControls) {
    buttonBox.add(Box.createVerticalGlue());
    JButton b = new JButton(label);
    b.setAlignmentX(0.0f);
    buttonBox.add(b);
    controls.add(buttonControls,label);
    b.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        cardLayout.show(controls,label);
      }
    });
    return b;
  }
  
  public void showFrame() {
    if (theFrame == null) {
      initComponents();
    }
    theFrame.setVisible(true);
  }
  
  public static void main(String[] args) {
    new ConsoleWindow().showFrame();
  }
}
