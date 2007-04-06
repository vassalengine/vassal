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

import java.awt.AlphaComposite;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.Box;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.Tutorial;
import VASSAL.chat.ui.ChatServerControls;
import VASSAL.configure.Configurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.launch.BasicModule;
import VASSAL.preferences.Prefs;
import VASSAL.preferences.PrefsEditor;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.DataArchive;

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
  protected JButton launchTutorial;
  protected JComponent myNameControls;
  protected JComponent playLocalControls;
  protected JComponent playOnlineControls;
  protected CardLayout cardLayout = new CardLayout();
  protected Color backgroundColor = Color.black;
  protected Color textColor = Color.white;
  protected String moduleVersionNumber = "5.0";
  protected String moduleName = "VASL";
  protected DefaultComboBoxModel setups = new DefaultComboBoxModel();
  protected JComboBox setupSelection;
  private Tutorial tutorial;

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
    theFrame.setTitle(moduleName + " version " + moduleVersionNumber);
    controls = new JPanel();
    controls.setLayout(cardLayout);
    background = Box.createVerticalBox();
    JLabel moduleTitle = new JLabel(moduleName);
    moduleTitle.setFont(new Font("SansSerif", Font.BOLD, 60));
    moduleTitle.setAlignmentX(0.5f);
    moduleTitle.setForeground(textColor);
    background.add(moduleTitle);
    JLabel moduleVersion = new JLabel("version " + moduleVersionNumber);
    moduleVersion.setFont(new Font("SansSerif", Font.BOLD, 12));
    moduleVersion.setAlignmentX(0.5f);
    moduleVersion.setForeground(textColor);
    background.add(moduleVersion);
    backgroundImage = Toolkit.getDefaultToolkit().getImage("images/Splash.gif");
    JLabel backgroundLabel = new JLabel(new ImageIcon(backgroundImage)) {
      protected void paintComponent(Graphics g) {
        Graphics2D g2d = (Graphics2D) g;
        Composite c = g2d.getComposite();
        g2d.setComposite(
            AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5F));
        super.paintComponent(g);
        g2d.setComposite(c);
      }
    };
    backgroundLabel.setAlignmentX(0.5f);
    background.add(backgroundLabel);
    buttonBox = Box.createVerticalBox();
    myName = addButton("My Name", createMyNameControls());
    if (tutorial != null) {
      launchTutorial = addButton(tutorial.getConfigureName(),new JPanel());
      launchTutorial.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          GameModule.getGameModule().getFrame().setVisible(true);
          tutorial.launch();
          resetControls();
        }
      });
    }
    playLocal = addButton("Play Offline", createPlayOfflineControls());
    if (GameModule.getGameModule() instanceof BasicModule) {
      final ChatServerControls controls = ((BasicModule)GameModule.getGameModule()).getServerControls();
      playOnline = addButton("Play Online", createPlayOnlineControls());
      playOnline.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          controls.toggleVisible();
          controls.getClient().setConnected(true);
          resetControls();
        }
      });
    }
    JPanel blank = new JPanel();
    blank.setOpaque(false);
    controls.add(blank, "");
    controls.setOpaque(false);
    cardLayout.show(controls, "");
    buttonBox.add(Box.createVerticalGlue());
    contentBox = Box.createHorizontalBox();
    contentBox.add(buttonBox);
    contentBox.add(controls);
    contentBox.setBounds(new Rectangle(new Point(), background.getPreferredSize()));
    theFrame.getLayeredPane().add(contentBox, new Integer(1));
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
    Box box = Box.createVerticalBox();
    box.add(Box.createVerticalGlue());
    JButton newGame = new JButton("New Game");
    newGame.setAlignmentX(0.5f);
    setupSelection = new JComboBox(setups);
    setupSelection.setVisible(setups.getSize() > 0);
    newGame.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (setups.getSize() == 0) {
          GameModule.getGameModule().getGameState().setup(false);
          GameModule.getGameModule().getGameState().setup(true);
        }
        else {
          ((PredefinedSetup) setups.getSelectedItem()).launch();
        }
        theFrame.setVisible(false);
      }
    });
    setupSelection.setMaximumSize(new Dimension(setupSelection.getMaximumSize().width, setupSelection.getPreferredSize().height));
    setupSelection.setRenderer(new DefaultListCellRenderer() {
      public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
        JLabel c = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        PredefinedSetup pds = (PredefinedSetup) value;
        c.setText((pds).getConfigureName());
        if (pds.isMenu()) {
          c.setSize(0, 0);
        }
        return c;
      }
    });
    Box newGameBox = Box.createHorizontalBox();
    newGameBox.setAlignmentX(0.5f);
    newGameBox.add(setupSelection);
    newGameBox.add(newGame);
    box.add(newGameBox);
    JButton loadGame = new JButton("Load Saved Game");
    loadGame.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        GameModule.getGameModule().getGameState().loadGame();
        theFrame.setVisible(false);
      }
    });
    loadGame.setAlignmentX(0.5f);
    box.add(loadGame);
    box.add(Box.createVerticalGlue());
    return box;
  }

  protected JComponent createMyNameControls() {
    Box b = Box.createVerticalBox();
    b.add(Box.createVerticalGlue());
    final Configurer nameConfig = GameModule.getGameModule().getPrefs().getOption(GameModule.REAL_NAME);
    final StringConfigurer localNameConfig = new StringConfigurer(null, "Real Name:  ");
    localNameConfig.setValue(nameConfig.getValue());
    nameConfig.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        localNameConfig.setFrozen(true);
        localNameConfig.setValue((String) evt.getNewValue());
        localNameConfig.setFrozen(false);
      }
    });
    localNameConfig.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        nameConfig.setFrozen(true);
        nameConfig.setValue((String) evt.getNewValue());
        nameConfig.setFrozen(false);
      }
    });
    b.add(localNameConfig.getControls());
    final Configurer pwdConfig = GameModule.getGameModule().getPrefs().getOption("UserName");
    final StringConfigurer localPwdConfig = new StringConfigurer(null, "Password:  ");
    localPwdConfig.setValue(pwdConfig.getValue());
    pwdConfig.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        localPwdConfig.setFrozen(true);
        localPwdConfig.setValue((String) evt.getNewValue());
        localPwdConfig.setFrozen(false);
      }
    });
    localPwdConfig.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        pwdConfig.setFrozen(true);
        pwdConfig.setValue((String) evt.getNewValue());
        pwdConfig.setFrozen(false);
      }
    });
    b.add(localPwdConfig.getControls());
    b.add(Box.createVerticalGlue());
    return b;
  }

  protected JButton addButton(final String label, JComponent buttonControls) {
    buttonBox.add(Box.createVerticalGlue());
    JButton b = new JButton(label);
    b.setAlignmentX(0.0f);
    buttonBox.add(b);
    controls.add(buttonControls, label);
    b.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        cardLayout.show(controls, label);
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

  public static void main(String[] args) throws Exception {
    GameModule.init(new BasicModule(new DataArchive("tour.mod"), new Prefs(new PrefsEditor(new ArchiveWriter("globalPrefs")), "VASSAL")));
    GameModule.getGameModule().getConsoleWindow().showFrame();
  }

  public Color getBackgroundColor() {
    return backgroundColor;
  }

  public void setBackgroundColor(Color backgroundColor) {
    this.backgroundColor = backgroundColor;
  }

  public Color getTextColor() {
    return textColor;
  }

  public void setTextColor(Color textColor) {
    this.textColor = textColor;
  }

  public void addPredefinedSetup(PredefinedSetup setup) {
    if (!setup.isMenu()) {
      setups.addElement(setup);
      resetControls();
    }
  }

  protected void resetControls() {
    if (theFrame != null) {
      theFrame.dispose();
    }
    theFrame = null;
  }

  public void removePredefinedSetup(PredefinedSetup setup) {
    setups.removeElement(setup);
    resetControls();
  }

  public void setTutorial(Tutorial tutorial) {
    this.tutorial = tutorial;
  }
}
