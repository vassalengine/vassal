/*
 *
 * Copyright (c) 2026 by Christian Holm Christensen
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

import VASSAL.tools.ErrorDialog;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Window;
import java.awt.event.ItemListener;

import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;


/**
 * A Configurer for {@link javax.swing.UIManager.LookAndFeel} values
 */
public class LookAndFeelConfigurer extends Configurer {
  public static class Info extends LookAndFeelInfo {
    public Info(String name, String clsName) {
      super(name, clsName);
    }
    public Info(LookAndFeelInfo info) {
      this(info.getName(), info.getClassName());
    }
    @Override
    public String toString() {
      return getName();
    }
  }
        
  private JPanel panel;
  private JComboBox<Info> laf;

  public LookAndFeelConfigurer(String key, String name) {
    this(key, name,
         new Info(UIManager.getLookAndFeel().getName(),
                  UIManager.getLookAndFeel().getClass().getName()));
  }

  public LookAndFeelConfigurer(String key, String name, LookAndFeelInfo val) {
    super(key, name, new Info(val));
  }

  protected Info getInfo() {
    return (Info)value;
  }
  
  @Override
  public String getValueString() {
    return encode((Info) value);
  }

  @Override
  public void setValue(String s) {
    setValue(decode(s));
    updateLnF();
  }

  @Override
  public java.awt.Component getControls() {
    if (panel == null) {
      panel = new ConfigurerPanel(getName(), "[]rel[]", "[]rel[]"); // NON-NLS
      laf = new JComboBox<>();
      final LookAndFeelInfo[] known = UIManager.getInstalledLookAndFeels();
      final String sysClsName = UIManager.getSystemLookAndFeelClassName();
      Info sysInfo    = null;
      Info current    = null;
      for (final LookAndFeelInfo info : known) {
        final Info i = new Info(info);
        laf.addItem(i);
        if (value != null &&
            getInfo().getClassName().equals(i.getClassName())) {
          current = i;
        }
        if (i.getClassName().equals(sysClsName)) {
          sysInfo = i;
        }
      }
      laf.setSelectedItem(current == null ? sysInfo : current);
      laf.setMaximumSize(new Dimension(laf.getMaximumSize().width,
                                       laf.getPreferredSize().height));
      panel.add(laf);

      final ItemListener l = evt -> setValue(laf.getSelectedItem());
      laf.addItemListener(l);
    }
    return panel;
  }

  public static Info decode(String s) {
    final int i = s.indexOf(',');
    return new Info(s.substring(0, i), s.substring(i + 1));
  }

  public static String encode(Info i) {
    return i.getName() + "," + i.getClassName();
  }

  protected void updateLnF() {
    // Check for change 
    if (getInfo().getClassName().equals(UIManager.getLookAndFeel().getClass().getName()))
      return;

    try {
      UIManager.setLookAndFeel(getInfo().getClassName());
      for (final Frame frame : Frame.getFrames()) {
        if (frame == null)
          continue;
        SwingUtilities.updateComponentTreeUI(frame);

        for (final Window window : frame.getOwnedWindows()) 
          SwingUtilities.updateComponentTreeUI(window);
      }
    }
    catch (Exception e) {
      ErrorDialog.bug(e);
    }
  }
  @Override
  public void fireUpdate() {
    super.fireUpdate();
    updateLnF();
  }
  
  @Override
  public void setLabelVisible(boolean visible) {
    if (panel instanceof ConfigurerPanel) {
      ((ConfigurerPanel) panel).setLabelVisibility(visible);
    }
  }

  public static void main(String[] args) {
    final JFrame frame = new JFrame();
    frame.setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));
    final LookAndFeelConfigurer config = new LookAndFeelConfigurer("a", "LaF: "); //NON-NLS
    frame.add(config.getControls());
    config.addPropertyChangeListener(evt -> {
      final Info info = (Info)evt.getNewValue();
      final LookAndFeelConfigurer fconfig
        = new LookAndFeelConfigurer(null, null, info);
      fconfig.setValue(fconfig.getValueString());
      fconfig.fireUpdate();
      frame.pack();
    });
    frame.pack();
    frame.setVisible(true);
  }
}
