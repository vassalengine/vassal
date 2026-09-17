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

import java.lang.reflect.InvocationTargetException;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.LookAndFeel;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;

public class LookAndFeelClassConfigurer  extends ListConfigurer {
  public LookAndFeelClassConfigurer(String key, String name) {
    super(key, name);
  }

  /**
   * Get configurer for a new entry in list, or recreate one.
   */
  @Override
  protected Configurer buildChildConfigurer() {
    return new StringConfigurer("", "");
  }
  
  /**
   * When a change is detected
   */
  @Override
  public void fireUpdate() {
    registerLnFClasses();
    super.fireUpdate();
  }

  @Override
  public void setValue(String s) {
    super.setValue(s);
    registerLnFClasses();
  }

  protected void registerLnFClasses() {
    for (final Object o : getListValue()) {
      final String className = (String)o;
      registerLnFClass(className);
    }
  }
  
  protected void registerLnFClass(String className) {
    System.out.println("Trying to register \"" + className + "\" for look'n'feel");
    try {
      final ClassLoader loader = ClassLoader.getSystemClassLoader();
      final Class<?>    cls    = loader.loadClass(className); // Class.forName(className, true, loader);
          
      if (LookAndFeel.class.isAssignableFrom(cls)) {
        final LookAndFeel lnf = (LookAndFeel)cls.getDeclaredConstructor().newInstance();
        UIManager.installLookAndFeel(lnf.getName(), cls.getName());
      }
      else if (LookAndFeelInfo.class.isAssignableFrom(cls)) {
        final LookAndFeelInfo lnf = (LookAndFeelInfo)cls.getDeclaredConstructor().newInstance();
        UIManager.installLookAndFeel(lnf);
      }
    }
    catch (ClassNotFoundException    |
           InstantiationException    |
           InvocationTargetException |
           NoSuchMethodException     |
           IllegalAccessException      e) {
      ErrorDialog.show(e, "Error.failed_to_load_lnf_class", className);
    }
  }

  /**
   * Test of this configurer
   */
  public static void main(String[] args) {
    final JFrame frame = new JFrame();
    frame.setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));
    
    final LookAndFeelClassConfigurer lafjar = new LookAndFeelClassConfigurer("a", "LaF classes: "); //NON-NLS
    final LookAndFeelConfigurer laf = new LookAndFeelConfigurer("b", "LaF: ");
    
    frame.add(lafjar.getControls());
    lafjar.addPropertyChangeListener(evt -> {
      final LookAndFeelClassConfigurer config
        = new LookAndFeelClassConfigurer("", "Testing");
      config.setValue(lafjar.getValueString());
      config.fireUpdate();
      laf.cacheKnownLookAndFeels();
      laf.updateAvailableLookAndFeels();
      frame.pack();
    });

    frame.add(laf.getControls());
    laf.addPropertyChangeListener(evt -> {
      final LookAndFeelConfigurer config
        = new LookAndFeelConfigurer(null, "Testing again");
      config.setValue(laf.getValueString());
      config.fireUpdate();
      frame.pack();
    });
    
    frame.pack();
    frame.setVisible(true);
  }
}

