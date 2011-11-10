/*
 * $Id$
 *
 * Copyright (c) 2010 by Joel Uckelman
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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.SwingUtilities;
import javax.swing.Timer;

import java.awt.Font;
import java.util.Enumeration;
import java.util.Iterator;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.plaf.FontUIResource;

import java.util.HashMap;
import java.util.Map;

public class ProgressDialogTest {

//  protected static class GTKLaF extends com.sun.java.swing.plaf.gtk.GTKLookAndFeel {
  protected static class GTKLaF extends com.sun.java.swing.plaf.windows.WindowsLookAndFeel {
    @Override
    public UIDefaults getDefaults() {
      final float scale = 3f;

      final UIDefaults defaults = super.getDefaults();

      final Map<Object,Object> changes = new HashMap<Object,Object>();

      for (Map.Entry<Object,Object> e : defaults.entrySet()) {
        final Object key = e.getKey();
        final Object val = e.getValue();

        if (val instanceof FontUIResource) {
          final FontUIResource ores = (FontUIResource) val;
          final FontUIResource nres =
            new FontUIResource(ores.deriveFont(ores.getSize2D()*scale));
          changes.put(key, nres);
          System.out.println(key + " = " + nres);
        }
        else if (val instanceof Font) {
          final Font ofont = (Font) val;
          final Font nfont = ofont.deriveFont(ofont.getSize2D()*scale);
          changes.put(key, nfont);
          System.out.println(key + " = " + nfont);
        }
      }

      defaults.putAll(changes);

      return defaults;
    }
  }

  public static void main(String[] args) throws Exception {

/*
    for (Iterator i = UIManager.getLookAndFeelDefaults().keySet().iterator(); i.hasNext();) {
      Object key = i.next();

      if ((key instanceof String) && (((String) key).endsWith(".font"))) {
        Font font = UIManager.getFont(key);
        Font biggerFont = font.deriveFont(2.0f*font.getSize2D());
        // change ui default to bigger font
        UIManager.put(key,biggerFont);
      }
    }
*/

/*
    final UIDefaults defaults = UIManager.getLookAndFeelDefaults();
    final Enumeration keys = defaults.keys();
    while (keys.hasMoreElements()) {
      final Object key = keys.nextElement();

      if ((key instanceof String) && (((String) key).endsWith(".font"))) {
        final FontUIResource font = (FontUIResource) UIManager.get(key);
        defaults.put(key, new FontUIResource(font.getFontName(), font.getStyle(), 16));
          System.out.println('!');
        }
/*

      if (value != null && value instanceof Font) {
        UIManager.put(key, null);
        final Font font = UIManager.getFont(key);
        if (font != null) {
          float size = font.getSize2D();
          UIManager.put(key, new FontUIResource(font.deriveFont(size*96f/72f)));
        }
      }
*/
//   }

/*
    float scale=1.5f;

    //if you want to change LaF to a spesific LaF,such as "GTK"
    //put here a if statement like:
    //if(info.getClassName().contains("GTK"))
    //UIManager.setLookAndFeel(info.getClassName());

    UIDefaults defaults = UIManager.getDefaults();
    Enumeration newKeys = defaults.keys();

    while (newKeys.hasMoreElements()) {
        Object obj = newKeys.nextElement();
        Object current = UIManager.get(obj);
        if (current instanceof FontUIResource) {
            FontUIResource resource = (FontUIResource) current;
            defaults.put(obj, new FontUIResource(resource.deriveFont(resource.getSize2D()*scale)));
            // System.out.printf("%50s : %s\n", obj,  UIManager.get(obj));
        } else if (current instanceof Font) {
            Font resource = (Font) current;
            defaults.put(obj, resource.deriveFont(resource.getSize2D()*scale));
            // System.out.printf("%50s : %s\n", obj,  UIManager.get(obj));
        }
    }
*/

    UIManager.setLookAndFeel(new GTKLaF());


    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        final ProgressDialog pd = new ProgressDialog(
          null, "A Sisyphean Task", "Rolling a stone up the hill..."
        );

        pd.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            System.exit(0);
          }
        });

        final Timer timer = new Timer(100, new ActionListener() {
          int progress = 0;

          public void actionPerformed(ActionEvent e) {
            progress = (progress+1) % 100;
            pd.setProgress(progress);
          }
        });
        timer.start();

        pd.setDefaultCloseOperation(ProgressDialog.DISPOSE_ON_CLOSE);
        pd.setVisible(true);
      }
    });
  }
}
