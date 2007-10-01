/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.build.module.map;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.RenderedImage;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import javax.imageio.ImageIO;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JWindow;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.border.BevelBorder;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.tools.BackgroundTask;
import VASSAL.tools.FileChooser;
import VASSAL.tools.LaunchButton;

/**
 * This allows the user to capture a snapshot of the entire map into a PNG file
 */
public class ImageSaver extends AbstractConfigurable {
  protected LaunchButton launch;
  protected Map map;
  protected boolean promptToSplit = false;
  protected static final String DEFAULT_ICON = "/images/camera.gif";

  public ImageSaver() {
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        writeMapAsImage();
      }
    };
    launch = new LaunchButton(null, TOOLTIP, BUTTON_TEXT, HOTKEY, ICON_NAME, al);
    // Set defaults for backward compatibility
    launch.setAttribute(TOOLTIP, "Save Map as PNG file");
    launch.setAttribute(BUTTON_TEXT, "");
    launch.setAttribute(ICON_NAME, DEFAULT_ICON);
  }

  public ImageSaver(Map m) {
    map = m;
  }

  /**
   * Expects to be added to a {@link Map}. Adds a button to the map window
   * toolbar that initiates the capture
   */
  public void addTo(Buildable b) {
    map = (Map) b;
    map.getToolBar().add(launch);
  }

  public void removeFrom(Buildable b) {
    map = (Map) b;
    map.getToolBar().remove(launch);
    map.getToolBar().revalidate();
  }

  protected static final String HOTKEY = "hotkey";
  protected static final String BUTTON_TEXT = "buttonText";
  protected static final String TOOLTIP = "tooltip";
  protected static final String ICON_NAME = "icon";

  public String[] getAttributeNames() {
    return new String[] {BUTTON_TEXT, TOOLTIP, ICON_NAME, HOTKEY};
  }

  public String[] getAttributeDescriptions() {
    return new String[] {"Button Text:  ", "Tooltip Text:  ", "Button icon:  ", "Hotkey:  "};
  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, String.class, IconConfig.class, KeyStroke.class};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, DEFAULT_ICON);
    }
  }

  public void setAttribute(String key, Object value) {
    launch.setAttribute(key, value);
  }

  public String getAttributeValueString(String key) {
    return launch.getAttributeValueString(key);
  }

  /**
   * Outputs a snapshot of the Map to a PNG file. Displays a file dialog to
   * prompt the user for the file
   */
  public void writeMapAsImage() {
    int sections = 1;
    if (promptToSplit) {
      String s = JOptionPane.showInputDialog("Divide map into how many sections?\n(Using more sections requires less memory)");
      if (s == null) {
        return;
      }
      try {
        sections = Integer.parseInt(s);
      }
      catch (NumberFormatException ex) {
      }
    }

    FileChooser fc = GameModule.getGameModule().getFileChooser();
    fc.setSelectedFile(
      new File(fc.getCurrentDirectory(),
               GameModule.getGameModule().getGameName() + "Map.png"));
    
    if (fc.showSaveDialog(map.getView()) == FileChooser.APPROVE_OPTION) {
      final int sectionCount = sections;
      final String fileName = fc.getSelectedFile().getPath();
      final JWindow w = new JWindow((Frame) SwingUtilities.getAncestorOfClass(Frame.class, map.getView()));
      final JLabel text = new JLabel("Saving Map Image ...");
      text.setFont(new Font("Dialog", Font.PLAIN, 48));
      text.setBackground(Color.white);
      text.setForeground(Color.black);
      text.setBorder(new BevelBorder(BevelBorder.RAISED, Color.lightGray, Color.darkGray));
      w.getContentPane().setBackground(Color.white);
      w.add(text);
      w.pack();
      Rectangle r = map.getView().getTopLevelAncestor().getBounds();
      w.setLocation(r.x + r.width / 2 - w.getSize().width / 2, r.y + r.height / 2 - w.getSize().height / 2);
      BackgroundTask task = new BackgroundTask() {
        private Throwable error;

        public void doFirst() {
          try {
            FileOutputStream[] p = new FileOutputStream[sectionCount];
            for (int i = 0; i < sectionCount; ++i) {
              String sectionName = fileName;
              if (sectionCount > 1) {
                if (fileName.lastIndexOf(".") >= 0) {
                  sectionName = fileName.substring(0, fileName.lastIndexOf(".")) + (i + 1) + fileName.substring(fileName.lastIndexOf("."));
                }
                else {
                  sectionName = fileName + (i + 1);
                }
              }
              p[i] = new FileOutputStream(sectionName);
            }
            writeImage(p);
          }
          catch (Throwable err) {
            error = err;
          }
        }

        public void doLater() {
          if (error instanceof OutOfMemoryError) {
            JOptionPane.showMessageDialog(map.getView().getTopLevelAncestor(), "Insufficient memory\n" + "Zooming out will reduce memory requirements\n"
                + "Otherwise, try again and you will be prompted to split the map\n" + "into a number of sections", "Error saving map image",
                JOptionPane.ERROR_MESSAGE);
            promptToSplit = true;
          }
          else if (error != null) {
            error.printStackTrace();
            String msg = error.getMessage();
            if (msg == null || msg.length() == 0) {
              msg = error.getClass().getName();
              msg = msg.substring(msg.lastIndexOf(".") + 1);
            }
            JOptionPane.showMessageDialog(map.getView().getTopLevelAncestor(), msg, "Error saving map image", JOptionPane.ERROR_MESSAGE);
          }
          w.dispose();
        }
      };
      Timer t = new Timer(1000, new ActionListener() {
        boolean toggle;

        public void actionPerformed(ActionEvent e) {
          if (toggle) {
            text.setText("Saving Map Image");
          }
          else {
            text.setText("Saving Map Image ...");
          }
          toggle = !toggle;
        }
      });
      w.setVisible(true);
      task.start();
      t.start();
    }
  }

  /**
   * Write a PNG-encoded snapshot of the map to the given OutputStreams,
   * dividing the map into vertical sections, one per stream
   */
  public void writeImage(OutputStream[] out) throws IOException {
    Dimension buffer = map.getEdgeBuffer();
    int totalWidth = (int) ((map.mapSize().width - 2 * buffer.width) * map.getZoom());
    int totalHeight = (int) ((map.mapSize().height - 2 * buffer.height) * map.getZoom());
    for (int i = 0; i < out.length; ++i) {
      int height = totalHeight / out.length;
      if (i == out.length - 1) {
        height = totalHeight - height * (out.length - 1);
      }

      Image output = map.getView().createImage(totalWidth, height);
      Graphics gg = output.getGraphics();
      map.paint(gg, -(int) (map.getZoom() * buffer.width), -(int) (map.getZoom() * buffer.height) + height * i);
      try {
        MediaTracker t = new MediaTracker(map.getView());
        t.addImage(output, 0);
        t.waitForID(0);
      }
      catch (Exception e) {
        e.printStackTrace();
      }
      writePNG(output, out[i]);
      out[i].close();
    }
  }

  private void writePNG(Image output, OutputStream out) throws IOException {
    if (output instanceof RenderedImage) {
      ImageIO.write((RenderedImage) output, "png", out);
    }
    else {
      throw new IOException("Bad image type");
    }
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.htm", "ImageCapture");
  }

  public static String getConfigureTypeName() {
    return "Image Capture Tool";
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }
}
