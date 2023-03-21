/*
 * Copyright (c) 2023 by vassalengine.org, Brian Reynolds
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

import VASSAL.build.GameModule;
import VASSAL.tools.ErrorDialog;

import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.StyleConstants;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.StyleSheet;
import java.awt.Color;
import java.awt.Font;
import java.io.IOException;

/**
 * Extends JTextPane in a way that lets us put an HTML-compliant string that might access images from the vmod archive
 *
 * Essentially behaves like a JLabel on steroids, the key thing being that <img> links in the HTML can access images in the module.
 */
public class DataArchiveTextPane extends JTextPane {
  private static final long serialVersionUID = 1L;

  public DataArchiveTextPane(String text, String styleName, Color color, Font font) {

    if ((styleName == null) || styleName.isBlank()) {
      styleName = "label"; //NON-NLS // just something to default to
    }

    // Build a JTextPane to render HTML with images
    setContentType("text/html"); //NON-NLS
    final LabelerDataArchiveHTMLEditorKit kit = new LabelerDataArchiveHTMLEditorKit(GameModule.getGameModule().getDataArchive());
    setEditorKit(kit);

    final StyleSheet style = kit.getStyleSheet();
    style.addRule("." + styleName +
      " {color:" +                                                                        //NON-NLS
      String.format("#%02x%02x%02x", color.getRed(), color.getGreen(), color.getBlue()) + //NON-NLS
      "; font-family:" +                                                                  //NON-NLS
      font.getFamily() +
      "; font-size:" +                                                           //NON-NLS
      font.getSize() + "pt" +                                                    //NON-NLS
      "; " +                                                                     //NON-NLS
      ((font.isBold()) ? "font-weight:bold" : "") +                              //NON-NLS
      "; background-color: rgba(255, 0, 0, 0.5);" +                              //NON-NLS
      "}");                                                                      //NON-NLS
    style.addRule("." + styleName + "color {color:" + String.format("#%02x%02x%02x", color.getRed(), color.getGreen(), color.getBlue()) + "; }"); //NON-NLS

    setOpaque(false);
    // Transparent background
    StyleConstants.setBackground(style.getStyle("." + styleName), new Color(0, 0, 0, 0)); //NON-NLS

    // Remove original HTML markers if present and replace wrapping a styled div, inserting into document
    final String fixed = text.replaceFirst("<html>", "").replaceFirst("</html>", ""); //NON-NLS
    final HTMLDocument doc = (HTMLDocument) getDocument();
    try {
      kit.insertHTML(doc, doc.getLength(), "<html><div class=\"" + styleName + "\">" + fixed + "</div></html>", 0, 0, null); //NON-NLS
    }
    catch (BadLocationException | IOException ble) {
      ErrorDialog.bug(ble);
    }

    setSize(getPreferredSize());
  }
}
