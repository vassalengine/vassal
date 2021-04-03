/*
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

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.file.Files;

import javax.swing.JOptionPane;

import VASSAL.build.AbstractToolbarItem;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.counters.GamePiece;
import VASSAL.i18n.Resources;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.filechooser.FileChooser;

public class TextSaver extends AbstractToolbarItem {

  protected static final String BUTTON_TEXT = "buttonText"; //NON-NLS
  protected static final String ICON_NAME = "icon"; //NON-NLS

  // These two identical to AbstractToolbarItem and exist only for clirr purposes
  @Deprecated protected static final String HOTKEY = "hotkey"; //NON-NLS
  @Deprecated protected static final String TOOLTIP = "tooltip"; //NON-NLS

  protected Map map;

  /** @deprecated use launch from the superclass */
  @Deprecated(since = "2021-04-03", forRemoval = true)
  protected LaunchButton launch;

  public TextSaver() {
    setNameKey("");
    setButtonTextKey(BUTTON_TEXT);

    setLaunchButton(makeLaunchButton(
      Resources.getString("Editor.TextSaver.save_tooltip"),
      Resources.getString("Editor.TextSaver.save_text"),
      "",
      e -> apply()
    ));
    launch = getLaunchButton(); // for compatibility
  }

  @Deprecated(since = "2020-10-01", forRemoval = true)
  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(
        key,
        name,
        ((TextSaver) c).getLaunchButton().getAttributeValueString(ICON_NAME)
      );
    }
  }

  @Override
  public void addTo(Buildable b) {
    map = (Map) b;
    map.getToolBar().add(getLaunchButton());
  }

  @Override
  public void removeFrom(Buildable b) {
    map = (Map) b;
    map.getToolBar().remove(getLaunchButton());
    map.getToolBar().revalidate();
  }

  public void apply() {
    switch (JOptionPane.showConfirmDialog(GameModule.getGameModule().getPlayerWindow(), Resources.getString("Editor.TextSaver.by_opponents"), "", JOptionPane.YES_NO_OPTION)) {
    case JOptionPane.NO_OPTION:
      writeMapAsText();
      break;
    case JOptionPane.YES_OPTION:
      final String myId = GameModule.getUserId();
      GameModule.setUserId("yendoR117"); //NON-NLS
      writeMapAsText();
      GameModule.setUserId(myId);
      break;
    }
  }

  protected void writeMapAsText() {
    final FileChooser fc = GameModule.getGameModule().getFileChooser();
    if (fc.showSaveDialog(map.getView()) != FileChooser.APPROVE_OPTION) return;

    final File file =  fc.getSelectedFile();

    // Writing out a text file for the user to do whatever with. Use the native encoding.
    try (Writer bw = Files.newBufferedWriter(file.toPath(), Charset.defaultCharset());
         PrintWriter p = new PrintWriter(bw)) {
      for (final GamePiece gp : map.getPieces()) {
        final String s = gp.getName();
        if (s.length() > 0) {
          p.println(map.locationName(gp.getPosition()) + ": " + s);
        }
      }
    }
    catch (final IOException e) {
      WriteErrorDialog.error(e, file);
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.html", "TextCapture"); //NON-NLS
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.TextSaver.component_type"); //$NON-NLS-1$
  }

  /**
   * @return an array of Configurer objects representing
   * all possible classes of Buildable children of this Configurable object
   */
  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }
}
