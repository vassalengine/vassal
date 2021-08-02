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
package VASSAL.build.module.documentation;

import java.awt.Image;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.List;
import java.util.Collection;

import javax.swing.AbstractAction;
import javax.swing.Action;

import VASSAL.Info;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Documentation;
import VASSAL.build.module.ModuleExtension;
import VASSAL.i18n.Resources;
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.swing.AboutWindow;

/**
 * Places an entry in the <code>Help</code> menu.  Selecting the entry
 * displays a window with a stored image on it.  Good for a splash
 * screen or an "about" screen.
 */
public class AboutScreen extends AbstractConfigurable {
  protected ImageOp op;
  protected Image image;
  protected String title;
  protected String fileName;
  protected Action launch;

  public AboutScreen() {
    launch = new AbstractAction() {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        launch();
      }
    };
  }

  public AboutScreen(ImageOp op) {
    this();
    if (op == null) throw new IllegalArgumentException();
    this.op = op;
  }

  public void launch() {
    if (op == null) return;

    final GameModule g = GameModule.getGameModule();
    if (g == null) return;

    final StringBuilder sb = new StringBuilder("<html><center>"); //NON-NLS

    sb.append(
      Resources.getString("AboutScreen.module_version",  //$NON-NLS-1$
        g.getLocalizedGameName(), g.getGameVersion()));

    for (final ModuleExtension ext : g.getComponentsOf(ModuleExtension.class)) {
      sb.append("<br/>").append(//NON-NLS
        Resources.getString("AboutScreen.extension_version",  //$NON-NLS-1$
          ext.getName(), ext.getVersion()));
    }

    sb.append("<br/>") //NON-NLS
      .append(
        Resources.getString(
          "AboutScreen.vassal_version",  //$NON-NLS-1$
          Info.getVersion()
        )
      )
      .append("</center></html>"); //NON-NLS

    final AboutWindow w =
      new AboutWindow(g.getPlayerWindow(), op.getImage(), sb.toString());
    w.setVisible(true);
    w.toFront();
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.AboutScreen.component_type");
  }

  /**
   * The attributes of an AboutScreen are:
   *
   * <code>TITLE</code> the text of the menu entry in the Help menu
   * <code>FILE</code> the name of an image file in the @link
   * DataArchive.  The image is displayed when the menu item is
   * selected
   */
  @Override
  public String[] getAttributeNames() {
    return new String[]{
      TITLE,
      FILE
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString("Editor.menu_command"),
      Resources.getString("Editor.AboutScreen.image")
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      Image.class
    };
  }

  @Override
  public String getAttributeValueString(String key) {
    if (TITLE.equals(key)) {
      return title;
    }
    else if (FILE.equals(key)) {
      return fileName;
    }
    return null;
  }

  public static final String TITLE = "title"; //$NON-NLS-1$
  public static final String FILE = "fileName"; //$NON-NLS-1$

  @Override
  public void setAttribute(String key, Object val) {
    if (TITLE.equals(key)) {
      title = (String) val;

      // don't permit "About VASSAL"
      if (title != null && title.equals(Resources.getString("AboutScreen.about_vassal"))) {
        title = Resources.getString("Documentation.about_module");
      }

      setConfigureName(title);
      launch.putValue(Action.NAME, title);
    }
    else if (FILE.equals(key)) {
      if (val instanceof File) {
        val = ((File) val).getName();
      }
      fileName = (String) val;

      op = null;
      if (fileName != null) {
        fileName = fileName.trim();
        if (fileName.length() > 0) {
          op = Op.load(fileName);

          final Image img = op.getImage();
          if (img != null) {
            GameModule.getGameModule()
                      .getWizardSupport()
                      .setBackgroundImage(op.getImage());
          }
          else {
            op = null;
          }
        }
      }
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public void removeFrom(Buildable b) {
    MenuManager.getInstance().removeAction("Documentation.about_module");
  }

  /**
   * Expects to be added to a {@link Documentation}.  Adds an entry
   * to the <code>Help</code> menu
   */
  @Override
  public void addTo(Buildable b) {
    MenuManager.getInstance().addAction("Documentation.about_module", launch);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("HelpMenu.html", "AboutScreen"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return List.of(title);
  }

  @Override
  public void addLocalImageNames(Collection<String> s) {
    if (fileName != null) s.add(fileName);
  }
}
