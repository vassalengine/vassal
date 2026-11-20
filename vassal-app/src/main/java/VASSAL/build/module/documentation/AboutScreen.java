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

import VASSAL.Info;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Documentation;
import VASSAL.build.module.ModuleExtension;
import VASSAL.i18n.Resources;
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.Op;
import VASSAL.launch.AboutVASSALAction;
import VASSAL.tools.swing.AboutWindow;

import javax.swing.AbstractAction;
import javax.swing.Action;
import java.awt.Image;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.Collection;
import java.util.List;

/**
 * Legacy Help menu component, kept so that modules made before 3.8.3 still
 * load and players on 3.8.0 to 3.8.2 still see their About image. The About
 * image is now set on the top-level Module ({@link GameModule#ABOUT_IMAGE}).
 * This component can no longer be added, and is hidden in the Editor: its
 * image and title are always written from the Module's About screen image
 * and name. To be removed in 3.9.
 *
 * <p>VASSAL shows one About entry, "About &lt;module name&gt;", following
 * platform conventions: in the application menu on macOS, and as the last
 * entry of the Help menu on Windows and Linux (see {@link AboutVASSALAction}).
 * It shows the module's About image (see {@link #getModuleAboutImage}) or
 * the VASSAL splash above the module, extension and VASSAL versions. This
 * component's image is used only if the Module has none; its Menu command
 * is no longer used.</p>
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

    show(g.getPlayerWindow(), op.getImage(), getAboutText(g));
  }

  /**
   * @return the image for the module's About screen: the module's own About
   * image (set on the top-level Module component) if any; otherwise, for
   * modules made before 3.8.3, the first legacy AboutScreen (including
   * extensions) that has an image; otherwise null
   */
  public static BufferedImage getModuleAboutImage(GameModule g) {
    if (g == null) return null;
    final ImageOp moduleOp = g.getAboutImageOp();
    if (moduleOp != null) {
      return moduleOp.getImage();
    }
    for (final AboutScreen a : g.getAllDescendantComponentsOf(AboutScreen.class)) {
      if (a.op != null) {
        return a.op.getImage();
      }
    }
    return null;
  }

  /**
   * @return the menu text for the module's About entry: "About &lt;module name&gt;"
   */
  public static String getModuleAboutMenuText(GameModule g) {
    return Resources.getString("AboutScreen.about_module_name", g.getLocalizedGameName()); //$NON-NLS-1$
  }

  /**
   * @return the HTML text shown below the About image: module version and
   * other info, loaded extensions, and the VASSAL version
   */
  public static String getAboutText(GameModule g) {
    final StringBuilder sb = new StringBuilder("<html><center>"); //NON-NLS

    sb.append(
      Resources.getString("AboutScreen.module_version",  //$NON-NLS-1$
        g.getLocalizedGameName(), g.getGameVersion()));

    final String o1 = g.getModuleOther1();
    if ((o1 != null) && !o1.isEmpty()) {
      sb.append(" - ")
        .append(o1);
    }
    final String o2 = g.getModuleOther2();
    if ((o2 != null) && !o2.isEmpty()) {
      sb.append(" - ")
        .append(o2);
    }

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

    return sb.toString();
  }

  /**
   * Shows the module's About screen: the module's AboutScreen image, or the
   * VASSAL splash if the module has none, above {@link #getAboutText}.
   */
  public static void showModuleAbout(Window owner, GameModule g) {
    final BufferedImage moduleImg = getModuleAboutImage(g);
    final BufferedImage img = moduleImg != null ? moduleImg : AboutVASSALAction.getSplashImage();
    if (img == null) return;
    show(owner, img, getAboutText(g));
  }

  private static void show(Window owner, BufferedImage img, String text) {
    final AboutWindow w = new AboutWindow(owner, img, text);
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
  }

  /**
   * Expects to be added to a {@link Documentation}. Does not add a menu
   * entry of its own; it only supplies a fallback image for the single
   * About entry. See {@link AboutVASSALAction}.
   */
  @Override
  public void addTo(Buildable b) {
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
