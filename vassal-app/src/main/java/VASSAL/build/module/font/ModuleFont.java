/*
 *
 * Copyright (c) 2023 by The VASSAL Development Team
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
package VASSAL.build.module.font;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AbstractFolder;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.ConfigurerPanel;
import VASSAL.configure.FontFileConfigurer;
import VASSAL.i18n.Resources;

import javax.swing.JTextField;
import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.io.File;

/**
 * A Component that allows a True or Open Type font to be loaded into a module.
 */
public class ModuleFont extends AbstractConfigurable {

  public static final String NAME = "name";
  public static final String FILE_NAME = "fileName";
  public static final String STATUS = "status";
  public static final String FONT_FAMILY = "fontFamily";
  public static final String FONT_NAME = "fontName";

  protected String fileName;
  protected VassalFont font;

  @Override
  public String[] getAttributeNames() {
    return new String[] {FILE_NAME, FONT_FAMILY, FONT_NAME, STATUS, NAME};
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (FILE_NAME.equals(key)) {
      if (value instanceof File) {
        final File file = (File) value;
        if (file.isFile()) {
          // Real file means a new file is being loaded
          font = new VassalFont(file);
          fileName =  ((File) value).getName();
          fontAdded();
          return;
        }
        else {
          // Not a real file, font is already in Vassal somewhere
          value = ((File) value).getName();
        }
      }

      // Ignore nulls and do not reload the same file
      // AutoConfigurer has a habit of repeatedly calling setAttribute on Configurers
      final String fname = (String) value;
      if (fname != null && !fname.equals(fileName)) {
        fileName = fname;
        font = new VassalFont(fileName);
        fontAdded();
      }
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (FILE_NAME.equals(key)) {
      return fileName;
    }
    else if (FONT_FAMILY.equals(key)) {
      return font == null ? "" : font.getFontFamily();
    }
    else if (FONT_NAME.equals(key)) {
      return font == null ? "" : font.getFontName();
    }
    else if (STATUS.equals(key)) {
      return font == null ? "Loading" : font.getStatus();
    }
    return null;
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
      Resources.getString("Editor.ModuleFont.font_file_name"),
      Resources.getString("Editor.ModuleFont.font_family"),
      Resources.getString("Editor.ModuleFont.font_name"),
      Resources.getString("Editor.ModuleFont.status")
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class[] { FontConfig.class, LabelConfig.class, LabelConfig.class, LabelConfig.class };
  }

  @Override
  public void addTo(Buildable parent) {
    if (parent instanceof AbstractFolder) {
      parent = ((AbstractFolder) parent).getNonFolderAncestor();
    }
  }

  public VassalFont getFont() {
    return font;
  }

  private void fontAdded() {
    if (font == null || font.getFontFamily() == null) {
      setConfigureName(fileName);
    }
    else {
      setConfigureName(font.getFontFamily() + " - " + font.getFontName());
      final FontOrganizer fo = GameModule.getGameModule().getFontOrganizer();
      if (fo != null) {
        fo.setDirty(true);
      }
    }
  }

  @Override
  public void removeFrom(Buildable parent) {

  }

  @Override
  public HelpFile getHelpFile() {
    return null;
  }

  @Override
  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.ModuleFont.component_type"); //$NON-NLS-1$
  }

  @Override
  public String getComponentTypeName() {
    return getConfigureTypeName();
  }

  @Override
  public String getComponentName() {
    return getConfigureTypeName();
  }

  public static class FontConfig implements ConfigurerFactory {

    private ModuleFont moduleFont;

    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      moduleFont = (ModuleFont) c;
      final Configurer config = new FontFileConfigurer(key, name, GameModule.getGameModule().getArchiveWriter());
      config.addPropertyChangeListener((e) -> update(e, config));
      return config;
    }

    private void update(PropertyChangeEvent e, Configurer config) {
      config.setFrozen(true);
      if (e.getNewValue() instanceof File) {
        moduleFont.fileName = ((File) e.getNewValue()).getName();
      }
      config.refreshParent();
      config.setFrozen(false);
    }
  }

  public static class LabelConfig implements ConfigurerFactory {

    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new labelConfigurer(key, name);
    }
  }

  private static class labelConfigurer extends Configurer {

    final JTextField label = new JTextField();

    public labelConfigurer(String key, String name) {
      super(key, name);
    }

    @Override
    public String getValueString() {
      return value.toString();
    }

    @Override
    public void setValue(String s) {
      if (!noUpdate && label != null) {
        label.setText(s);
      }
      setValue((Object) s);
    }

    @Override
    public Component getControls() {
      final ConfigurerPanel p = new ConfigurerPanel(getName(), "[fill,grow]0[0]", "[][fill,grow][]"); // NON-NLS
      label.setText(getValueString());
      label.setEditable(false);
      p.add(label, "grow");
      return p;
    }
  }
}
