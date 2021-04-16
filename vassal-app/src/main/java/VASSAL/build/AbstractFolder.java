package VASSAL.build;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.i18n.Resources;

public abstract class AbstractFolder extends AbstractConfigurable {
  public static final String NAME        = "name"; //NON-NLS
  public static final String DESCRIPTION = "desc"; //NON-NLS

  String description = "";

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString(Resources.NAME_LABEL),
      Resources.getString(Resources.DESCRIPTION)
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      String.class,
    };
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      NAME,
      DESCRIPTION
    };
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (DESCRIPTION.equals(key)) {
      return description;
    }
    else {
      return null;
    }
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (DESCRIPTION.equals(key)) {
      description = (String) value;
    }
  }


  @Override
  public void addTo(Buildable parent) {
    setAttributeTranslatable(NAME, false);
    setAttributeTranslatable(DESCRIPTION, false);
  }

  @Override
  public void removeFrom(Buildable parent) {
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Folder.html"); //NON-NLS
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.Folder.component_type"); //$NON-NLS-1$
  }
}
