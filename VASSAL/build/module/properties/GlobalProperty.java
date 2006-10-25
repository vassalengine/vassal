package VASSAL.build.module.properties;

import java.beans.PropertyChangeSupport;
import javax.swing.JToolBar;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.TemporaryToolBar;
import VASSAL.tools.ToolBarComponent;

/**
 * Adds a global property to a Map or Module
 * @author rkinney
 *
 */
public class GlobalProperty extends AbstractConfigurable implements ToolBarComponent, GameComponent, CommandEncoder, PropertySource {
  protected PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);

  public static final String NAME = "name";
  public static final String INITIAL_VALUE = "initialValue";
  public static final String DESCRIPTION = "description";
  public static final String NUMERIC = "isNumeric";
  public static final String MIN_VALUE = "min";
  public static final String MAX_VALUE = "max";
  public static final String WRAP = "wrap";
  

  protected static final String COMMAND_PREFIX = "GlobalProperty\t";
  protected TemporaryToolBar tempToolbar = new TemporaryToolBar();
  protected String propertyValue;
  protected String description;
  protected String initialValue;
  protected boolean numeric;
  protected String minValue;
  protected String maxValue;
  protected boolean wrap;
  protected VisibilityCondition numericVisibility;
  protected FormattedString format = new FormattedString();

  protected PropertySource propertySource;
  

  public GlobalProperty() {
    numericVisibility = new VisibilityCondition() {
      public boolean shouldBeVisible() {
        return isNumeric();
      }
    };
  }

  public String[] getAttributeDescriptions() {
    return new String[] {"Name", "Initial value", "Description","Is Numeric","Minimum value","Maximum value","Wrap around"};
  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, String.class, String.class, Boolean.class, String.class,String.class, Boolean.class};
  }

  public String[] getAttributeNames() {
    return new String[] {NAME, INITIAL_VALUE, DESCRIPTION,NUMERIC,MIN_VALUE,MAX_VALUE,WRAP};
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      String oldName = getConfigureName();
      propertyChangeSupport.firePropertyChange(oldName,propertyValue,null); // Clear the value under the old key
      setConfigureName((String) value);
      propertyChangeSupport.firePropertyChange(getConfigureName(),null,propertyValue);
    }
    else if (INITIAL_VALUE.equals(key)) {
      initialValue = (String) value;
      setPropertyValue(initialValue);
    }
    else if (DESCRIPTION.equals(key)) {
      description = (String) value;
    }
    else if (NUMERIC.equals(key)) {
      numeric = Boolean.TRUE.equals(value) || "true".equals(value);
    }
    else if (MIN_VALUE.equals(key)) {
      minValue = (String)value;
    }
    else if (MAX_VALUE.equals(key)) {
      maxValue = (String)value;
    }
    else if (WRAP.equals(key)) {
      wrap = Boolean.TRUE.equals(value) || "true".equals(value);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (INITIAL_VALUE.equals(key)) {
      return initialValue;
    }
    else if (DESCRIPTION.equals(key)) {
      return description;
    }
    else if (NUMERIC.equals(key)) {
      return String.valueOf(numeric);
    }
    else if (MIN_VALUE.equals(key)) {
      return String.valueOf(minValue);
    }
    else if (MAX_VALUE.equals(key)) {
      return String.valueOf(maxValue);
    }
    else if (WRAP.equals(key)) {
      return String.valueOf(wrap);
    }
    return null;
  }
  
  public VisibilityCondition getAttributeVisibility(String name) {
    if (MIN_VALUE.equals(name) || MAX_VALUE.equals(name) || WRAP.equals(name)) {
      return numericVisibility;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  public void removeFrom(Buildable parent) {
    propertyChangeSupport.firePropertyChange(getConfigureName(),propertyValue,null);
    propertyChangeSupport.removePropertyChangeListener(((GlobalPropertiesContainer)parent).getPropertyListener());
    GameModule.getGameModule().removeCommandEncoder(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GlobalProperties.htm");
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{ChangePropertyButton.class};
  }

  public void addTo(Buildable parent) {
    // Initialize property with current values
    propertyChangeSupport.addPropertyChangeListener(((GlobalPropertiesContainer)parent).getPropertyListener());
    propertyChangeSupport.firePropertyChange(getConfigureName(),null,propertyValue);
    tempToolbar.setDelegate((ToolBarComponent)parent);
    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    propertySource = (PropertySource) parent;
  }
  
  public String getPropertyValue() {
    return propertyValue;
  }
  
  public void setPropertyValue(String value) {
    String oldValue = propertyValue;
    propertyValue = value;
    propertyChangeSupport.firePropertyChange(getConfigureName(),oldValue,propertyValue);
  }

  public JToolBar getToolBar() {
    return tempToolbar.getToolBar();
  }
  
  public void setup(boolean gameStarting) {
    return;
  }

  public Command getRestoreCommand() {
    return new SetGlobalProperty(this, "", getPropertyValue());
  }

  public Command decode(String command) {
    Command comm = null;

    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(command, ';');
    String prefix = sd.nextToken("");
    if (prefix.equals(COMMAND_PREFIX)) {
      String marker = sd.nextToken("");
      if (marker.equals(getConfigureName())) {
        comm = new SetGlobalProperty(this, getPropertyValue(), sd.nextToken(""));
      }
    }
    return comm;
  }

  public String encode(Command c) {
    String s = null;
    if (c instanceof SetGlobalProperty ) {
      if (((SetGlobalProperty) c).getTargetName().equals(getConfigureName())) {
        SequenceEncoder se = new SequenceEncoder(COMMAND_PREFIX, ';');
        se.append(getConfigureName());
        se.append(((SetGlobalProperty) c).newValue);
        s = se.getValue();
      }
    }
    return s;
  }
  /**
   * Command to pass a new Global property value to other players or into the
   * logfile.
   */
  protected static class SetGlobalProperty extends Command {

    protected String newValue;
    protected String oldValue;
    protected GlobalProperty target;
    protected String targetName;

    protected SetGlobalProperty(GlobalProperty target, String oldV, String newV) {

      oldValue = oldV;
      newValue = newV;
      this.target = target;
    }

    public String getTargetName() {
      return target == null ? "" : target.getConfigureName();
    }
    
    protected void executeCommand() {
      target.setPropertyValue(newValue);
    }

    protected Command myUndoCommand() {
      return new SetGlobalProperty(target, newValue, oldValue);
    }
  }
  public int getMaxValue() {
    int max = 100;
    if (maxValue != null) {
      format.setFormat(maxValue);
      try {
        max = Integer.parseInt(format.getText(this));
      }
      catch (NumberFormatException e) {
      }
    }
    return max;
  }

  public int getMinValue() {
    int min = 0;
    if (minValue != null) {
      format.setFormat(minValue);
      try {
        min = Integer.parseInt(format.getText(this));
      }
      catch (NumberFormatException e) {
      }
    }
    return min;
  }

  public boolean isNumeric() {
    return numeric;
  }

  public boolean isWrap() {
    return wrap;
  }

  public String getDescription() {
    return description;
  }

  public Object getProperty(Object key) {
    return propertySource == null ? null : propertySource.getProperty(key);
  }

  public static String getConfigureTypeName() {
    return "Global Property";
  }


}
