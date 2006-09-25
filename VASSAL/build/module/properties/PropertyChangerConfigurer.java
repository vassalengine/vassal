package VASSAL.build.module.properties;

import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.Map;

import javax.swing.BoxLayout;
import javax.swing.JPanel;

import VASSAL.build.module.gamepieceimage.StringEnumConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.SequenceEncoder;

public class PropertyChangerConfigurer extends Configurer {
  protected static final String PLAIN_TYPE = "Set value directly";
  protected static final String INCREMENT_TYPE = "Increment numeric value";
  protected static final String PROMPT_TYPE = "Prompt user";
  protected static final String SELECT_TYPE = "Prompt user to select from list";
  protected static final char PLAIN_CODE = 'P';
  protected static final char PROMPT_CODE = 'R';
  protected static final char ENUM_CODE = 'E';
  protected static final char INCR_CODE = 'I';
  protected static final Map typeToCode = new HashMap();
  protected static final Map typeToDescription = new HashMap();
  protected static final Map descriptionToCode = new HashMap();
  static {
    typeToCode.put(PropertySetter.class, new Character(PLAIN_CODE));
    typeToCode.put(PropertyPrompt.class, new Character(PROMPT_CODE));
    typeToCode.put(NumericPropertyPrompt.class, new Character(PROMPT_CODE));
    typeToCode.put(IncrementProperty.class, new Character(INCR_CODE));
    typeToCode.put(EnumeratedPropertyPrompt.class, new Character(ENUM_CODE));

    typeToDescription.put(PropertySetter.class, PLAIN_TYPE);
    typeToDescription.put(PropertyPrompt.class, PROMPT_TYPE);
    typeToDescription.put(NumericPropertyPrompt.class, PROMPT_TYPE);
    typeToDescription.put(IncrementProperty.class, INCREMENT_TYPE);
    typeToDescription.put(EnumeratedPropertyPrompt.class, SELECT_TYPE);

    descriptionToCode.put(PLAIN_TYPE, new Character(PLAIN_CODE));
    descriptionToCode.put(INCREMENT_TYPE, new Character(INCR_CODE));
    descriptionToCode.put(PROMPT_TYPE, new Character(PROMPT_CODE));
    descriptionToCode.put(SELECT_TYPE, new Character(ENUM_CODE));
  }

  protected Constraints constraints;
  protected JPanel controls;
  protected StringEnumConfigurer typeConfig;
  protected StringConfigurer valueConfig;
  protected StringConfigurer promptConfig;
  protected StringConfigurer incrConfig;
  protected StringArrayConfigurer validValuesConfig;

  public PropertyChangerConfigurer(String key, String name, Constraints constraints) {
    super(key, name);
    this.constraints = constraints;
    setValue(new PropertySetter("", null));
  }

  public Component getControls() {
    if (controls == null) {
      PropertyChangeListener l = new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          updateValue();
          updateControls();
        }
      };
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls,BoxLayout.X_AXIS));
      typeConfig = new StringEnumConfigurer(null,"Type:  ",new String[]{PLAIN_TYPE,INCREMENT_TYPE,PROMPT_TYPE,SELECT_TYPE});
      typeConfig.addPropertyChangeListener(l);
      valueConfig = new StringConfigurer(null,"New Value:  ");
      valueConfig.addPropertyChangeListener(l);
      promptConfig = new StringConfigurer(null,"Prompt:  ");
      promptConfig.addPropertyChangeListener(l);
      incrConfig = new StringConfigurer(null,"Increment by:  ");
      incrConfig.addPropertyChangeListener(l);
      validValuesConfig = new StringArrayConfigurer(null,"Valid Values");
      validValuesConfig.addPropertyChangeListener(l);
      controls.add(typeConfig.getControls());
      controls.add(valueConfig.getControls());
      controls.add(promptConfig.getControls());
      controls.add(incrConfig.getControls());
      controls.add(validValuesConfig.getControls());
      updateControls();
    }
    return controls;
  }

  protected void updateControls() {
    PropertyChanger pc = getPropertyChanger();
    typeConfig.setValue(typeToDescription.get(pc.getClass()));
    if (pc instanceof PropertySetter) {
      valueConfig.setValue(((PropertySetter)pc).getRawValue());
      valueConfig.getControls().setVisible(true);
    }
    else {
      valueConfig.getControls().setVisible(false);
    }
    if (pc instanceof IncrementProperty) {
      incrConfig.setValue(String.valueOf(((IncrementProperty) pc).getIncrement()));
      incrConfig.getControls().setVisible(true);
    }
    else {
      incrConfig.getControls().setVisible(false);
    }
    if (pc instanceof PropertyPrompt) {
      promptConfig.setValue(((PropertyPrompt) pc).getPrompt());
      promptConfig.getControls().setVisible(true);
    }
    else {
      promptConfig.getControls().setVisible(false);
    }
    if (pc instanceof EnumeratedPropertyPrompt) {
      validValuesConfig.setValue(((EnumeratedPropertyPrompt) pc).getValidValues());
      validValuesConfig.getControls().setVisible(true);
    }
    else {
      validValuesConfig.getControls().setVisible(false);
    }
  }
  
  protected void updateValue() {
    PropertyChanger p;
    switch (((Character)descriptionToCode.get(typeConfig.getValueString())).charValue()) {
    case PROMPT_CODE:
      p = new PropertyPrompt(constraints, promptConfig.getValueString());
      break;
    case INCR_CODE:
      p = new IncrementProperty(incrConfig.getValueString(), constraints);
      break;
    case ENUM_CODE:
      p = new EnumeratedPropertyPrompt(constraints, promptConfig.getValueString(), validValuesConfig.getStringArray());
      break;
    case PLAIN_CODE:
    default:
      p = new PropertySetter(valueConfig.getValueString(), constraints);
    }
    setValue(p);
  }

  public String getValueString() {
    PropertyChanger propChanger = getPropertyChanger();
    SequenceEncoder se = new SequenceEncoder(',');
    if (propChanger != null) {
      switch (((Character) typeToCode.get(propChanger.getClass())).charValue()) {
      case PROMPT_CODE:
        se.append(PROMPT_CODE).append(((PropertyPrompt) propChanger).getPrompt());
        break;
      case INCR_CODE:
        se.append(INCR_CODE).append(((IncrementProperty) propChanger).getIncrement());
        break;
      case ENUM_CODE:
        se.append(ENUM_CODE).append(((PropertyPrompt) propChanger).getPrompt()).append(((EnumeratedPropertyPrompt) propChanger).getValidValues());
        break;
      case PLAIN_CODE:
        se.append(PLAIN_CODE).append(((PropertySetter)propChanger).getRawValue());
      }
    }
    return se.getValue();
  }

  public PropertyChanger getPropertyChanger() {
    return (PropertyChanger) getValue();
  }

  public void setValue(String s) {
    PropertyChanger p;
    if (s == null || s.length() == 0) {
      s = Character.toString(PLAIN_CODE);
    }
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
    switch (sd.nextChar(PLAIN_CODE)) {
    case PROMPT_CODE:
      p = new PropertyPrompt(constraints, sd.nextToken("Enter new value"));
      break;
    case INCR_CODE:
      p = new IncrementProperty(sd.nextToken("1"), constraints);
      break;
    case ENUM_CODE:
      p = new EnumeratedPropertyPrompt(constraints, sd.nextToken("Select new value"), sd.nextStringArray(0));
      break;
    case PLAIN_CODE:
    default:
      p = new PropertySetter(sd.nextToken("new value"), constraints);
    }
    setValue(p);
  }

  public static interface Constraints extends PropertyPrompt.Constraints, IncrementProperty.Constraints, PropertySource {
  }

}
